# Stock SMART national Dynamic Linear Model analysis
#
# Uses all eligible stock-assessment IDs in newest_assessment_data_with_refpts.
#
# Ratio mapping:
#   BvB = Bratio = B / biomass reference point
#   UvU = Fratio = F / fishing-mortality reference point
#
# Required input:
# newest_assessment_data_with_refpts must be in the R environment and contain:
# stock_id, assessment_id, stock_name, year, metric, Bratio, Fratio.

library(tidyverse)
library(TMB)

# ------------------------------------------------------------
# 1. Project paths and TMB compilation/loading
# ------------------------------------------------------------

project_dir <- "fish_stock_figure-main"
cpp_dir <- file.path(project_dir, "cpp")
cpp_file <- file.path(cpp_dir, "dlm_ar1.cpp")

if (!dir.exists(project_dir)) {
  stop("Project directory does not exist:\n", project_dir)
}

if (!dir.exists(cpp_dir)) {
  stop("C++ directory does not exist:\n", cpp_dir)
}

if (!file.exists(cpp_file)) {
  stop(
    "Cannot find the TMB source file:\n", cpp_file,
    "\n\nC++ files found in the project:\n",
    paste(
      list.files(
        project_dir,
        pattern = "\\.cpp$",
        recursive = TRUE,
        full.names = TRUE
      ),
      collapse = "\n"
    )
  )
}

# Compile from the cpp directory so the DLL is written beside dlm_ar1.cpp.
original_wd <- getwd()
setwd(cpp_dir)

tryCatch(
  {
    TMB::compile("dlm_ar1.cpp")
  },
  finally = {
    setwd(original_wd)
  }
)

# Find the resulting platform-specific shared library.
dll_candidates <- unique(c(
  list.files(
    cpp_dir,
    pattern = "^dlm_ar1\\.(dll|so|dylib)$",
    full.names = TRUE,
    ignore.case = TRUE
  ),
  list.files(
    project_dir,
    pattern = "^dlm_ar1\\.(dll|so|dylib)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
))

if (length(dll_candidates) == 0) {
  stop(
    "No dlm_ar1 shared library was found after compilation.\n\n",
    "Files matching 'dlm_ar1' in the project:\n",
    paste(
      list.files(
        project_dir,
        recursive = TRUE,
        full.names = TRUE,
        pattern = "dlm_ar1"
      ),
      collapse = "\n"
    )
  )
}

dll_file <- dll_candidates[1]
message("Using compiled TMB library: ", dll_file)

if (!"dlm_ar1" %in% names(getLoadedDLLs())) {
  dyn.load(dll_file)
}

# ------------------------------------------------------------
# 2. Create a stock-assessment-year input table
# ------------------------------------------------------------
# One row per stock_id + assessment_id + year.
#
# A stock ID may appear in more than one assessment version. Including
# assessment_id in stockid preserves assessment-specific time series.

if (!exists("newest_assessment_data_with_refpts")) {
  stop(
    "newest_assessment_data_with_refpts does not exist.\n",
    "Run the Stock SMART assessment/reference-point preparation script first."
  )
}

fish_stock_input <- newest_assessment_data_with_refpts |>
  transmute(
    stock_id,
    stock_name,
    common_name,
    scientific_name,
    stock_area,
    jurisdiction,
    fmp,
    assessment_id,
    assessment_year,
    assessment_type,
    year = as.numeric(year),
    
    # Ratio mappings for the DLM workflow:
    BvB = if_else(metric == "Abundance", Bratio, NA_real_),
    UvU = if_else(metric == "Fmort", Fratio, NA_real_)
  ) |>
  filter(!is.na(year)) |>
  group_by(
    stock_id,
    stock_name,
    common_name,
    scientific_name,
    stock_area,
    jurisdiction,
    fmp,
    assessment_id,
    assessment_year,
    assessment_type,
    year
  ) |>
  summarise(
    BvB = first(BvB[!is.na(BvB)], default = NA_real_),
    UvU = first(UvU[!is.na(UvU)], default = NA_real_),
    .groups = "drop"
  ) |>
  arrange(stock_id, assessment_id, year)

# Inspect input coverage before modeling.
print(
  fish_stock_input |>
    summarise(
      stock_assessments = n_distinct(paste(stock_id, assessment_id)),
      stock_ids = n_distinct(stock_id),
      first_year = min(year, na.rm = TRUE),
      last_year = max(year, na.rm = TRUE),
      rows_with_bvb = sum(!is.na(BvB)),
      rows_with_uvu = sum(!is.na(UvU))
    )
)

# ------------------------------------------------------------
# 3. National DLM function: no regional elements
# ------------------------------------------------------------

run_national_fisheries_analysis <- function(
    fish_stock_input,
    variables_to_analyze = c("BvB", "UvU"),
    start_year = 1980
) {
  
  required_columns <- c(
    "stock_id",
    "assessment_id",
    "stock_name",
    "year",
    variables_to_analyze
  )
  
  missing_columns <- setdiff(required_columns, names(fish_stock_input))
  
  if (length(missing_columns) > 0) {
    stop(
      "fish_stock_input is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  # Convert BvB and UvU columns to a single long-format analysis table.
  analysis_data <- fish_stock_input |>
    select(
      stock_id,
      stock_name,
      assessment_id,
      assessment_year,
      year,
      all_of(variables_to_analyze)
    ) |>
    pivot_longer(
      cols = all_of(variables_to_analyze),
      names_to = "variable",
      values_to = "value"
    ) |>
    mutate(
      year = as.numeric(year),
      
      # Treat each assessment version as its own modeled stock time series.
      stockid = paste(stock_id, assessment_id, sep = "_")
    ) |>
    filter(
      year >= start_year,
      !is.na(value),
      is.finite(value),
      value >= 0
    ) |>
    arrange(variable, stockid, year)
  
  if (nrow(analysis_data) == 0) {
    stop(
      "No usable observations remain after filtering to the requested start year."
    )
  }
  
  # Coverage denominator: all modeled stock-assessment series with at least
  # one usable observation for the respective variable.
  stock_totals <- analysis_data |>
    group_by(variable) |>
    summarise(
      Ntotal = n_distinct(stockid),
      .groups = "drop"
    )
  
  coverage_df <- analysis_data |>
    group_by(variable, year) |>
    summarise(
      N = n_distinct(stockid),
      .groups = "drop"
    ) |>
    left_join(stock_totals, by = "variable") |>
    mutate(
      Coverage = N / Ntotal
    )
  
  print(
    coverage_df |>
      group_by(variable) |>
      summarise(
        total_stock_assessments = first(Ntotal),
        first_year = min(year),
        last_year = max(year),
        maximum_coverage = max(Coverage, na.rm = TRUE),
        .groups = "drop"
      )
  )
  
  purrr::map_dfr(
    variables_to_analyze,
    function(current_variable) {
      
      dat <- analysis_data |>
        filter(variable == current_variable) |>
        arrange(stockid, year)
      
      if (
        nrow(dat) == 0 ||
        n_distinct(dat$year) < 2 ||
        n_distinct(dat$stockid) < 2
      ) {
        warning(
          "Skipping ", current_variable,
          ": fewer than two years or fewer than two stock-assessment series."
        )
        return(NULL)
      }
      
      # The original model works on log-transformed ratios. The small offset
      # retains legitimate zero F/Fmsy records.
      dat <- dat |>
        mutate(
          lnvar = log(value + 0.001),
          stockid = factor(stockid)
        )
      
      # --------------------------------------------------------
      # Fixed-effects comparison model
      # --------------------------------------------------------
      
      lm_fit <- lm(
        lnvar ~ stockid + factor(year),
        data = dat
      )
      
      lm_coef <- coef(lm_fit)
      year_levels <- sort(unique(dat$year))
      baseline_effect <- unname(lm_coef["(Intercept)"])
      
      lm_effects <- tibble(year = year_levels) |>
        mutate(
          year_term = paste0("factor(year)", year),
          fixed.effects = case_when(
            year == min(year) ~ exp(baseline_effect),
            year_term %in% names(lm_coef) ~
              exp(baseline_effect + lm_coef[year_term]),
            TRUE ~ NA_real_
          )
        ) |>
        select(year, fixed.effects)
      
      # --------------------------------------------------------
      # Build stock-by-year data matrix for TMB
      # --------------------------------------------------------
      
      y <- dat |>
        select(stockid, year, lnvar) |>
        distinct() |>
        pivot_wider(
          id_cols = stockid,
          names_from = year,
          values_from = lnvar
        ) |>
        arrange(stockid) |>
        select(-stockid) |>
        as.matrix()
      
      y <- y[, order(as.numeric(colnames(y))), drop = FALSE]
      
      n <- ncol(y)  # Number of years
      m <- nrow(y)  # Number of stock-assessment time series
      
      if (n < 2 || m < 2) {
        warning(
          "Skipping ", current_variable,
          ": model matrix has fewer than two years or rows."
        )
        return(NULL)
      }
      
      ypresent <- ifelse(is.na(y), 0, 1)
      
      # TMB uses zero-based indexing for the first observed year.
      first_obs <- apply(ypresent, 1, which.max) - 1
      
      # --------------------------------------------------------
      # Fit TMB dynamic linear model
      # --------------------------------------------------------
      
      obj <- TMB::MakeADFun(
        data = list(
          y = y,
          ypresent = ypresent,
          first_obs = first_obs
        ),
        parameters = list(
          lnsde = log(0.1),
          lnsdx = log(0.1),
          logitrho = -log(2 / (1 + 0.5) - 1),
          x = rep(0, n),
          Apar = rep(0, m - 1)
        ),
        random = "x",
        DLL = "dlm_ar1",
        silent = TRUE
      )
      
      opt <- nlminb(
        start = obj$par,
        objective = obj$fn,
        gradient = obj$gr,
        lower = c(
          lnsde = log(0.05),
          lnsdx = log(0.05)
        ),
        control = list(
          iter.max = 1000,
          eval.max = 1000
        )
      )
      
      all_years <- as.numeric(colnames(y))
      
      if (opt$convergence == 0) {
        
        rep <- TMB::sdreport(obj)
        srep <- summary(rep)
        
        x_rows <- grepl("^x", rownames(srep))
        
        pred.df <- as_tibble(srep[x_rows, , drop = FALSE]) |>
          mutate(year = all_years) |>
          left_join(
            coverage_df |>
              filter(variable == current_variable),
            by = "year"
          ) |>
          mutate(
            fpc.se = if_else(
              Ntotal > 1,
              `Std. Error` * sqrt((Ntotal - N) / (Ntotal - 1)),
              `Std. Error`
            ),
            dlm.geomean = exp(Estimate),
            dlm.lower = exp(Estimate - 1.96 * fpc.se),
            dlm.upper = exp(Estimate + 1.96 * fpc.se)
          ) |>
          select(
            year,
            Coverage,
            dlm.geomean,
            dlm.lower,
            dlm.upper
          )
        
      } else {
        
        warning(
          "TMB optimizer did not converge for ",
          current_variable,
          ". nlminb convergence code: ",
          opt$convergence
        )
        
        pred.df <- tibble(
          year = all_years,
          Coverage = NA_real_,
          dlm.geomean = NA_real_,
          dlm.lower = NA_real_,
          dlm.upper = NA_real_
        )
      }
      
      # --------------------------------------------------------
      # Observed annual distribution statistics
      # --------------------------------------------------------
      
      stats_df <- dat |>
        group_by(year) |>
        summarise(
          lower.whisker = boxplot.stats(value)$stats[1],
          q.25 = boxplot.stats(value)$stats[2],
          median = boxplot.stats(value)$stats[3],
          q.75 = boxplot.stats(value)$stats[4],
          upper.whisker = boxplot.stats(value)$stats[5],
          .groups = "drop"
        )
      
      # Follow the original approach: use years with >90% stock coverage
      # to rescale the log-scale model estimates to observed medians.
      high_coverage_years <- coverage_df |>
        filter(
          variable == current_variable,
          Coverage > 0.9
        )
      
      pred.df <- pred.df |>
        left_join(lm_effects, by = "year")
      
      scaling_df <- pred.df |>
        inner_join(stats_df, by = "year") |>
        filter(year %in% high_coverage_years$year)
      
      # If no years exceed 90% coverage, retain model scale rather than fail.
      scale_dlm <- if (
        nrow(scaling_df) > 0 &&
        sum(scaling_df$dlm.geomean, na.rm = TRUE) > 0
      ) {
        sum(scaling_df$median, na.rm = TRUE) /
          sum(scaling_df$dlm.geomean, na.rm = TRUE)
      } else {
        1
      }
      
      scale_fixed <- if (
        nrow(scaling_df) > 0 &&
        sum(scaling_df$fixed.effects, na.rm = TRUE) > 0
      ) {
        sum(scaling_df$median, na.rm = TRUE) /
          sum(scaling_df$fixed.effects, na.rm = TRUE)
      } else {
        1
      }
      
      pred.df |>
        mutate(
          variable = current_variable,
          dlm.geomean = dlm.geomean * scale_dlm,
          dlm.lower = dlm.lower * scale_dlm,
          dlm.upper = dlm.upper * scale_dlm,
          fixed.effects = fixed.effects * scale_fixed
        ) |>
        left_join(stats_df, by = "year") |>
        select(
          variable,
          year,
          Coverage,
          dlm.geomean,
          dlm.lower,
          dlm.upper,
          fixed.effects,
          lower.whisker,
          q.25,
          median,
          q.75,
          upper.whisker
        ) |>
        arrange(year)
    }
  )
}

# ------------------------------------------------------------
# 4. Run one national analysis across all stock IDs
# ------------------------------------------------------------

us_prep <- run_national_fisheries_analysis(
  fish_stock_input = fish_stock_input,
  variables_to_analyze = c("BvB", "UvU"),
  start_year = 1980
) |>
  mutate(
    variable = factor(
      variable,
      levels = c("BvB", "UvU")
    )
  )

print(us_prep)

# ------------------------------------------------------------
# 5. Save national output
# ------------------------------------------------------------

output_data_dir <- file.path(project_dir, "data")

if (!dir.exists(output_data_dir)) {
  dir.create(output_data_dir, recursive = TRUE)
}

readr::write_csv(
  us_prep,
  file.path(output_data_dir, "us_prep_stocksmart_all_stocks.csv")
)
