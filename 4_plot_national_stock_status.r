# National Stock SMART stock-status plot
#
# Requires `us_prep`, created by run_stocksmart_dlm_analysis_compile_in_cpp.r.
# The input must contain at least:
# year, variable, dlm.geomean, dlm.lower, dlm.upper, and median.
#
# This script plots only the national/USA Dynamic Linear Model results.

library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. Validate input
# ------------------------------------------------------------

if (!exists("us_prep")) {
  stop(
    "The object `us_prep` does not exist. ",
    "Run the national Dynamic Linear Model analysis first."
  )
}

required_columns <- c(
  "year",
  "variable",
  "dlm.geomean",
  "dlm.lower",
  "dlm.upper",
  "median"
)

missing_columns <- setdiff(required_columns, names(us_prep))

if (length(missing_columns) > 0) {
  stop(
    "us_prep is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 2. Prepare national U.S. plotting data
# ------------------------------------------------------------

national_plot_data <- us_prep |>
  filter(
    year >= 1980,
    variable %in% c("BvB", "UvU"),
    !is.na(dlm.geomean)
  ) |>
  mutate(
    variable = recode(
      as.character(variable),
      "BvB" = "B/Bmsy",
      "UvU" = "F/Fmsy"
    ),
    variable = factor(
      variable,
      levels = c("B/Bmsy", "F/Fmsy")
    )
  ) |>
  arrange(variable, year)

if (nrow(national_plot_data) == 0) {
  stop(
    "No national BvB/UvU model estimates are available for 1980 onward."
  )
}

# Report available data before plotting.
print(
  national_plot_data |>
    group_by(variable) |>
    summarise(
      first_year = min(year, na.rm = TRUE),
      last_year = max(year, na.rm = TRUE),
      annual_estimates = n(),
      .groups = "drop"
    )
)

# Create readable five-year x-axis breaks.
x_break_start <- floor(min(national_plot_data$year, na.rm = TRUE) / 5) * 5
x_break_end <- ceiling(max(national_plot_data$year, na.rm = TRUE) / 5) * 5

# ------------------------------------------------------------
# 3. Create national Dynamic Linear Model plot
# ------------------------------------------------------------

national_plot <- ggplot(
  national_plot_data,
  aes(
    x = year,
    y = dlm.geomean,
    color = variable,
    fill = variable
  )
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "grey35",
    linewidth = 0.5
  ) +
  geom_ribbon(
    aes(
      ymin = dlm.lower,
      ymax = dlm.upper
    ),
    alpha = 0.18,
    color = NA,
    na.rm = TRUE
  ) +
  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +
  geom_point(
    aes(y = median),
    shape = 21,
    fill = "white",
    size = 1.8,
    stroke = 0.45,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = c(
      "B/Bmsy" = "#0072B2",
      "F/Fmsy" = "#D55E00"
    )
  ) +
  scale_fill_manual(
    values = c(
      "B/Bmsy" = "#0072B2",
      "F/Fmsy" = "#D55E00"
    )
  ) +
  scale_x_continuous(
    breaks = seq(x_break_start, x_break_end, by = 5)
  ) +
  labs(
    title = "United States Stock Status Relative to F and B MSY",
    subtitle = "DO NOT PUBLISH WITHOUT INPUT FROM OLAF ET AL. - National Dynamic Linear Model estimates; shaded bands are 95% confidence intervals",
    x = "Year",
    y = "Ratio relative to MSY reference point",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Explicitly print the ggplot, including when this script is sourced.
print(national_plot)

# ------------------------------------------------------------
# 4. Optional: save high-resolution PNG
# ------------------------------------------------------------

# ggsave(
#   filename = "national_stock_status_dlm.png",
#   plot = national_plot,
#   width = 10,
#   height = 6,
#   units = "in",
#   dpi = 300
# )
