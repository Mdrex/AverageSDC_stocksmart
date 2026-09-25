# Stock SMART: latest assessment per stock, BMSY/FMSY reference points,
# derived ratios, and plots for five randomly selected stocks.
#
# This script:
#   1. Loads stock_assessment_data from the stocksmart package.
#   2. Keeps every row associated with the most recent assessment year per stock_id.
#   3. Loads assessment summary data and joins BMSY/FMSY by stock_id + assessment_id.
#   4. Creates Bratio for Abundance rows and Fratio for Fmort rows.
#   5. Randomly selects five eligible stocks and plots ratios for 1980 onward
#      in a 3-row x 2-column faceted ggplot.
#
# Install once if needed:
# remotes::install_github("NOAA-EDAB/stocksmart")
# install.packages(c("dplyr", "tidyr", "ggplot2", "janitor"))

library(stocksmart)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)

# ------------------------------------------------------------
# 1. Load Stock SMART time-series assessment data
# ------------------------------------------------------------

data("stock_assessment_data", package = "stocksmart")

print(names(stock_assessment_data))

# ------------------------------------------------------------
# 2. Keep all rows from the latest assessment year per stock
# ------------------------------------------------------------
# If a stock has multiple assessment IDs in its newest assessment year,
# all tied newest assessments are retained.

newest_assessment_data <- stock_assessment_data |>
  filter(
    !is.na(stock_id),
    !is.na(assessment_id),
    !is.na(assessment_year)
  ) |>
  group_by(stock_id) |>
  filter(assessment_year == max(assessment_year, na.rm = TRUE)) |>
  ungroup()

newest_assessment_lookup <- newest_assessment_data |>
  distinct(
    stock_id,
    stock_name,
    assessment_id,
    assessment_year,
    assessment_type
  ) |>
  arrange(stock_name, assessment_year, assessment_id)

print(newest_assessment_lookup)

latest_assessment_ties <- newest_assessment_lookup |>
  count(stock_id, name = "n_latest_assessments") |>
  filter(n_latest_assessments > 1)

print(latest_assessment_ties)

# ------------------------------------------------------------
# 3. Load the Stock SMART assessment-summary data
# ------------------------------------------------------------
# Package releases may use either snake_case or the older camelCase name.

available_datasets <- data(package = "stocksmart")$results[, "Item"]

if ("stock_assessment_summary" %in% available_datasets) {
  data("stock_assessment_summary", package = "stocksmart")
} else if ("stockAssessmentSummary" %in% available_datasets) {
  data("stockAssessmentSummary", package = "stocksmart")
  stock_assessment_summary <- stockAssessmentSummary
} else {
  stop(
    "Could not find a stock assessment summary dataset in the installed stocksmart package. ",
    "Run data(package = 'stocksmart')$results[, 'Item'] to inspect available datasets."
  )
}

assessment_reference_points <- stock_assessment_summary |>
  clean_names()

print(
  grep(
    pattern = "bmsy|fmsy|stock_id|assessment",
    x = names(assessment_reference_points),
    value = TRUE,
    ignore.case = TRUE
  )
)

# ------------------------------------------------------------
# 4. Build a one-row-per-assessment BMSY/FMSY lookup
# ------------------------------------------------------------

# ------------------------------------------------------------
# 4. Build a one-row-per-assessment BMSY/FMSY lookup
# ------------------------------------------------------------

assessment_reference_points_join <- assessment_reference_points |>
  transmute(
    stock_id,
    assessment_id,
    bmsy,
    fmsy
  ) |>
  distinct(
    stock_id,
    assessment_id,
    .keep_all = TRUE
  )

# ------------------------------------------------------------
# 5. Join reference points and calculate annual ratios
# ------------------------------------------------------------

newest_assessment_data_with_refpts <- newest_assessment_data |>
  left_join(
    assessment_reference_points_join,
    by = c("stock_id", "assessment_id")
  ) |>
  mutate(
    Bratio = case_when(
      metric == "Abundance" & !is.na(bmsy) & bmsy != 0 ~ value / bmsy,
      TRUE ~ NA_real_
    ),
    Fratio = case_when(
      metric == "Fmort" & !is.na(fmsy) & fmsy != 0 ~ value / fmsy,
      TRUE ~ NA_real_
    )
  )

reference_point_join_summary <- newest_assessment_data_with_refpts |>
  distinct(stock_id, stock_name, assessment_id, assessment_year, bmsy, fmsy) |>
  summarise(
    newest_assessments = n(),
    assessments_with_bmsy = sum(!is.na(bmsy)),
    assessments_with_fmsy = sum(!is.na(fmsy)),
    assessments_with_both = sum(!is.na(bmsy) & !is.na(fmsy))
  )

print(reference_point_join_summary)
