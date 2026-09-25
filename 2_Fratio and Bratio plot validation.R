------------------------------------------------------------
  # 6. Randomly select five stocks with usable ratio values
  # ------------------------------------------------------------

set.seed(123)

eligible_stocks <- newest_assessment_data_with_refpts |>
  filter(!is.na(Bratio) | !is.na(Fratio)) |>
  distinct(stock_id, stock_name) |>
  filter(!is.na(stock_id), !is.na(stock_name))

if (nrow(eligible_stocks) < 6) {
  stop("Fewer than five stocks have usable B/BMSY or F/FMSY values.")
}

selected_stocks <- eligible_stocks |>
  slice_sample(n = 6) |>
  arrange(stock_name)

# Prints the selected stock names and identifiers.
print(selected_stocks)
print(selected_stocks$stock_name)

# ------------------------------------------------------------
# 7. Prepare plotted ratios for years 1980 onward
# ------------------------------------------------------------

ratio_plot_data <- newest_assessment_data_with_refpts |>
  semi_join(selected_stocks, by = c("stock_id", "stock_name")) |>
  select(
    stock_id,
    stock_name,
    year,
    Fratio,
    Bratio
  ) |>
  mutate(year = as.numeric(year)) |>
  pivot_longer(
    cols = c(Fratio, Bratio),
    names_to = "metric",
    values_to = "ratio"
  ) |>
  mutate(
    metric = recode(
      metric,
      Fratio = "F/Fmsy",
      Bratio = "B/Bmsy"
    ),
    metric = factor(metric, levels = c("F/Fmsy", "B/Bmsy"))
  ) |>
  filter(
    !is.na(year),
    year >= 1970,
    !is.na(ratio)
  )

# ------------------------------------------------------------
# 8. Plot B/BMSY and F/FMSY together for five stocks
# ------------------------------------------------------------
# Each stock receives one facet. Five facets are arranged in a
# 3-row x 2-column layout, leaving one unused panel space.

ratio_plot <- ggplot(
  ratio_plot_data,
  aes(
    x = year,
    y = ratio,
    color = metric,
    group = metric
  )
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "grey35",
    linewidth = 0.5
  ) +
  geom_line(
    linewidth = 0.85,
    na.rm = TRUE
  ) +
  geom_point(
    size = 1.5,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ stock_name,
    ncol = 2,
    nrow = 3,
    scales = "free_y"
  ) +
  scale_color_manual(
    values = c(
      "F/Fmsy" = "#D55E00",
      "B/Bmsy" = "#0072B2"
    )
  ) +
  labs(
    title = "F/Fmsy and B/Bmsy for Five Randomly Selected Stocks",
    subtitle = "Years 1980 onward; dashed line indicates the MSY reference threshold (ratio = 1)",
    x = "Year",
    y = "Ratio",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(ratio_plot)
