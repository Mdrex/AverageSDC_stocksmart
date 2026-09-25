# AverageSDC Stock SMART

This code adapts the approach from [samrblackburn/fish_stock_figure](https://github.com/samrblackburn/fish_stock_figure) and integrates Stock SMART data instead of RAM Legacy data.

Stock SMART outputs should be verified.

- Stock SMART F and B basis should be verified.
- Output data and model results should be verified.

## Steps

1. **`1_SM data and ratio estimates.R`**
   - Pulls Stock SMART data and generates ratio estimates.

2. **`2_Fratio and Bratio plot validation.R`**
   - Use this to validate trends against the original assessment document linked through Stock SMART.

3. **`3_run_stocksmart_dlm_analysis_compile_in_cpp2.R`**
   - Modifies the [`samrblackburn/fish_stock_figure`](https://github.com/samrblackburn/fish_stock_figure) workflow.
   - Requires downloading [`samrblackburn/fish_stock_figure`](https://github.com/samrblackburn/fish_stock_figure).

4. **`4_plot_national_stock_status.R`**
   - Plots national stock status.

All credit to [`samrblackburn/fish_stock_figure`](https://github.com/samrblackburn/fish_stock_figure).

<img width="891" height="423" alt="image" src="https://github.com/user-attachments/assets/4342a843-2b32-4a47-817e-6fb87c420380" />
