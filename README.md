# AverageSDC_stocksmart

This code takes the approach by https://github.com/samrblackburn/fish_stock_figure and integrates stock smart data instead of ram legacy data 
Stock smart outputs should be verified 
Stock smart F and B basis should be verified  
Outputs need to be verified

Steps: 

1_SM data and ratio estimates.R 
    *pulls stocksmart data and generates ratios
    
2_Fratio and Bratio plot validation.R 
    *use this to validate trends against original assessment document linked via stocksmart
    
3_run_stocksmart_dlm_analysis_compile_in_cpp2.R 
    *modifies samrblackburn/fish_stock_figure
    *must download samrblackburn/fish_stock_figure
    
4_plot_national_stock_status.r
    *plots the national stock 

All credit to [samrblackburn/fish_stock_figure](https://github.com/samrblackburn/fish_stock_figure/)
