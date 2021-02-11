# Analysis

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", 2009)
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   2019)
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         2013)

# Figures: Function ------------------------------------------------------------
make_figure <- function(df, x_var, y_var, x_var_title, y_var_title, title){
  df$x_var <- df[[x_var]]
  df$y_var <- df[[y_var]]
  
  df %>%
    filter(!is.na(x_var) & !is.na(y_var)) %>%
    filter(y_var > 0) %>%
    ggplot(aes(x = x_var, 
               y = y_var)) +
    geom_point(size=.1) +
    labs(x=x_var_title,
         y=y_var_title,
         title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face="bold")) + 
    facet_wrap(~unit,
               ncol = 1,
               scales = "free") 
}

# Figures: Employment ----------------------------------------------------------
for(industry_var in c("employment_sum_all_log", "N_firms_sum_all_log")){
  
  mex_dmspols$industry_var <- mex_dmspols[[industry_var]]
  mex_viirs$industry_var   <- mex_viirs[[industry_var]]
  can$industry_var         <- can[[industry_var]]
  
  if(industry_var %in% "employment_sum_all_log") y_axis_title <- "log(Employment)"
  if(industry_var %in% "N_firms_sum_all_log")    y_axis_title <- "log(N Firms)"
  
  ## Mexico
  p_employ_mex_dmspols <- make_figure(mex_dmspols, 
                                      "dmspols_mean_log", 
                                      industry_var,
                                      "log(NTL)",
                                      y_axis_title,
                                      "DMSP-OLS, 2009")
  p_employ_mex_viirs <- make_figure(mex_viirs, 
                                    "viirs_mean_log", 
                                    industry_var,
                                    "log(NTL)",
                                    y_axis_title,
                                    "VIIRS, 2019")
  p_employ_mex <- ggarrange(p_employ_mex_dmspols, p_employ_mex_viirs) %>%
    annotate_figure(top = text_grob("Mexico", color = "black", face = "bold", size = 14))
  
  ## Canada
  p_employ_can_dmspols <- make_figure(can, 
                                      "dmspols_mean_log", 
                                      industry_var,
                                      "log(NTL)",
                                      y_axis_title,
                                      "DMSP-OLS, 2013")
  p_employ_can_viirs <- make_figure(can, 
                                    "viirs_mean_log", 
                                    industry_var,
                                    "log(NTL)",
                                    y_axis_title,
                                    "VIIRS, 2013")
  p_employ_can <- ggarrange(p_employ_can_dmspols, p_employ_can_viirs) %>%
    annotate_figure(top = text_grob("Canada", color = "black", face = "bold", size = 14))
  
  ## Both
  p_employ <- ggarrange(p_employ_can, p_employ_mex)
  ggsave(p_employ, filename = file.path(figures_file_path, paste0("levels_scatter_", industry_var, ".png")), height=10, width=10)
}
