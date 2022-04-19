# Analysis

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", 2014) 
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   2019) # 2019
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         2013) 

# Figures: Function ------------------------------------------------------------
make_figure <- function(df, x_var, y_var, x_var_title, y_var_title, title){
  df$x_var <- df[[x_var]]
  df$y_var <- df[[y_var]]
  
  df <- df %>%
    filter(!is.na(x_var) & !is.na(y_var)) %>%
    filter(y_var > 0)
  
  cor.test(df$x_var, df$y_var) %>% print()
  
  df %>%
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
p_employ_mex_dmspols <- make_figure(mex_dmspols,
                                    "dmspolszhang_mean_diff2", 
                                    "employment_sum_all_diff2",
                                    "Change in DMSP-OLS (10 Years)",
                                    "Change in Employment (10 Years)",
                                    "DMSP-OLS (10 Year Diff)")

p_employ_mex_viirs <- make_figure(mex_viirs,
                                  "viirs_mean_diff2", 
                                  "employment_sum_all_diff2",
                                  "Change in VIIRS (2 Years)",
                                  "Change in Employment (2 Years)",
                                  "VIIRS (2 Year Diff)")

p_employ_can_dmspols <- make_figure(can,
                                    "dmspolszhang_mean_diff6", 
                                    "employment_sum_all_diff6",
                                    "Change DMSP-OLS (12 Years)",
                                    "Change Employment (12 Years)",
                                    "DMSP-OLS (12 Year Diff)")

p_employ_can_viirs <- make_figure(can,
                                  "viirs_mean_diff1", 
                                  "employment_sum_all_diff1",
                                  "Change VIIRS (2 Years)",
                                  "Change Employment (2 Years)",
                                  "VIIRS (2 Year Diff)")

p_employ_can <- ggarrange(p_employ_can_dmspols, p_employ_can_viirs) %>%
  annotate_figure(top = text_grob("Canada", color = "black", face = "bold", size = 14))
p_employ_mex <- ggarrange(p_employ_mex_dmspols, p_employ_mex_viirs) %>%
  annotate_figure(top = text_grob("Mexico", color = "black", face = "bold", size = 14))

p_employ <- ggarrange(p_employ_can, p_employ_mex)
ggsave(p_employ, filename = file.path(figures_file_path, paste0("diff_scatter_", "employment_sum_all", ".png")), height=10, width=10)



