# Analysis

# Load Data ---------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "Results", "regression_results_yrid.Rds"))

df$unit <- df$unit %>% factor(levels = c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid", "250km Grid", "500km Grid", "1000km Grid"))
df <- df[df$unit %in% c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid"),]

df$ntl_var[df$ntl_var %in% "dmspols_mean_log"] <- "DMSP-OLS"
df$ntl_var[df$ntl_var %in% "dmspolselvidge_mean_log"] <- "DMSP-OLS [Elvidge]"
df$ntl_var[df$ntl_var %in% "dmspolszhang_mean_log"] <- "DMSP-OLS [Zhang]"
df$ntl_var[df$ntl_var %in% "viirs_mean_log"] <- "VIIRS"

country_i <- "canada"
industry_var_i <- "employment_sum_all_log"

# Figure Function --------------------------------------------------------------
make_figure <- function(df, country_i, FE_i){
  
  if(FE_i %in% "year_id") title <- "Year and Unit Fixed Effects"
  if(FE_i %in% "yr")      title <- "Year Fixed Effects"
  if(FE_i %in% "id")      title <- "Unit Fixed Effects"
  
 df %>%
    filter(country %in% country_i,
           industry_var %in% industry_var_i,
           FE %in% FE_i) %>%
    ggplot(aes(y = coef,
               ymin = ci2_5,
               ymax = ci97_5,
               x = unit)) +
    geom_hline(yintercept=0,size=.2,color="red") +
    geom_point() +
    geom_linerange() +
    labs(x = NULL,
         y = "Coefficient (+/- 95% CI)",
         title = title) +
    coord_flip() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10)) +
    facet_wrap(~ntl_var,
               scales = "free_x",
               nrow = 1) 
}


for(industry_var_i in c("employment_sum_all_log", "N_firms_sum_all_log")){
  
  if(industry_var_i %in% "employment_sum_all_log") indus_title <- "Elasticity of Nighttime Lights to Number of Employment"
  if(industry_var_i %in% "N_firms_sum_all_log")    indus_title <- "Elasticity of Nighttime Lights to Number of Firms"
  
  p_yrid_mex <- make_figure(df, "mexico", "year_id")
  p_yr_mex   <- make_figure(df, "mexico", "yr")
  p_id_mex   <- make_figure(df, "mexico", "id")
  
  p_yrid_can <- make_figure(df, "canada", "year_id")
  p_yr_can   <- make_figure(df, "canada", "yr")
  p_id_can   <- make_figure(df, "canada", "id")
  
  p_can <- ggarrange(p_yr_can, p_id_can, p_yrid_can,
                     ncol = 1) %>%
    annotate_figure(top = text_grob("Canada", 
                                    color = "black", face = "bold", size = 14))
  
  p_mex <- ggarrange(p_yr_mex, p_id_mex, p_yrid_mex,
                     ncol = 1) %>%
    annotate_figure(top = text_grob("Mexico", 
                                    color = "black", face = "bold", size = 14))
  
  
  p <- ggarrange(p_can, p_mex) %>%
    annotate_figure(top = text_grob(indus_title, 
                                    color = "black", face = "bold", size = 14))
  
  
  ggsave(p, filename = file.path(figures_file_path, paste0("reg_fig_",industry_var_i,".png")), height = 9, width=15)
  
  
} 







