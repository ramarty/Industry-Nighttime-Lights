# Analysis

# TODO: If want multiple DMSPOLS, include in same plot with different colors

country_i <- "canada"
industry_var_i <- "employment_sum_all_log"

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "Results", "regression_results_yrid.Rds"))

df <- df %>%
  filter(unit %in% c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid"),
         FE %in% c("id", "yr"),
         ntl_var %in% c("viirs_mean_log", "dmspolsharmon_mean_log")) %>%
  mutate(unit = unit %>% factor(levels = c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid")),
         ntl_var = case_when(ntl_var == "viirs_mean_log" ~ "VIIRS",
                             ntl_var == "dmspolsharmon_mean_log" ~ "DMSP-OLS"),
         country = country %>% tools::toTitleCase()) 

# Figure Function --------------------------------------------------------------
make_figure <- function(df, FE_i, industry_var_i){
  
  # if(FE_i %in% "year_id") title <- "Year and Unit Fixed Effects"
  # if(FE_i %in% "yr")      title <- "Year Fixed Effects"
  # if(FE_i %in% "id")      title <- "Unit Fixed Effects"
  # 
  #df$country_ntl <- paste0(df$country %>% tools::toTitleCase(), 
  #                         ", ", 
  #                         df$ntl_var)
  
  if(industry_var_i %in% "employment_sum_all_log") title <- "Elasticity of NTL and Total Employment"
  if(industry_var_i %in% "N_firms_sum_all_log")    title <- "Elasticity of NTL and Number of Firms"
  
  df %>%
    filter(industry_var %in% industry_var_i,
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
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11)) +
    facet_wrap(~country + ntl_var,
               scales = "free_x",
               nrow = 1) +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"))
}

# Make Figure ------------------------------------------------------------------

p_yr_employ <- make_figure(df, "yr", "employment_sum_all_log")
p_yr_firm   <- make_figure(df, "yr", "N_firms_sum_all_log")

p_yr <- ggarrange(p_yr_employ, p_yr_firm,
                  nrow = 1) %>%
  annotate_figure(top = text_grob("Year Fixed Effects", 
                                  color = "black", face = "bold", size = 14))

p_id_employ <- make_figure(df, "id", "employment_sum_all_log")
p_id_firm   <- make_figure(df, "id", "N_firms_sum_all_log")

p_id <- ggarrange(p_id_employ, p_id_firm,
                  nrow = 1) %>%
  annotate_figure(top = text_grob("Unit Fixed Effects", 
                                  color = "black", face = "bold", size = 14))

p <- ggarrange(p_yr,
               p_id,
               nrow = 2)

ggsave(p, filename = file.path(figures_file_path, "fe_elasticity_fig.png"), 
       height = 6, width=12)




