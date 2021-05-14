# Analysis

# Load Data --------------------------------------------------------------------
df_fe <- readRDS(file.path(data_file_path, "Results", "regression_results_yrid.Rds"))
df_longdiff <- readRDS(file.path(data_file_path, "Results", "regression_results_longdiff.Rds"))

df <- bind_rows(df_fe,
                df_longdiff)

df$FE[df$long_diff %in% T] <- "long-diff"

df <- df %>%
  filter(FE %in% c("id", "yr", "long-diff"),
         ntl_var %in% c("viirs_mean_log", "dmspolsharmon_mean_log"),
         dv_type %in% "ntl" # industry, ntl
  ) %>%
  mutate(unit = unit %>% factor(levels = c("City", "Grid in Cities", "5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid")),
         ntl_var = case_when(ntl_var == "viirs_mean_log" ~ "VIIRS",
                             ntl_var == "dmspolsharmon_mean_log" ~ "DMSP-OLS"),
         country = country %>% tools::toTitleCase()) %>%
  distinct(FE, splag, industry_var, ntl_var, unit, country, dv_type, .keep_all = T)

df$r2 <- df$P.adj.r.squared
df$r2[df$FE %in% "long-diff"] <- df$adj.r.squared[df$FE %in% "long-diff"]

# Figure Function --------------------------------------------------------------
make_figure <- function(df, FE_i, industry_var_i){
  
  if(industry_var_i %in% "employment_sum_all_log") title <- "Total Employment"
  if(industry_var_i %in% "N_firms_sum_all_log")    title <- "Number of Firms"
  
  df %>%
    filter(industry_var %in% industry_var_i,
           FE %in% FE_i) %>%
    ggplot(aes(y = r2,
               x = unit,
               color = splag)) +
    # color = var
    geom_hline(yintercept=0,size=.2,color="red") +
    geom_point() +
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

## Year FE
p_yr_employ <- make_figure(df, "yr", "employment_sum_all_log")
p_yr_firm   <- make_figure(df, "yr", "N_firms_sum_all_log")

p_yr <- ggarrange(p_yr_employ, p_yr_firm,
                  nrow = 1) %>%
  annotate_figure(top = text_grob("Year Fixed Effects - Association Across Levels", 
                                  color = "black", face = "bold", size = 14))

## Unit FE
p_id_employ <- make_figure(df, "id", "employment_sum_all_log")
p_id_firm   <- make_figure(df, "id", "N_firms_sum_all_log")

p_id <- ggarrange(p_id_employ, p_id_firm,
                  nrow = 1) %>%
  annotate_figure(top = text_grob("Unit Fixed Effects - Association Across Time", 
                                  color = "black", face = "bold", size = 14))

## Long Difference
p_ld_employ <- make_figure(df, "long-diff", "employment_sum_all_log")
p_ld_firm   <- make_figure(df, "long-diff", "N_firms_sum_all_log")

p_ld <- ggarrange(p_ld_employ, p_ld_firm,
                  nrow = 1) %>%
  annotate_figure(top = text_grob("Long Difference", 
                                  color = "black", face = "bold", size = 14))


## Append Everything Together
p <- ggarrange(p_yr,
               p_id,
               p_ld,
               nrow = 3)

ggsave(p, filename = file.path(figures_file_path, "fe_r2_fig.png"), 
       height = 9, width=12)

