# Analysis

library(ggtext)

# Load Data --------------------------------------------------------------------
cor_df <- readRDS(file.path(data_file_path, "Results", "correlation_within_unit.Rds"))

# Prep Data --------------------------------------------------------------------
cor_df <- cor_df %>%
  dplyr::mutate(cor_p5 = cor > 0.5,
                ntl_var_change = abs(ntl_var_max - ntl_var_min),
                firm_var_change = abs(firm_var_max - firm_var_min),
                
                ntl_var_splag_change = abs(ntl_var_splag_max - ntl_var_splag_min),
                firm_var_splag_change = abs(firm_var_splag_max - firm_var_splag_min)) %>%
  dplyr::mutate(firm_var_splag_change_diff_abs = abs(firm_var_change - firm_var_splag_change),
                firm_var_splag_change_diff = (firm_var_splag_change - firm_var_change)/firm_var_change) %>%
  dplyr::mutate(firm_var_change_sq = firm_var_change^2) %>%
  dplyr::filter(transform %in% "log")

# Regressions ------------------------------------------------------------------
## Dataframe that includes model
lm_list <- cor_df %>%
  group_by(country, unit, ntl_var, firm_var) %>%
  do(model = lm(cor ~ firm_var_min + firm_var_change, data = .)) %>%
  ungroup() %>%
  mutate(model_id = 1:n())

## Add model parameters
lm_coefs <- map_df(lm_list$model_id, function(i){
  
  lm_list$model[lm_list$model_id %in% i][[1]] %>% 
    extract_coefs() %>%
    mutate(model_id = i) %>%
    dplyr::filter(var != "(Intercept)")
  
})

lm_list$model <- NULL
lm_coefs <- lm_coefs %>% 
  left_join(lm_list, by = "model_id")

## Cleanup
lm_coefs <- lm_coefs %>%
  mutate(adj.r.squared = "R<sup>2</sup>: " %>% paste0(round(adj.r.squared, 3))) %>%
  mutate(var = case_when(var == "firm_var_change" ~ "Firm Growth Rate",
                         var == "firm_var_min" ~ "Firm Min. Value (Log)",
                         TRUE ~ var)) %>%
  mutate(ntl_var = case_when(ntl_var == "dmspolsharmon_mean" ~ "DMSP-OLS",
                             ntl_var == "viirs_mean" ~ "VIIRS")) %>%
  mutate(unit = unit %>% factor(levels = c("City",
                                           "Grid in Cities",
                                           "5km Grid",
                                           "10km Grid",
                                           "25km Grid",
                                           "50km Grid",
                                           "100km Grid")))

lm_coefs <- lm_coefs[!(lm_coefs$country == "Canada" & lm_coefs$ntl_var == "VIIRS"),]

# Visualize --------------------------------------------------------------------
make_figure <- function(country_i,
                        firmvar_i){
  
  if(firmvar_i == "N_firms_sum_all")    firmvar_clean <- "N Firms"
  if(firmvar_i == "employment_sum_all") firmvar_clean <- "Employment"
  
  if(country_i == "Mexico" & firmvar_i == "N_firms_sum_all") text_x <- 0.2
  if(country_i == "Mexico" & firmvar_i == "employment_sum_all") text_x <- 0.1
  
  if(country_i == "Canada" & firmvar_i == "N_firms_sum_all") text_x <- 0.025
  if(country_i == "Canada" & firmvar_i == "employment_sum_all") text_x <- 0.025
  
  lm_coefs %>%
    filter(country == country_i) %>%
    filter(firm_var == firmvar_i) %>%
    ggplot(aes(y=unit,
               x = coef,
               xmin = ci2_5,
               xmax = ci97_5)) +
    geom_vline(xintercept = 0,
               alpha = 0.7) +
    geom_point(aes(color = var),
               position = position_dodge(0.2)) + 
    geom_linerange(aes(color = var),
                   position = position_dodge(0.2)) +
    geom_richtext(aes(x = text_x, 
                      y = unit,
                      label = adj.r.squared),
                  fill = NA, label.color = NA, 
                  label.padding = grid::unit(rep(0, 4), "pt"),
                  vjust = -0.8,
                  size = 3) +
    labs(color = "Variable",
         x = "Coef (+/- 95% CI)",
         y = NULL,
         title = firmvar_clean) +
    scale_color_manual(values = c("dodgerblue3", "darkorange2")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~ntl_var)
  
}

mex_firms <- make_figure("Mexico", "N_firms_sum_all")
mex_employ <- make_figure("Mexico", "employment_sum_all")

can_firms <- make_figure("Canada", "N_firms_sum_all")
can_employ <- make_figure("Canada", "employment_sum_all")

can <- ggarrange(can_firms,
                 can_employ,
                 ncol = 2,
                 common.legend = T,
                 legend = "right") %>%
  annotate_figure(top = text_grob("Canada", color = "black", face = "bold", size = 14))

mex <- ggarrange(mex_firms,
                 mex_employ,
                 ncol = 2,
                 common.legend = T,
                 legend = "right") %>%
  annotate_figure(top = text_grob("Mexico", color = "black", face = "bold", size = 14))

p <- ggarrange(can,
               mex,
               nrow = 2)

ggsave(p, filename = file.path(figures_file_path, "predict_within_cor_reg.png"),
       height = 10, 
       width = 12)

# Other Figure -----------------------------------------------------------------
cor_df %>%
  filter(country == "Mexico",
         unit == "5km Grid",
         firm_var == "N_firms_sum_all",
         ntl_var == "viirs_mean") %>%
  ggplot(aes(x = firm_var_change,
             y = cor)) +
  geom_point()+
  labs(x = "Firm Growth Rate",
       y = "Correlation",
       title = "Firm Growth Rate within Units vs. Within Unit\nCorrelation Between N Firms and VIIRS\n(Mexico, 5km Grid)") +
  ggsave(filename = file.path(figures_file_path, "withincor_vs_firmgrowth.png"),
         height = 4, 
         width = 4)


