# Analysis

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

cor_df <- cor_df %>%
  dplyr::mutate(ntl_var = case_when(ntl_var %in% "dmspolsharmon_mean" ~ "DMSP",
                                    ntl_var %in% "viirs_mean"        ~ "VIIRS"),
                firm_var = case_when(firm_var %in% "N_firms_sum_all"    ~ "N Firms",
                                     firm_var %in% "employment_sum_all" ~ "Employment")) %>%
  dplyr::mutate(unit = unit %>% factor(levels = c("City",
                                                  "Grid in Cities",
                                                  "5km Grid",
                                                  "10km Grid",
                                                  "25km Grid",
                                                  "50km Grid",
                                                  "100km Grid"))) %>%
  dplyr::mutate(title = paste0(country, "\n", ntl_var, " & ", firm_var))

# Regressions ------------------------------------------------------------------
## Dataframe that includes model
lm_list <- cor_df %>%
  dplyr::group_by(country, unit, ntl_var, firm_var, title) %>%
  do(model = lm(cor ~ firm_var_min + firm_var_change + firm_var_splag_change + firm_var_splag_change_diff_abs + firm_var_change:firm_var_splag_change_diff_abs + firm_var_splag_change:firm_var_splag_change_diff_abs, data = .)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(model_id = 1:n())

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
  mutate(adj.r.squared = paste0("<b>R<sup>2</sup>: ", round(adj.r.squared, 3)), "</b>") %>%
  mutate(var = case_when(var == "firm_var_change" ~ "Firm Growth Rate",
                         var == "firm_var_min" ~ "Firm Min. Value (Log)",
                         var == "firm_var_splag_change" ~ "Firm Growth Rate [Spatial Lag]",
                         var == "firm_var_splag_change_diff_abs" ~ "Diff Growth Rate: Firm and Spatial Lag",
                         TRUE ~ var)) 

lm_coefs <- lm_coefs[!(lm_coefs$country == "Canada" & lm_coefs$ntl_var == "VIIRS"),]

# Visualize --------------------------------------------------------------------
text_x <- 0.15

p <- lm_coefs %>%
  filter(unit != "City") %>%
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
       y = NULL) +
  scale_color_manual(values = c("dodgerblue3", "darkorange2", "red", "green", "purple", "black"),
                     guide = guide_legend(reverse = TRUE)) +
  theme(strip.text = element_text(face = "bold", size = 9),
        strip.background = element_blank()) +
  facet_wrap(~title,
             nrow = 1)

ggsave(p, filename = file.path(figures_file_path, "predict_within_cor_reg.png"),
       height = 5, 
       width = 10)

# Other Figure -----------------------------------------------------------------
# filter(country == "Mexico",
#        unit == "5km Grid",
#        firm_var == "N Firms",
#        ntl_var == "VIIRS") %>%

p_cor <- cor_df %>%
  dplyr::filter( (country == "Mexico") |
                   country == "Canada" & ntl_var == "DMSP") %>%
  mutate(unit = unit %>% factor() %>% fct_rev()) %>%
  ggplot(aes(x = firm_var_change,
             y = cor)) +
  geom_point(alpha = 1,
             size = 0.02,
             color = "dodgerblue4")+
  labs(x = "N Firms Growth Rate",
       y = "Correlation\nBetween NTL\nand N Firms\nWithin Units",
       title = NULL) +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        strip.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text = element_text(size = 16),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y.right = element_text(angle = 0, size = 20, face = "bold")) +
  facet_grid(unit~country+ntl_var) 

ggsave(p_cor, filename = file.path(figures_file_path, "withincor_vs_firmgrowth.png"),
       height = 8.5, 
       width = 13)


