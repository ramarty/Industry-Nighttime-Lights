# Analysis

# Load Data --------------------------------------------------------------------
cor_df <- readRDS(file.path(data_file_path, "Results", "cor_cities_levels_ldwithin.Rds"))

## Within
levels_df <- cor_df %>%
  filter(type %in% "cor_levels_within",
         industry_var %in% "N_firms_sum_all_log",
         N >= 10) %>%
  mutate(ntl_var = case_when(
    ntl_var == "dmspolsharmon_mean_log" ~ "DMSP-OLS",
    ntl_var == "viirs_mean_log" ~ "VIIRS"
  )) %>%
  mutate(country = case_when(
    country == "mexico" ~ "Mexico",
    country == "canada" ~ "Canada"
  ))

# Distribution -----------------------------------------------------------------
ggplot() +
  geom_histogram(data = levels_df,
                 aes(x = cor,
                     group = ntl_var,
                     fill = ntl_var),
                 position="identity",
                 alpha = 0.5) +
  geom_vline(xintercept = 0, size = 0.1, color = "black") +
  xlim(-0.5, 1) +
  scale_fill_manual(values = c("dodgerblue3", "orange")) +
  facet_wrap(~country) +
  theme_minimal() +
  labs(x = "Correlation",
       y = "Number of Cities",
       fill = NULL,
       fill = NULL) +
  ggsave(file.path(figures_file_path, "city_dist_cor_within.png"), height = 2.5, width = 4.5)

# Stats -----------------------------------------------------------------
levels_df$id <- 1:nrow(levels_df)
levels_df$city_name <- fct_reorder(levels_df$city_name, levels_df$ntl_var_sd)
levels_df$id <- fct_reorder(as.character(levels_df$id), levels_df$ntl_var_sd)

stats_df <- levels_df %>%
  dplyr::filter(N >= 10) %>%
  group_by(country) %>%
  dplyr::summarise(cor_mean = mean(cor, na.rm=T),
                   N_mean = mean(N),
                   N_median = median(N),
                   ntl_var_sd_mean = mean(ntl_var_sd),
                   ntl_var_sd_median = median(ntl_var_sd))

