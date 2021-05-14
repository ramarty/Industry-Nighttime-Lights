# Analysis

# Load Data --------------------------------------------------------------------
cor_df <- readRDS(file.path(data_file_path, "Results", "cor_cities_levels_ldwithin.Rds"))

## Within
levels_df <- cor_df %>%
  filter(type %in% "cor_levels_within",
         industry_var %in% "N_firms_sum_all_log",
         N >= 100)

lm(cor ~ N + log(ntl_var_mean) + log(industry_var_mean) + country, data = levels_df[levels_df$ntl_var %in% "viirs_mean_log",]) %>%
  summary()

ggplot() +
  geom_histogram(data = within_df,
                 aes(x = cor,
                     group = ntl_var,
                     fill = ntl_var))


## ld
ld_df <- cor_df %>%
  filter(type %in% "cor_levels_longdiff",
         industry_var %in% "N_firms_sum_all_log",
         N >= 20)

ggplot() +
  geom_histogram(data = ld_df,
                 aes(x = cor,
                     group = ntl_var,
                     fill = ntl_var))

lm(cor ~ N + industry_var_ld_max, data = ld_df) %>% 
  summary()





