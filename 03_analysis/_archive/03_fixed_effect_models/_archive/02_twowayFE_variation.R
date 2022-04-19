# Analysis

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", "all", "5km Grid")
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   "all", "5km Grid")
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         "all", "5km Grid")

# Correlation Dataframe --------------------------------------------------------
calc_per_change <- function(x){
  x_max <- max(x)
  x_min <- min(x)
  x_min[x_min %in% 0] <- 1
  (x_max - x_min) / x_min
}

year_init <- 2001
year_init <- 2004
df <- mex_dmspols %>%
  group_by(id) %>%
  dplyr::mutate(N_firm_max = max(N_firms_sum_all)) %>%
  ungroup() %>%
  filter(N_firm_max > 0)

df <- df %>%
  group_by(id) %>%
  dplyr::mutate(N_firms_sum_all_log_diff = max(N_firms_sum_all_log) - min(N_firms_sum_all_log),
                employment_sum_all_log_diff = max(employment_sum_all_log) - min(employment_sum_all_log),
                
                N_firms_sum_all_perchange = calc_per_change(N_firms_sum_all),
                employment_sum_all_perchange = calc_per_change(employment_sum_all),

                dmspols_mean_log_init = dmspols_mean_log[year == year_init],
                N_firms_sum_all_log_init = N_firms_sum_all_log[year == year_init],
                employment_sum_all_log_init = employment_sum_all_log[year == year_init])


a <- felm(employment_sum_all_log ~ dmspols_mean_log | year + id | 0 | 0 , data = df[df$])
summary(a)

a <- felm(N_firms_sum_all_log ~ dmspols_mean_log + dmspols_mean_log*N_firms_sum_all_perchange*N_firms_sum_all_log_init | year + id | 0 | 0 , data = df)
summary(a)


df <- can

cor_df <- df %>%
  group_by(id) %>%
  dplyr::summarise(cor_dmspols_firms        = cor(dmspols_mean_log,        N_firms_sum_all),
                   cor_dmspolszhang_firms   = cor(dmspolszhang_mean_log,   N_firms_sum_all),
                   cor_dmspolselvidge_firms = cor(dmspolselvidge_mean_log, N_firms_sum_all),
                   
                   cor_dmspols_employ        = cor(dmspols_mean_log,        employment_sum_all),
                   cor_dmspolszhang_employ   = cor(dmspolszhang_mean_log,   employment_sum_all),
                   cor_dmspolselvidge_employ = cor(dmspolselvidge_mean_log, employment_sum_all),
                   
                   employment_sum_all_diff = max(employment_sum_all) - min(employment_sum_all),
                   N_firms_sum_all_diff  = max(N_firms_sum_all) - min(N_firms_sum_all),
                   N_firms_sum_all_max = max(N_firms_sum_all)) %>%
  dplyr::mutate_at(vars(matches("cor")), round_bin) %>%
  dplyr::mutate(N_firms_sum_all_diff_bin = as.factor(as.numeric(N_firms_sum_all_diff >= 25)))

cor_df <- cor_df %>%
  filter(N_firms_sum_all_max > 0)

cor_df_col <- cor_df %>%
  group_by(N_firms_sum_all_diff_bin, cor_dmspols_firms) %>%
  dplyr::summarise(N = n()) %>%
  group_by(cor_dmspols_firms) %>%
  mutate(N_all = sum(N)) %>%
  dplyr::mutate(prop = N/N_all)

cor_df_col_l <- cor_df_col %>%
  dplyr::select(-N_all) %>%
  pivot_longer(cols = c("N", "prop"))

cor_df_col_l$name[cor_df_col_l$name %in% "N"] <- "Number of Units"
cor_df_col_l$name[cor_df_col_l$name %in% "prop"] <- "Proportion of Units"

cor_df_col_l %>%
  filter(!is.na(cor_dmspols_firms)) %>%
  ggplot() +
  geom_col(aes(x = cor_dmspols_firms, y = value, 
               fill = N_firms_sum_all_diff_bin,
               group = N_firms_sum_all_diff_bin)) + 
  labs(x = "Correlation", 
       y = "N", 
       title = "Correlation within Units, Across Time",
       fill = "N Firms\nChanged by 50") +
  scale_fill_manual(values = c("darkorange1", "dodgerblue3"),
                    labels = c("No", "Yes")) + 
  theme_minimal() +
  facet_wrap(~name, 
             scale = "free_y")


cor_df <- cor_df[cor_df$N_firms_sum_all_max > 0,]
cor.test(cor_df$cor_dmspolselvidge_firms, log(cor_df$N_firms_sum_all_max))
cor.test(cor_df$cor_dmspolselvidge_firms, cor_df$N_firms_sum_all_diff)


