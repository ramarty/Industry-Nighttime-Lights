# Analysis

round_bin <- function(x) as.factor(round(x*10)/10)

calc_per_change <- function(x){
  x_max <- max(x)
  x_min <- min(x)
  x_min[x_min %in% 0] <- 1
  (x_max - x_min) / x_min
}

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", "all", "5km Grid")
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   "all", "5km Grid")
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         "all", "5km Grid")

mex_viirs$employment_sum_all[mex_viirs$year %in% c(2017, 2018, 2020)] <- 
  mex_viirs$empl_med_sum_all[mex_viirs$year %in% c(2017, 2018, 2020)]

# Correlation Dataframe --------------------------------------------------------
df <- mex_viirs
cor_df <- df %>%
  group_by(id) %>%
  dplyr::summarise(cor_dmspols_firms        = cor(dmspols_mean_log,        N_firms_sum_all_log),
                   cor_dmspolszhang_firms   = cor(dmspolszhang_mean_log,   N_firms_sum_all_log),
                   cor_dmspolselvidge_firms = cor(dmspolselvidge_mean_log, N_firms_sum_all_log),
                   cor_viirs_firms          = cor(viirs_mean, N_firms_sum_all),
                   
                   cor_dmspols_employ        = cor(dmspols_mean_log,        employment_sum_all_log),
                   cor_dmspolszhang_employ   = cor(dmspolszhang_mean_log,   employment_sum_all_log),
                   cor_dmspolselvidge_employ = cor(dmspolselvidge_mean_log, employment_sum_all_log),
                   
                   N_firms_sum_all_min = min(N_firms_sum_all),
                   employment_sum_all_diff = max(employment_sum_all) - min(employment_sum_all),
                   N_firms_sum_all_diff  = max(N_firms_sum_all) - min(N_firms_sum_all),
                   employment_sum_all_pchange = calc_per_change(employment_sum_all),
                   N_firms_sum_all_pchange = calc_per_change(N_firms_sum_all)) %>%
  dplyr::mutate_at(vars(matches("cor")), round_bin) 

cor_df$firm_bin <- as.factor(as.numeric(cor_df$N_firms_sum_all_diff >= 25))
cor_df$cor_var <- cor_df$cor_viirs_firms

cor_df_col <- cor_df %>%
  group_by(firm_bin, cor_var) %>%
  dplyr::summarise(N = n()) %>%
  group_by(cor_var) %>%
  mutate(N_all = sum(N)) %>%
  dplyr::mutate(prop = N/N_all)

cor_df_col_l <- cor_df_col %>%
  dplyr::select(-N_all) %>%
  pivot_longer(cols = c("N", "prop"))

cor_df_col_l$name[cor_df_col_l$name %in% "N"] <- "Number of Units"
cor_df_col_l$name[cor_df_col_l$name %in% "prop"] <- "Proportion of Units"

cor_df_col_l %>%
  filter(!is.na(cor_var)) %>%
  ggplot() +
  geom_col(aes(x = cor_var, y = value, 
               fill = firm_bin,
               group = firm_bin)) + 
  labs(x = "Correlation", 
       y = "N", 
       title = "Correlation within Units, Across Time",
       fill = "N Firms\nChanged by 50") +
  scale_fill_manual(values = c("darkorange1", "dodgerblue3"),
                    labels = c("No", "Yes")) + 
  theme_minimal() +
  facet_wrap(~name, 
             scale = "free_y")


