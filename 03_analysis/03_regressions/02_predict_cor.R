# Analysis

round_bin <- function(x) as.factor(round(x*10)/10)

calc_max_diff <- function(x){
  max(x) - min(x)
}

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", "all", "5km Grid")
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   "all", "5km Grid")
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         "all", "5km Grid")

# Correlation Dataframe --------------------------------------------------------
mex_viirs$employment_sum_all[mex_viirs$year %in% c(2017, 2018, 2020)] <- 
  mex_viirs$empl_med_sum_all[mex_viirs$year %in% c(2017, 2018, 2020)]

mex_dmspols_df <- mex_dmspols %>%
  group_by(id) %>%
  dplyr::summarise(cor_dmspols_firms  = cor(dmspolselvidge_mean_log, N_firms_sum_all_log),
                   cor_dmspols_employ = cor(dmspolselvidge_mean_log, employment_sum_all_log),
                   
                   N_firms_sum_all_log_max  = max(N_firms_sum_all_log),
                   N_firms_sum_all_log_min  = min(N_firms_sum_all_log),
                   N_firms_sum_all_log_diff = calc_max_diff(N_firms_sum_all_log),
                   
                   employment_sum_all_log_max  = max(employment_sum_all_log),
                   employment_sum_all_log_min  = min(employment_sum_all_log),
                   employment_sum_all_log_diff = calc_max_diff(employment_sum_all_log),
                   
                   dmspolselvidge_mean_log_init = dmspolselvidge_mean_log[year == 2004])

mex_dmspols_df$cor_dmspols_firms_bin <- as.numeric(mex_dmspols_df$cor_dmspols_firms > 0.7)
lm(cor_dmspols_firms_bin ~ N_firms_sum_all_log_diff, data =mex_dmspols_df) %>% summary()


cor_df <- df %>%
  group_by(id) %>%
  dplyr::summarise(cor_dmspols_firms        = cor(dmspols_mean_log,        N_firms_sum_all_log),
                   cor_dmspolszhang_firms   = cor(dmspolszhang_mean_log,   N_firms_sum_all_log),
                   cor_dmspolselvidge_firms = cor(dmspolselvidge_mean_log, N_firms_sum_all_log),
                   
                   cor_dmspols_employ        = cor(dmspols_mean_log,        employment_sum_all_log),
                   cor_dmspolszhang_employ   = cor(dmspolszhang_mean_log,   employment_sum_all_log),
                   cor_dmspolselvidge_employ = cor(dmspolselvidge_mean_log, employment_sum_all_log),
                   
                   employment_sum_all_diff = max(employment_sum_all) - min(employment_sum_all),
                   N_firms_sum_all_diff  = max(N_firms_sum_all) - min(N_firms_sum_all),
                   N_firms_sum_all_min = min(N_firms_sum_all),
                   N_firms_sum_all_max = max(N_firms_sum_all)) %>%
  dplyr::mutate_at(vars(matches("cor")), round_bin) %>%
  dplyr::mutate(N_firms_sum_all_diff_bin = as.factor(as.numeric(N_firms_sum_all_diff >= 2)))

cor_df <- cor_df %>%
  filter(N_firms_sum_all_max > 0)

cor_df$N_firms_sum_all_min[cor_df$N_firms_sum_all_min %in% 0] <- 1
cor_df <- cor_df %>%
  mutate(perchange = (N_firms_sum_all_max - N_firms_sum_all_min) / N_firms_sum_all_min)

cor_df$perchange_bin <- as.factor(as.numeric(cor_df$perchange >= 2 & cor_df$N_firms_sum_all_diff >= 5))

cor_df_col <- cor_df %>%
  group_by(perchange_bin, cor_dmspolselvidge_firms) %>%
  dplyr::summarise(N = n()) %>%
  group_by(cor_dmspolselvidge_firms) %>%
  mutate(N_all = sum(N)) %>%
  dplyr::mutate(prop = N/N_all)

cor_df_col_l <- cor_df_col %>%
  dplyr::select(-N_all) %>%
  pivot_longer(cols = c("N", "prop"))

cor_df_col_l$name[cor_df_col_l$name %in% "N"] <- "Number of Units"
cor_df_col_l$name[cor_df_col_l$name %in% "prop"] <- "Proportion of Units"

cor_df_col_l %>%
  filter(!is.na(cor_dmspolselvidge_firms)) %>%
  ggplot() +
  geom_col(aes(x = cor_dmspolselvidge_firms, y = value, 
               fill = perchange_bin,
               group = perchange_bin)) + 
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

a <- cor_df[cor_df$cor_dmspolselvidge_firms %in% c("0.9", "1"),]

df %>%
  filter(id == 2377) %>%
  ggplot() +
  geom_line(aes(x = year, y = dmspolselvidge_mean))

df %>%
  filter(id == 2377) %>%
  ggplot() +
  geom_line(aes(x = year, y = N_firms_sum_all))
