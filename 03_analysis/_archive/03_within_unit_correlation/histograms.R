# Analysis

round_bin <- function(x) as.factor(round(x*10)/10)

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", "all", "5km Grid")
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   "all", "5km Grid")
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         "all", "5km Grid")

mex_viirs$employment_sum_all[mex_viirs$year %in% c(2017, 2018, 2020)] <- 
  mex_viirs$empl_med_sum_all[mex_viirs$year %in% c(2017, 2018, 2020)]

# Correlation Dataframe --------------------------------------------------------
make_cor_df <- function(df){
  
  cor_df <- df %>%
    group_by(id) %>%
    dplyr::summarise(#cor_dmspols_firms        = cor(dmspols_mean_log,        N_firms_sum_all_log),
      #cor_dmspolszhang_firms   = cor(dmspolszhang_mean_log,   N_firms_sum_all_log),
      cor_dmspolselvidge_firms = cor(dmspolselvidge_mean_log, N_firms_sum_all_log),
      cor_viirs_firms          = cor(viirs_mean_log, N_firms_sum_all_log),
      
      #cor_dmspols_employ        = cor(dmspols_mean_log,        employment_sum_all_log),
      #cor_dmspolszhang_employ   = cor(dmspolszhang_mean_log,   employment_sum_all_log),
      cor_dmspolselvidge_employ = cor(dmspolselvidge_mean_log, employment_sum_all_log),
      cor_viirs_employ          = cor(viirs_mean_log, employment_sum_all_log),
      
      N_firms_sum_all_max = max(N_firms_sum_all),
      employment_sum_all_max = max(employment_sum_all))# %>%
  #dplyr::mutate_at(vars(matches("cor")), round_bin) 
  
  #cor_df$cor_dmspols_firms[cor_df$N_firms_sum_all_max %in% 0] <- NA
  #cor_df$cor_dmspolszhang_firms[cor_df$N_firms_sum_all_max %in% 0] <- NA
  cor_df$cor_dmspolselvidge_firms[cor_df$N_firms_sum_all_max %in% 0] <- NA
  cor_df$cor_viirs_firms[cor_df$N_firms_sum_all_max %in% 0] <- NA
  
  #cor_df$cor_dmspols_employ[cor_df$employment_sum_all_max %in% 0] <- NA
  #cor_df$cor_dmspolszhang_employ[cor_df$employment_sum_all_max %in% 0] <- NA
  cor_df$cor_dmspolselvidge_employ[cor_df$employment_sum_all_max %in% 0] <- NA
  cor_df$cor_viirs_employ[cor_df$employment_sum_all_max %in% 0] <- NA
  
  cor_df$N_firms_sum_all_max <- NULL 
  cor_df$employment_sum_all_max <- NULL
  return(cor_df)
}

mex_viirs_df   <- make_cor_df(mex_viirs) %>% dplyr::select(id, cor_viirs_firms, cor_viirs_employ)
mex_dmspols_df <- make_cor_df(mex_dmspols) %>% dplyr::select(id, cor_dmspolselvidge_firms, cor_dmspolselvidge_employ) 
mex_df <- merge(mex_viirs_df, mex_dmspols_df, by = "id", all = T) %>% mutate(country = "Mexico")

can_df         <- make_cor_df(can) %>% mutate(country = "Canada")

# Append -----------------------------------------------------------------------
df <- bind_rows(mex_df, can_df) %>%
  pivot_longer(cols = -c(id, country)) %>%
  dplyr::rename(var = name,
                cor = value) %>%
  filter(!is.na(cor)) 

df$firm_var <- ""
df$firm_var[grepl("_firms", df$var)] <- "N Firms"
df$firm_var[grepl("_employ", df$var)] <- "Employment"

df$sat_var <- ""
df$sat_var[grepl("_viirs", df$var)] <- "VIIRS"
df$sat_var[grepl("_dmspols", df$var)] <- "DMSP-OLS"

df$country_sat <- paste0(df$country, ", ", df$sat_var)

#df$cora <- df$cor %>% round_bin()

#df_col <- df %>%
 # group_by(country_sat, firm_var, cor) %>%
 # mutate(N = n())

# Figures ----------------------------------------------------------------------
p_firms <- df %>%
  filter(firm_var %in% "N Firms") %>%
  ggplot() +
  geom_histogram(aes(x = cor), fill = "dodgerblue3", color = "black", bins = 21) + 
  labs(x = "Correlation", 
       y = "N", 
       title = "N Firms") +
  scale_fill_manual(values = c("darkorange1", "dodgerblue3"),
                    labels = c("No", "Yes")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  facet_wrap(~country_sat, 
             scale = "free_y")

p_employ <- df %>%
  filter(firm_var %in% "Employment") %>%
  ggplot() +
  geom_histogram(aes(x = cor), fill = "dodgerblue3", color = "black", bins = 21) + 
  labs(x = "Correlation", 
       y = "N", 
       title = "Employment") +
  scale_fill_manual(values = c("darkorange1", "dodgerblue3"),
                    labels = c("No", "Yes")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  facet_wrap(~country_sat, 
             scale = "free_y")

p <- ggarrange(p_firms,
               p_employ,
               nrow = 2)
ggsave(p, filename = file.path(figures_file_path, "within_unit_cor_hist.png"), height = 7, width = 9)


