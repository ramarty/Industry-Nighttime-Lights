# Analysis

set.seed(42)

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

can_grid <- readRDS(file.path(data_file_path, "Grid", "RawData", "can_hex_5km.Rds"))
mex_grid <- readRDS(file.path(data_file_path, "Grid", "RawData", "mex_hex_5km.Rds"))

# Correlation Dataframe --------------------------------------------------------
mex_viirs$employment_sum_all[mex_viirs$year %in% c(2017, 2018, 2020)] <- 
  mex_viirs$empl_med_sum_all[mex_viirs$year %in% c(2017, 2018, 2020)]

mex_dmspols_df <- mex_dmspols %>%
  group_by(id) %>%
  dplyr::summarise(cor_ntl_firms  = cor(dmspolselvidge_mean_log, N_firms_sum_all_log),
                   cor_ntl_employ = cor(dmspolszhang_mean_log,   employment_sum_all_log),
                   N_firms_sum_all_max = max(N_firms_sum_all),
                   employment_sum_all_max = max(employment_sum_all)) 

mex_viirs_df <- mex_viirs %>%
  group_by(id) %>%
  dplyr::summarise(cor_ntl_firms  = cor(viirs_mean_log, N_firms_sum_all_log),
                   cor_ntl_employ = cor(viirs_mean_log,   employment_sum_all_log),
                   N_firms_sum_all_max = max(N_firms_sum_all),
                   employment_sum_all_max = max(employment_sum_all)) 

can_dmspols_df <- can %>%
  group_by(id) %>%
  dplyr::summarise(cor_ntl_firms  = cor(dmspolselvidge_mean_log, N_firms_sum_all_log),
                   cor_ntl_employ = cor(dmspolszhang_mean_log,   employment_sum_all_log),
                   N_firms_sum_all_max = max(N_firms_sum_all),
                   employment_sum_all_max = max(employment_sum_all)) 

# Merge with Grid --------------------------------------------------------------
mex_grid@data <- merge(mex_grid@data, mex_dmspols_df, by="id", all.x=T)
mex_grid <- mex_grid[!is.na(mex_grid$cor_ntl_employ),]

mex_grid$id <- row.names(mex_grid)
mex_grid_tidy <- tidy(mex_grid)
mex_grid_tidy <- merge(mex_grid_tidy, mex_grid, by = "id")

ggplot() +
  geom_polygon(data = mex_grid_tidy,
               aes(x = long, y = lat, group = group,
                   fill = cor_ntl_employ),
               color = NA) +
  geom_polygon(data = mex_grid_tidy[mex_grid_tidy$cor_ntl_employ >= 0.8,],
               aes(x = long, y = lat, group = group),
               color = NA,
               fill = "red") +
  theme_void() +
  scale_fill_viridis()


