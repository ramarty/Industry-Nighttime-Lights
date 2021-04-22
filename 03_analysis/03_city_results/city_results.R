# Analysis

# Load/Prep Data ---------------------------------------------------------------
country_name <- "canada"
viirs_df <- readRDS(file.path(data_file_path, "Grid", "FinalData", country_name, "merged_datasets", "citygridviirs.Rds"))
dmsp_df  <- readRDS(file.path(data_file_path, "Grid", "FinalData", country_name, "merged_datasets", "citygriddmsp.Rds"))

viirs_df <- viirs_df %>%
  dplyr::mutate_at(vars(N_firms_sum_all,
                        employment_sum_all), replace_na, 0) 

dmsp_df <- dmsp_df %>%
  dplyr::mutate_at(vars(N_firms_sum_all,
                        employment_sum_all), replace_na, 0) 

if(country_name %in% "canada"){
  viirs_df <- viirs_df[viirs_df$year %in% 2013,]
  dmsp_df  <- dmsp_df[dmsp_df$year %in% 2013,]
} else{
  viirs_df <- viirs_df[viirs_df$year %in% 2014,]
  dmsp_df  <- dmsp_df[dmsp_df$year %in% 2014,]
}

a <- viirs_df %>%
  filter(N_firms_sum_all > 0,
         !is.na(viirs_mean)) %>% 
  dplyr::group_by(city_name) %>%
  dplyr::summarise(cor_firms = cor(N_firms_sum_all, viirs_mean),
                   cor_employ = cor(employment_sum_all, viirs_mean),
                   viirs_mean_sd = sd(viirs_mean),
                   N = n()) %>%
  filter(N > 1)





