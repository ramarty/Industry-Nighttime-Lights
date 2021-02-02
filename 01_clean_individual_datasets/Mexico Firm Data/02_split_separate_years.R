# Clean Firm Data

# Load Data --------------------------------------------------------------------
firmdata_04_14_df <- readRDS(file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_04_14.Rds"))
firmdata_17_20_df <- readRDS(file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_17_20.Rds"))

naics2 <- c(unique(firmdata_04_14_df$naics2), unique(firmdata_17_20_df$naics2)) %>% unique()
saveRDS(naics2, file.path(data_file_path, "Mexico Industry Data", "FinalData", "naics2_types.Rds"))

for(year in c(2004, 2009, 2014)){
  firm_i <- firmdata_04_14_df[firmdata_04_14_df$year %in% year,]
  saveRDS(firm_i, file.path(data_file_path, "Mexico Industry Data", "FinalData", paste0("firms_",year,".Rds")))
}

for(year in c(2017, 2018, 2019, 2020)){
  firm_i <- firmdata_17_20_df[firmdata_17_20_df$year %in% year,]
  saveRDS(firm_i, file.path(data_file_path, "Mexico Industry Data", "FinalData", paste0("firms_",year,".Rds")))
}


