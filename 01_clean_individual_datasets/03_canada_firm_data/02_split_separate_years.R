# Clean Firm Data

# Load Data --------------------------------------------------------------------
firmdata_df <- readRDS(file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))

for(year in FIRM_YEARS_CANADA){
  print(year)
  firm_i <- firmdata_df[firmdata_df$year %in% year,]
  saveRDS(firm_i, file.path(data_file_path, "Canada Industry Data", "FinalData", paste0("firms_",year,".Rds")))
}



