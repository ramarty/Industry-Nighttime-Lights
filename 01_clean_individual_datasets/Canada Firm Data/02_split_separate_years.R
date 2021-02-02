# Clean Firm Data

# Load Data --------------------------------------------------------------------
firmdata_df <- readRDS(file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))

naics2 <- firmdata_df$naics2 %>% unique()

for(year in c(2001, 2003, 2005, 2007, 2009, 2011, 2013)){
  firm_i <- firmdata_df[firmdata_df$year %in% year,]
  saveRDS(firm_i, file.path(data_file_path, "Canada Industry Data", "FinalData", paste0("firms_",year,".Rds")))
}

saveRDS(naics2, file.path(data_file_path, "Canada Industry Data", "FinalData", "naics2_types.Rds"))



