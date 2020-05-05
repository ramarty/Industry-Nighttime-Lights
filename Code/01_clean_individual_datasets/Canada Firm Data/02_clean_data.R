# Clean Firm Data

# Load Data --------------------------------------------------------------------
firms_df <- readRDS(file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))

# Restrict Variables to Only Needed Ones ---------------------------------------
# Large dataset, so helps with efficiency

firms_df@data <- firms_df@data %>%
  dplyr::select(year, employment, dmspols)

# Export Data ------------------------------------------------------------------
saveRDS(firms_df, file.path(data_file_path, "Canada Industry Data", "FinalData", "firms_clean.Rds"))
