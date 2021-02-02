# Clean Firm Data

# Load Data --------------------------------------------------------------------
firms_df <- readRDS(file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms.Rds"))

# Restrict Variables to Only Needed Ones ---------------------------------------
# Large dataset, so helps with efficiency

firms_df@data <- firms_df@data %>%
  dplyr::rename(employment = empl) %>%
  dplyr::select(year, employment, dmspols, naics2, naicsname) 

# Aggregate naics2 categories --------------------------------------------------

# Export Data ------------------------------------------------------------------
saveRDS(firms_df, file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_clean.Rds"))
