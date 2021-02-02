# Append Canada Firm Data

# Load, Append and Merge Data --------------------------------------------------
# Load and Append Firm Data
firmdata_df <- list.files(file.path(data_file_path, "Canada Industry Data", "RawData", "Firm Data"), 
                          pattern = "*.csv",
                          full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows

# Export -----------------------------------------------------------------------
#firmdata_df <- as.data.frame(firmdata_df)
saveRDS(firmdata_df, file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))

