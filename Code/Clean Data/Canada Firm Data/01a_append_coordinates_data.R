# Append Canada Firm Data

# Load Data --------------------------------------------------------------------
list.files(file.path(raw_data_file_path, "Canada Industry Data"), 
           pattern = "*.csv",
           full.names = TRUE) %>%
  lapply(read.csv) %>%
  bind_rows %>%
  saveRDS(file.path(final_data_file_path, "Canada Industry Data", "coordinates_appended.Rds"))
