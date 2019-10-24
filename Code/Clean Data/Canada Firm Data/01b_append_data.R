# Append Canada Firm Data

# Load Data --------------------------------------------------------------------
list.files(file.path(raw_data_file_path, "Canada Industry Data"), 
           pattern = "*.dta",
           full.names = TRUE) %>%
  lapply(read_haven) %>%
  bind_rows 


#%>%
  write.csv(file.path(final_data_file_path, "Canada Industry Data", "coordinates_appended.csv"), row.names=F)
