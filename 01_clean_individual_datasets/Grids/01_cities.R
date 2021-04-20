# Create Grids within Cities

# Load Cities ------------------------------------------------------------------
can_cities <- readRDS(file.path(data_file_path, "Cities in Canada", "FinalData", "can_cities.Rds"))
mex_cities <- readRDS(file.path(data_file_path, "Cities in Mexico", "FinalData", "mex_cities.Rds"))

# Cleanup ----------------------------------------------------------------------
can_cities@data <- can_cities@data %>%
  dplyr::rename(id = city_uid)

mex_cities@data <- mex_cities@data %>%
  dplyr::rename(id = city_uid)

# Export -----------------------------------------------------------------------
## RawData
saveRDS(can_cities[,"id"], file.path(data_file_path, "Grid", "RawData","can_city.Rds"))
saveRDS(mex_cities[,"id"], file.path(data_file_path, "Grid", "RawData","mex_city.Rds"))

## With City Info
saveRDS(can_cities, file.path(data_file_path, "Grid", "FinalData", "canada", "individual_datasets", "can_city_cityinfo.Rds"))
saveRDS(mex_cities, file.path(data_file_path, "Grid", "FinalData", "mexico", "individual_datasets", "mex_city_cityinfo.Rds"))




