# Append Canada Firm Data

# Load Data --------------------------------------------------------------------
coords_df <- readRDS(file.path(final_data_file_path, "Canada Industry Data", "coordinates_appended.Rds"))
coordinates(coords_df) <- ~lon+lat
