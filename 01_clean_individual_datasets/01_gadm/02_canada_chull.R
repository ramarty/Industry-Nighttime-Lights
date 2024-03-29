# Clean Firm Data

# Load Data --------------------------------------------------------------------
firms_df <- readRDS(file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))

gadm <- readRDS(file.path(data_file_path, "GADM", "FinalData", "blank_files", "gadm36_CAN_0_sp.rds"))

gadm_chull <- gConvexHull(gadm)
gadm_chull$id <- 1
gadm_chull <- gBuffer(gadm_chull, byid=T, width = 2/111.12)

# Export -----------------------------------------------------------------------
writeOGR(obj = gadm_chull,
         dsn = file.path(data_file_path, "GADM", "FinalData", "canada_chull"),
         layer = "canada_chull",
         driver = "ESRI Shapefile",
         overwrite_layer = T,
         delete_dsn = T)

saveRDS(gadm_chull, file.path(data_file_path, "GADM", "FinalData", "canada_chull", "canada_chull.Rds"))
