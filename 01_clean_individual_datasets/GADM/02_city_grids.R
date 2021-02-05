# Clean Firm Data

# Load Data --------------------------------------------------------------------
can <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_CAN_1_sp.rds"))
mex <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_MEX_2_sp.rds"))

MEX_mexico_city <- mex[mex$NAME_1 %in% "Distrito Federal",]

leaflet() %>%
  addTiles() %>%
  addPolygons(data = mex,
              popup = ~NAME_2)






firms_df <- readRDS(file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))


gadm_chull <- gConvexHull(gadm)
gadm_chull$id <- 1
gadm_chull <- gBuffer(gadm_chull, byid=T, width = 2/111.12)

# Export -----------------------------------------------------------------------
writeOGR(obj = gadm_chull,
         dsn = file.path(data_file_path, "GADM", "canada_chull"),
         layer = "canada_chull",
         driver = "ESRI Shapefile")


