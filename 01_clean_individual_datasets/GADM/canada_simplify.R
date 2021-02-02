# Simplify Canada Shapefile

hex_1000 <- readRDS(file.path(data_file_path, "Grid", "RawData","can_hex_1000km.Rds"))

hex_1000 <- spTransform(hex_1000, CRS("+init=epsg:4326"))

leaflet() %>%
  addTiles() %>%
  addPolygons(data = hex_1000)



# Filepaths --------------------------------------------------------------------
setwd(file.path(data_file_path, "GADM", "RawData"))

getData('GADM', country='CAN', level=0)
getData('GADM', country='CAN', level=1)
getData('GADM', country='CAN', level=2)
getData('GADM', country='CAN', level=3)

getData('GADM', country='MEX', level=0)
getData('GADM', country='MEX', level=1)
getData('GADM', country='MEX', level=2)
