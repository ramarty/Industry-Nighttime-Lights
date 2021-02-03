# Create Hexagons

# Create hexagons around all areas with lights or firms. Creates hexagons of 
# different sizes. The main challenge the code deals with is making hexagons
# across a large extent of Canada. To addess, it

# (1) Creates simplified area of where firms/light are
#   (a) Grabs coordinates of all locations with light and firms
#   (b) Rounds coordinates to the nearest 1km
#   (c) Buffers coordinates by 5km
# (2) First creates hexagons with 1000km buffer for Canada
# (3) Subset hexagon to those that intersect with above layer (firm+light layer)
# (4) Creates hexagons with 500km buffer for above layer
# (5) Repeat -  this ensures we only create hexagons around areas where firms/light
#     are, and we use the larger hexagons to minimize initial number of hexagons
#     made originally. 

# Coordiantes with Firms / NTL -------------------------------------------------
#### Firms
firms <- readRDS(file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))
firms_coords <- firms %>% coordinates() %>% 
  as.data.frame %>% 
  distinct()

#### DMSP-OLS
dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "canada_dmspols_2013.tif"))
dmspols_df <- dmspols %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  dplyr::rename(value = canada_dmspols_2013,
                lon = x,
                lat = y) 

dmspols_df <- dmspols_df[dmspols_df$value > 0,] %>%
  dplyr::select(lon, lat)

#### Coordinates
coords_df <- bind_rows(firms_coords, dmspols_df) %>%
  mutate(id = 1:n())

coordinates(coords_df) <- ~lon+lat
crs(coords_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
coords_df <- spTransform(coords_df, CRS(PROJ_canada))

## Reduce number of coordinates
# Round to nearest 1km and buffer
coords_df_coords <- coords_df %>% coordinates() %>% as.data.frame()
coords_df_coords$lon <- coords_df_coords$lon %>% round(-4)
coords_df_coords$lat <- coords_df_coords$lat %>% round(-4)
coords_df_coords_dnct <- coords_df_coords %>% 
  distinct() %>%
  mutate(id = 1)

coordinates(coords_df_coords_dnct) <- ~lon+lat
crs(coords_df_coords_dnct) <- CRS(PROJ_canada)
coords_buff <- gBuffer(coords_df_coords_dnct, width=5*1000, capStyle = "SQUARE", byid = T)
coords_buff_agg <- raster::aggregate(coords_buff, by="id")

coords_buff_agg_sf <- st_as_sf(coords_buff_agg)

# Make Hexagons ----------------------------------------------------------------
gadm <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_CAN_0_sp.rds"))
gadm <- gSimplify(gadm, tol = .1)
gadm <- spTransform(gadm, CRS(PROJ_canada))


hex_1000 <- create_hexagons(gadm,     1000*1000, T, coords_buff_agg_sf)
hex_500  <- create_hexagons(hex_1000, 500*1000,  T, coords_buff_agg_sf)
hex_250  <- create_hexagons(hex_500,  250*1000,  T, coords_buff_agg_sf)
hex_100  <- create_hexagons(hex_250,  100*1000,  T, coords_buff_agg_sf)
hex_50   <- create_hexagons(hex_100,  50*1000,   T, coords_buff_agg_sf)
hex_25   <- create_hexagons(hex_50,   25*1000,   T, coords_buff_agg_sf)
hex_10   <- create_hexagons(hex_25,   10*1000,   T, coords_buff_agg_sf)
hex_5    <- create_hexagons(hex_10,   5*1000,    T, coords_buff_agg_sf)
saveRDS(hex_5,    file.path(data_file_path, "Grid", "hex_5km.Rds"))

#hex_1    <- create_hexagons(hex_5,    1*1000,    F, coords_buff_agg_sf)
#saveRDS(hex_1,    file.path(data_file_path, "Grid", "hex_1km.Rds"))

# Export -----------------------------------------------------------------------
saveRDS(hex_1000, file.path(data_file_path, "Grid", "RawData","can_hex_1000km.Rds"))
saveRDS(hex_500,  file.path(data_file_path, "Grid", "RawData","can_hex_500km.Rds"))
saveRDS(hex_250,  file.path(data_file_path, "Grid", "RawData","can_hex_250km.Rds"))
saveRDS(hex_100,  file.path(data_file_path, "Grid", "RawData","can_hex_100km.Rds"))
saveRDS(hex_50,   file.path(data_file_path, "Grid", "RawData","can_hex_50km.Rds"))
saveRDS(hex_25,   file.path(data_file_path, "Grid", "RawData","can_hex_25km.Rds"))
saveRDS(hex_10,   file.path(data_file_path, "Grid", "RawData","can_hex_10km.Rds"))
saveRDS(hex_5,    file.path(data_file_path, "Grid", "RawData","can_hex_5km.Rds"))
#saveRDS(hex_1,    file.path(data_file_path, "Grid", "RawData","can_hex_1km.Rds"))
