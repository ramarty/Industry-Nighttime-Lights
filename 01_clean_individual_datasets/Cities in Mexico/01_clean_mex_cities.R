# Clean Mexico Cities

# Load Data --------------------------------------------------------------------
mex_cities <- readOGR(file.path(data_file_path, "Cities in Mexico", "RawData", "LocalidadUrbana.shp"))
mex_cities <- mex_cities %>% spTransform(CRS("+init=epsg:4326"))

# Cleanup ----------------------------------------------------------------------

mex_cities$OBJECTID <- mex_cities$OBJECTID %>% as.character() %>% as.numeric()

## Make unique city name
mex_cities$city_name <- paste0(mex_cities$NOM, " - ",
                               mex_cities$NOMENT, " - ",
                               mex_cities$OBJECTID)

## Only keep select variables
mex_cities@data <- mex_cities@data %>%
  dplyr::select(OBJECTID, city_name) %>%
  dplyr::rename(city_uid = OBJECTID)

mex_cities <- spTransform(mex_cities, 
                          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Export -----------------------------------------------------------------------
saveRDS(mex_cities, file.path(data_file_path, "Cities in Mexico", "FinalData",
                              "mex_cities.Rds"))

## Checks
mex_cities$city_uid %>% table %>% table
mex_cities$city_name %>% table %>% table

mex_cities$city_uid %>% is.na %>% table()
mex_cities$city_name %>% is.na %>% table()

