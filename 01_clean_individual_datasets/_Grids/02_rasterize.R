# Rasterize Hexagons

res <- 0.75/111.12

file_names <- file.path(data_file_path, "Grid", "RawData") %>%
  list.files(pattern = "*.Rds") %>%
  stri_subset_fixed("raster", negate = TRUE) %>% # exclude _raster.Rds
  stri_subset_fixed("city", negate = TRUE) # exclude city level; use poygons for those

for(file_i in file_names){
  print(file_i)
  
  ## Load Polygon
  polygon <- readRDS(file.path(data_file_path, "Grid", "RawData", file_i))
  polygon <- polygon[1:5,]
  polygon <- spTransform(polygon, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  ## Make Blank Raster
  ext <- extent(polygon)
  
  x <- floor((ext@xmax - ext@xmin) / res)
  y <- floor((ext@ymax - ext@ymin) / res)
  
  mat <- matrix(0, y, x)
  vx <- velox(mat, extent=ext, res=c(res,res), crs="+proj=longlat +datum=WGS84 +no_defs")
  
  ## Rasterize
  vx$rasterize(spdf=polygon, field="id", background=-1)
  
  ## To Raster Object
  ras <- vx$as.RasterLayer(assign_data_type = TRUE)
  
  ## Export
  writeRaster(ras, file.path(data_file_path, "Grid", "RawData", file_i %>% str_replace_all(".Rds", ".tif")))
  saveRDS(ras, file.path(data_file_path, "Grid", "RawData", file_i %>% str_replace_all(".Rds", "_raster.Rds")))
}


