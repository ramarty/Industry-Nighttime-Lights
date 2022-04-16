# Identify Clusters of Lights

for(country in c("mexico", "canada")){
  
  clumps_sp <- readRDS(file.path(data_file_path, "DMSPOLS_Clusters", "RawData",
                                 paste0(country %>% substring(1,3) %>% paste0("_dmspcluster.Rds"))))
  
  # Group together close together clusters ---------------------------------------
  
  ## Centroid
  points_sp <- coordinates(clumps_sp) %>%
    as.data.frame() %>%
    dplyr::rename(lon = V1,
                  lat = V2) %>%
    bind_cols(clumps_sp@data)
  
  ## Spatially Define and project
  coordinates(points_sp) <- ~lon+lat
  crs(points_sp) <- CRS("+init=epsg:4326")
  points_sp <- spTransform(points_sp, CRS(UTM_ETH))
  
  ## Back to dataframe
  points <- as.data.frame(points_sp)
  
  ## Clusters
  points_dist <- points[,c("lat", "lon")] %>% dist()
  clumps_sp$wardheirch_clust_id <- hclust(points_dist, method = "ward.D2") %>%
    cutree(h = 10000)
  
  clumps_sp <- raster::aggregate(clumps_sp, by = "wardheirch_clust_id", sums=list(list(sum, 'cluster_n_cells')))
  
  clumps_sp@data <- clumps_sp@data %>%
    dplyr::select(-c(wardheirch_clust_id)) %>% 
    dplyr::mutate(cell_id = 1:n()) # prevous cell_id summed version; fresh, aggregated version
  
  # Export -----------------------------------------------------------------------
  # We save "polygon" and "points" file, where "points" is actually just the polygon.
  # We do this to make compatible with some scripts that also process grid data
  
  # TODO: Should we just export to GRID folder? may make life easier?
  saveRDS(clumps_sp, file.path(data_file_path, "DMSPOLS_Clusters", "RawData",
                               paste0(country %>% substring(1,3) %>% paste0("_dmspclustergrouped.Rds"))))
}




