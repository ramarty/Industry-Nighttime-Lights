# Identify Clusters of Lights

# TODO: Speed up with mclapply

for(country in c("mexico", "canada")){
  
  NTL_PATH <- file.path(data_file_path, "Nighttime Lights", "DMSPOLS")
  
  # Load Data --------------------------------------------------------------------
  dmspols_1996 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_1996.tif")))
  dmspols_1997 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_1997.tif")))
  dmspols_1998 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_1998.tif")))
  dmspols_1999 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_1999.tif")))
  dmspols_2000 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2000.tif")))
  dmspols_2001 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2001.tif")))
  dmspols_2002 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2002.tif")))
  dmspols_2003 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2003.tif")))
  dmspols_2004 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2004.tif")))
  dmspols_2005 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2005.tif")))
  dmspols_2006 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2006.tif")))
  dmspols_2007 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2007.tif")))
  dmspols_2008 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2008.tif")))
  dmspols_2009 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2009.tif")))
  dmspols_2010 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2010.tif")))
  dmspols_2011 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2011.tif")))
  dmspols_2012 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2012.tif")))
  dmspols_2013 <- raster(file.path(NTL_PATH, paste0(country, "_dmspols_2013.tif")))
  
  # Define raster layer of clusters ----------------------------------------------
  dmspols_2013_binary <- dmspols_2013
  dmspols_2013_binary[] <- as.numeric(dmspols_1996[] >= 1 |
                                        dmspols_1997[] >= 1 |
                                        dmspols_1998[] >= 1 |
                                        dmspols_1999[] >= 1 |
                                        dmspols_2000[] >= 1 |
                                        dmspols_2001[] >= 1 |
                                        dmspols_2002[] >= 1 |
                                        dmspols_2003[] >= 1 |
                                        dmspols_2004[] >= 1 |
                                        dmspols_2005[] >= 1 |
                                        dmspols_2006[] >= 1 |
                                        dmspols_2007[] >= 1 |
                                        dmspols_2008[] >= 1 |
                                        dmspols_2009[] >= 1 |
                                        dmspols_2010[] >= 1 |
                                        dmspols_2011[] >= 1 |
                                        dmspols_2012[] >= 1 |
                                        dmspols_2013[] >= 1)
  dmspols_2013_clumps <- clump(dmspols_2013_binary, directions=8)
  
  clumps_unique_values <- unique(dmspols_2013_clumps[])[!is.na(unique(dmspols_2013_clumps[]))]
  
  # Polygonize clusters ----------------------------------------------------------
  clumps_sp <- lapply(clumps_unique_values, function(clump_i){
    print(paste(country, "-", clump_i, "/", length(clumps_unique_values)))
    clump_i_sp <- rasterToPolygons(dmspols_2013_clumps, 
                                   fun=function(x){x==clump_i}, 
                                   n=4, na.rm=TRUE, 
                                   digits=12, 
                                   dissolve=F)
    clump_i_sp$cell_id <- clump_i
    clump_i_sp$cluster_n_cells <- nrow(clump_i_sp)
    return(clump_i_sp)
  }) %>% do.call(what="rbind")
  
  clumps_sp <- raster::aggregate(clumps_sp, 
                                 by="cell_id",
                                 list(list(mean, 'cluster_n_cells')))
  
  # Group together close together clusters ---------------------------------------
  saveRDS(clumps_sp, file.path(data_file_path, "DMSPOLS_Clusters", "RawData",
                               paste0(country %>% substring(1,3) %>% paste0("_dmspcluster.Rds"))))
}




