# Merge Polygon Datasets

print("02 MERGE DATASETS")

for(country in c("canada", "mexico")){
  for(dataset_type in c("hex_5km",
                        "hex_10km",
                        "hex_25km",
                        "hex_50km",
                        "hex_100km",
                        "hex_250km",
                        "hex_500km",
                        "hex_1000km",
                        "citygriddmsp",
                        "citygridviirs",
                        "city")){
    
    print(paste(country, dataset_type, "-------------------------------------"))
    
    #### Filepaths for (1) raw data (2) individual files and (3) merged files
    RAW_DATA_PATH <- file.path(data_file_path, "Grid", "RawData", 
                               paste0(substring(country,1,3),"_", dataset_type,".Rds"))
    IND_DATA_PATH <- file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets")
    MERGED_DATA_PATH <- file.path(data_file_path, "Grid", "FinalData", country, "merged_datasets")
    
    ## Coordinates
    raw_data <- readRDS(RAW_DATA_PATH)
    coords_data <- raw_data %>%
      coordinates() %>%
      as.data.frame() %>%
      dplyr::rename(lon = V1,
                    lat = V2) 
    coords_data$id <- raw_data$id
    
    ## List of datasets associated with root dataset
    data_files <- list.files(IND_DATA_PATH, pattern=paste0(dataset_type,"_"), full.names = T) %>%
      stri_subset_fixed("_cityinfo", negate = TRUE) %>% # doesn't contain "year"; merge in later
      stri_subset_fixed("viirs.Rds", negate = TRUE) %>% # DELETE THESE DATASETS
      stri_subset_fixed("dmspolselvidge_.Rds", negate = TRUE) %>% # DELETE THESE DATASETS
      stri_subset_fixed("dmspolsharmon_.Rds", negate = TRUE) %>% # DELETE THESE DATASETS
      stri_subset_fixed("dmspolszhang_.Rds", negate = TRUE) %>% # DELETE THESE DATASETS
      lapply(readRDS)
    
    ## Merge datasets
    # Reduce allows merging a list of datasets together
    data_merged <- Reduce(function(x, y) merge(x, y, by = c("id", "year"), all=TRUE), data_files)

    # Cleanup vars
    for(i in 1:10) data_merged$group   <- NULL
    for(i in 1:10) data_merged$group.y <- NULL
    for(i in 1:10) data_merged$group.x <- NULL
    
    # Add city info
    if(grepl("city", dataset_type)){
      city_info_df <- readRDS(file.path(IND_DATA_PATH, paste0(substring(country,1,3),
                                                              "_",
                                                              dataset_type,
                                                              "_cityinfo.Rds")))
      
      data_merged <- merge(data_merged, city_info_df, by = "id")
    }
  
    # Add coordinates
    data_merged <- merge(data_merged, coords_data, by="id")
    
    # Cleanup vars
    # TODO: prevent this issue
    #for(i in 1:10) data_merged$group   <- NULL
    #for(i in 1:10) data_merged$group.y <- NULL
    #for(i in 1:10) data_merged$group.x <- NULL
    
    # For citydmsp/viirs, subset by years
    #if(dataset_type %in% "citygridviirs") data_merged <- data_merged[data_merged$year >= 2012,]
    #if(dataset_type %in% "citygriddmsp")  data_merged <- data_merged[data_merged$year <= 2013,]
    
    ## Subset
    # Only keep cells that had a firm at some point
    data_merged <- data_merged %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(firms_positive_anyyear   = max(N_firms_sum_all, na.rm=T) > 0) %>%
      dplyr::ungroup() #%>%
      #dplyr::filter(firms_positive_anyyear %in% T)
    # Don't filter here, as want all grids for the city figures; we filter in 
    # the 04_clean_datasets.R script
    
    ## Export
    saveRDS(data_merged, file.path(MERGED_DATA_PATH, paste0(dataset_type, ".Rds")))
    
    # If Mexico, save as two separate files: DMSPOLS period and VIIRS period. Do 
    # this because years between data is different in two time periods (so affects
    # differencing)
    if(country %in% "mexico"){
      data_merged_dmspols <- data_merged[data_merged$year <= 2014,]
      data_merged_viirs   <- data_merged[data_merged$year >= 2014,]
      
      saveRDS(data_merged_dmspols, file.path(MERGED_DATA_PATH, paste0(dataset_type, "_dmspols.Rds")))
      saveRDS(data_merged_viirs,   file.path(MERGED_DATA_PATH, paste0(dataset_type, "_viirs.Rds")))
    }
    
    
  }
}




