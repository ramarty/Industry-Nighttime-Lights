# Merge Polygon Datasets

print("02 MERGE DATASETS")

for(country in c("mexico", "canada")){
  for(dataset_type in c("hex_5km", 
                        "hex_10km",
                        "hex_25km",
                        "hex_50km",
                        "hex_100km",
                        "hex_250km",
                        "hex_500km",
                        "hex_1000km")){
    
    print(paste(country, dataset_type, "-------------------------------------"))
    
    #### Filepaths for (1) raw data (2) individual files and (3) merged files
    if(grepl("hex", dataset_type)){
      RAW_DATA_PATH <- file.path(data_file_path, "Grid", "RawData", 
                                 paste0(substring(country,1,3),"_", dataset_type,".Rds"))
      IND_DATA_PATH <- file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets")
      MERGED_DATA_PATH <- file.path(data_file_path, "Grid", "FinalData", country, "merged_datasets")
    }
    
    if(grepl("gadm", dataset_type)){
      RAW_DATA_PATH <- file.path(data_file_path, "GADM", "RawData")
      IND_DATA_PATH <- file.path(data_file_path, "GADM", "FinalData", country, "individual_datasets")
      MERGED_DATA_PATH <- file.path(data_file_path, "GADM", "FinalData", country, "merged_datasets")
    }
    
    ## Coordinates
    raw_data <- readRDS(RAW_DATA_PATH)
    coords_data <- raw_data %>%
      coordinates() %>%
      as.data.frame() %>%
      dplyr::rename(lon = V1,
                    lat = V2) 
    coords_data$id <- raw_data$id
    
    ## List of datasets associated with root dataset
    data_files <- list.files(IND_DATA_PATH, pattern=dataset_type, full.names = T) %>%
      lapply(readRDS)
    
    ## Merge datasets
    # Reduce allows merging a list of datasets together
    data_merged <- Reduce(function(x, y) merge(x, y, by = c("id", "year"), all=TRUE), data_files)
    
    # Add coordinates
    data_merged <- merge(data_merged, coords_data, by="id")
    
    ## Export
    saveRDS(data_merged, file.path(MERGED_DATA_PATH, paste0(dataset_type, ".Rds")))
    
    # If Mexico, save as two separate files: DMSPOLS period and VIIRS period. Do 
    # this because years between data is different in two time periods.
    if(country %in% "mexico"){
      data_merged_dmspols <- data_merged[data_merged$year <= 2014,]
      data_merged_viirs   <- data_merged[data_merged$year >= 2014,]
      
      saveRDS(data_merged_dmspols, file.path(MERGED_DATA_PATH, paste0(dataset_type, "_dmspols.Rds")))
      saveRDS(data_merged_viirs,   file.path(MERGED_DATA_PATH, paste0(dataset_type, "_viirs.Rds")))
    }
    
  }
}




