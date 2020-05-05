# Merge Polygon Datasets

## Root dataset names
grid_files <- list.files(file.path(data_file_path, "Grid", "RawData"), pattern = "*.Rds") %>%
  str_replace_all(".Rds", "")

gadm_files <- list.files(file.path(data_file_path, "GADM", "RawData"), pattern = "*.rds") %>%
  str_replace_all(".rds", "")

for(dataset in grid_files){
  
  print(paste(dataset, "-----------------------------------------------------"))
  
  ## Filepaths for (1) raw data (2) individual files and (3) merged files
  if(grepl("hex", dataset)){
    RAW_DATA_PATH <- file.path(data_file_path, "Grid", "RawData")
    IND_DATA_PATH <- file.path(data_file_path, "Grid", "FinalData", "individual_datasets")
    MERGED_DATA_PATH <- file.path(data_file_path, "Grid", "FinalData", "merged_datasets")
  }
  
  if(grepl("gadm", dataset)){
    RAW_DATA_PATH <- file.path(data_file_path, "GADM", "RawData")
    IND_DATA_PATH <- file.path(data_file_path, "GADM", "FinalData", "individual_datasets")
    MERGED_DATA_PATH <- file.path(data_file_path, "GADM", "FinalData", "merged_datasets")
  }
  
  ## Raw data coordinates
  raw_data <- readRDS(file.path(RAW_DATA_PATH, paste0(dataset,".Rds")))
  #raw_data <- spTransform(raw_data, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  coords_data <- raw_data %>%
    coordinates() %>%
    as.data.frame() %>%
    dplyr::rename(lon = V1,
                  lat = V2) 
  coords_data$id <- raw_data$id
  
  ## List of datasets associated with root dataset
  # eg, dataet for firms_all, dmspols, etc
  data_files <- list.files(IND_DATA_PATH, pattern=paste0("^", dataset), full.names = T) %>%
    lapply(readRDS)
  
  ## Merge datasets
  # Reduce allows merging a list of datasets together
  data_merged <- Reduce(function(x, y) merge(x, y, by = c("id", "year"), all=TRUE), data_files)
  
  # Add coordinates
  data_merged <- merge(data_merged, coords_data, by="id")
  
  ## Export
  saveRDS(data_merged, file.path(MERGED_DATA_PATH, paste0(dataset, ".Rds")))
}
