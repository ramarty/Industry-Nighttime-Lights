# Merge Polygon Datasets

## Root dataset names
grid_files <- list.files(file.path(data_file_path, "Grid", "FinalData", "merged_datasets"), pattern = "*.Rds") %>%
  str_replace_all(".Rds", "")
grid_files <- grid_files[!grepl("clean", grid_files)]

gadm_files <- list.files(file.path(data_file_path, "GADM", "FinalData", "merged_datasets"), pattern = "*.rds") %>%
  str_replace_all(".rds", "")
gadm_files <- gadm_files[!grepl("clean", gadm_files)]

for(dataset in grid_files){
  
  print(paste(dataset, "-----------------------------------------------------"))
  
  ## Filepaths for (1) raw data (2) individual files and (3) merged files
  if(grepl("hex", dataset)){
    MERGED_DATA_PATH <- file.path(data_file_path, "Grid", "FinalData", "merged_datasets")
  }
  
  if(grepl("gadm", dataset)){
    MERGED_DATA_PATH <- file.path(data_file_path, "GADM", "FinalData", "merged_datasets")
  }
  
  # Load Data ------------------------------------------------------------------
  data <- readRDS(file.path(MERGED_DATA_PATH, paste0(dataset, ".Rds")))
  
  #### Remove unneeded variables
  data$group <- NULL
  data$year_sum_all <- NULL
  data$year_mean_all <- NULL
  
  # Transform Variables --------------------------------------------------------
  #### Transform
  variables <- names(data)[grepl("dmsp|firms|employment", names(data))]
  
  for(var in variables){
    data[[paste0(var, "_log")]] <- log(data[[var]] + 1)
    data[[paste0(var, "_g5")]] <- data[[var]] %>% cut2(g = 5) %>% as.numeric()
    data[[paste0(var, "_g10")]] <- data[[var]] %>% cut2(g = 10) %>% as.numeric()
    data[[paste0(var, "_g25")]] <- data[[var]] %>% cut2(g = 25) %>% as.numeric()
    data[[paste0(var, "_g50")]] <- data[[var]] %>% cut2(g = 50) %>% as.numeric()
  }
  

  #### Add differences  
  merge_vars <- c("id", "year")
  
  data_diffs_list <- lapply(1:6, function(i){
    
    calc_diffi <- function(var) var - lag(var, i)
    
    data_diffi <- data %>%
      arrange(year) %>%
      group_by(id) %>%
      mutate_at(vars(-id, -year), calc_diffi) %>%
      ungroup() %>%
      dplyr::select(-lat, -lon)
    
    names(data_diffi)[!(names(data_diffi) %in% merge_vars)] <- 
      paste0(names(data_diffi)[!(names(data_diffi) %in% merge_vars)], "_diff", i)

    return(data_diffi)
  })
  
  data_diffs_df <- Reduce(function(df1, df2) merge(df1, df2, by = merge_vars), data_diffs_list)
  
  data <- merge(data, data_diffs_df, by=merge_vars)
  
  #### Add other variables
  data$time <- data$year %>% as.factor() %>% as.numeric()
  data$unit <- dataset %>% str_replace_all(".Rds", "")

  # Export ---------------------------------------------------------------------
  saveRDS(data, file.path(MERGED_DATA_PATH, paste0(dataset, "_clean.Rds")))
}





