# Merge Polygon Datasets

country <- "mexico"

for(dataset in paste0("hex_",c(5,10,25,50,100,250,500,1000), "km") ){
  
  print(paste(dataset, "-----------------------------------------------------"))
  
  ## Filepaths for (1) raw data (2) individual files and (3) merged files
  if(grepl("hex", dataset)){
    MERGED_DATA_PATH <- file.path(data_file_path, "Grid", "FinalData", country, "merged_datasets")
  }
  
  if(grepl("gadm", dataset)){
    MERGED_DATA_PATH <- file.path(data_file_path, "GADM", "FinalData", country, "merged_datasets")
  }
  
  # Load Data ------------------------------------------------------------------
  data <- readRDS(file.path(MERGED_DATA_PATH, paste0(dataset, ".Rds")))
  
  # Always NA or 0? Only include cells that have positive light or a firm
  # at some point
  data <- data %>%
    group_by(id) %>%
    mutate(firms_sum_all_SUM_ID = sum(firms_sum_all, na.rm=T),
           dmspol_sum_SUM_ID = sum(dmspol_sum, na.rm=T)) %>%
    ungroup()

  data <- data[data$dmspol_sum_SUM_ID > 0 & data$firms_sum_all_SUM_ID > 0,]
  data$firms_sum_all_SUM_ID <- NULL
  data$dmspol_sum_SUM_ID <- NULL
  
  #### Remove unneeded variables
  #data$group <- NULL
  #data$year_sum_all <- NULL
  #data$year_mean_all <- NULL
  
  gen_var_to_rm <- c("group")
  dmsp_var_to_rm <- names(data)[grepl("dmspols", names(data)) & grepl("_t", names(data))]
  naics2_var_to_rm <- names(data)[grepl("^naics2_", names(data))]
  firmsmean_var_to_rm <- names(data)[grepl("firms_mean", names(data))] # only need sum
  employmean_var_to_rm <- names(data)[grepl("employment_mean", names(data))] # only need sum (for now?)
  
  vars_to_rm <- c(gen_var_to_rm,
                 dmsp_var_to_rm,
                 naics2_var_to_rm,
                 firmsmean_var_to_rm,
                 employmean_var_to_rm)
  
  data <- data %>%
    dplyr::select(-all_of(vars_to_rm))
  
  # Firms: If NA, then 0 -------------------------------------------------------
  firm_vars <- names(data)[grepl("firms|employment", names(data))]
  
  for(var in firm_vars){
    data[[var]][is.na(data[[var]])] <- 0
  }
  
  # Transform Variables --------------------------------------------------------
  #### Transform
  variables <- names(data)[grepl("dmsp|firms|employment", names(data))]
  
  for(var in variables){
    data[[paste0(var, "_log")]] <- log(data[[var]] + 1)
    data[[paste0(var, "_g5")]] <- data[[var]] %>% cut2(g = 5) %>% as.numeric()
    #data[[paste0(var, "_g10")]] <- data[[var]] %>% cut2(g = 10) %>% as.numeric()
    #data[[paste0(var, "_g25")]] <- data[[var]] %>% cut2(g = 25) %>% as.numeric()
    #data[[paste0(var, "_g50")]] <- data[[var]] %>% cut2(g = 50) %>% as.numeric()
  }
  

  #### Add differences  
  merge_vars <- c("id", "year")
  
  if(country %in% "canada") MAX_DIFF <- 6
  if(country %in% "mexico") MAX_DIFF <- 2
  data_diffs_list <- lapply(1:MAX_DIFF, function(i){
    print(i)
    
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
  data$unit <- dataset 

  # Export ---------------------------------------------------------------------
  saveRDS(data, file.path(MERGED_DATA_PATH, paste0(dataset, "_clean.Rds")))
}





