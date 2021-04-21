# Merge Polygon Datasets

country <- "mexico"
dataset <- "hex_50km"
subset <- "viirs"

print("03 CLEAN DATASETS")

for(country in c("canada", "mexico")){
  
  datasets <- file.path(data_file_path, "Grid", "FinalData", country, "merged_datasets") %>%
    list.files() %>%
    stri_subset_fixed("_clean.Rds", negate = TRUE)
  
  for(dataset_i in datasets){
  
    print(paste(dataset_i, country, "--------------------------------"))
    
    # Load Data ------------------------------------------------------------------
    MERGED_DATA_PATH <- file.path(data_file_path, "Grid", "FinalData", country, "merged_datasets")
    
    data <- readRDS(file.path(MERGED_DATA_PATH, dataset_i))
    data$group   <- NULL
    data$group.x <- NULL
    data$group.y <- NULL
    
    # Firms: If NA, then 0 -------------------------------------------------------
    firm_vars <- names(data)[grepl("firms|employment", names(data))]
    
    for(var in firm_vars){
      data[[var]][is.na(data[[var]])] <- 0
    }
    
    # Subset Data ----------------------------------------------------------------
    # NO: Only keep cells that had positive light or a firm at some point
    # Only keep cells that had a firm at some point
    data <- data %>%
      group_by(id) %>%
      #mutate(firms_positive_anyyear   = max(N_firms_sum_all, na.rm=T) > 0,
      #       dmspols_positive_anyyear = max(dmspols_sum, na.rm=T) > 0,
      #       viirs_positive_anyyear   = max(viirs_sum, na.rm=T) > 0) %>%
      mutate(firms_positive_anyyear   = max(N_firms_sum_all, na.rm=T) > 0) %>%
      ungroup() %>%
      #dplyr::filter(firms_positive_anyyear | dmspols_positive_anyyear | viirs_positive_anyyear)
      dplyr::filter(firms_positive_anyyear %in% T)
    
    #### Remove unneeded variables
    #specific_vars_to_rm <- c("group.x", "group.y")
    firmsmean_var_to_rm <- names(data)[grepl("firms_mean", names(data))] # only need sum
    employmean_var_to_rm <- names(data)[grepl("employment_mean|empl_med_fact_mean|empl_med_mean", names(data))] # only need sum (for now?)
    employmean_other_var_to_rm <- names(data)[grepl("empl_med_fact_sum_[[:digit:]]|empl_med_sum_[[:digit:]]", names(data))] # only need sum (for now?)
    
    vars_to_rm <- c(#specific_vars_to_rm,
      firmsmean_var_to_rm,
      employmean_var_to_rm,
      employmean_other_var_to_rm)
    
    data <- data[,!(names(data) %in% vars_to_rm)]
    #data <- data %>%
    #  dplyr::select(-all_of(vars_to_rm))
    
    #vars_to_keep <- names(data)[!grepl("_[[:digit:]][[:digit:]]", names(data))]
    #data <- data[,vars_to_keep]
    
    # Transform Variables --------------------------------------------------------
    variables <- names(data)[grepl("dmspols|firms|employment|empl|viirs", names(data))]
    variables <- variables[!grepl("anyyear", variables)]
    variables <- variables[!(variables %in% c("lon", "lat"))]
    
    ## REMOVE BY SECTOR
    variables <- variables[!grepl("_[[:digit:]][[:digit:]]", variables)]
    
    for(var in variables){
      data[[paste0(var, "_log")]] <- log(data[[var]] + 1)
      #data[[paste0(var, "_g5")]] <- data[[var]] %>% cut2(g = 5) %>% as.numeric()
      #data[[paste0(var, "_g10")]] <- data[[var]] %>% cut2(g = 10) %>% as.numeric()
      #data[[paste0(var, "_g25")]] <- data[[var]] %>% cut2(g = 25) %>% as.numeric()
      #data[[paste0(var, "_g50")]] <- data[[var]] %>% cut2(g = 50) %>% as.numeric()
    }
    
    # Complete ---------------------------------------------------------------
    # Need to complete year-id combination for Mexico with viirs subset
    # as irregular time period
    if(country %in%  "mexico" & subset %in% "viirs"){
      data <- data %>%
        complete(year = 2014:2020, id)
    }
    
    # Variable Differences ---------------------------------------------------
    merge_vars <- c("id", "year")
    
    if(country %in% "canada") MAX_DIFF <- 6
    if(country %in% "mexico" & subset %in% "dmspols") MAX_DIFF <- 2
    if(country %in% "mexico" & subset %in% "viirs")   MAX_DIFF <- 6
    
    data_diffs_list <- lapply(1:MAX_DIFF, function(i){
      print(i)
      
      calc_diffi <- function(var, i) var - lag(var, i)
      
      data_diffi <- data %>%
        dplyr::arrange(year) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate_at(vars(-id, -year), ~calc_diffi(., i)) %>%
        dplyr::ungroup() %>%
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
    if(country %in%  "mexico" & subset %in% "viirs"){
      data <- data %>%
        filter(!(year %in% c(2015, 2016)))
    }
    
    saveRDS(data, file.path(MERGED_DATA_PATH, paste0(dataset, "_clean.Rds")))
  }
}
#}





