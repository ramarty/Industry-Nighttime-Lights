# Merge Polygon Datasets

OVERWRITE_FILE <- F

if(OVERWRITE_FILE){
  print("Deleting data!!!")
  Sys.sleep(10)
  
  to_rm_can <- file.path(data_file_path, "Grid", "FinalData", "canada", 
                         "merged_datasets") %>%
    list.files(full.names = T) %>%
    str_subset("_diff")
  
  to_rm_mex <- file.path(data_file_path, "Grid", "FinalData", "mexico", 
                         "merged_datasets") %>%
    list.files(full.names = T) %>%
    str_subset("_diff")
  
  for(file in c(to_rm_can, to_rm_mex)){
    file.remove(file)
  }
  
}

for(country in c("canada", "mexico")){
  
  # Load and prep data ---------------------------------------------------------
  ## Load data
  df <- readRDS(file.path(data_file_path, "Grid", "FinalData", country, 
                          "merged_datasets", "firm_ntl.Rds"))
  df <- df[df$unit != "hexgrid7",]
  
  ## Complete Mexico data; irregular time period between years, so make regular
  if(country %in%  "mexico"){
    df <- df %>%
      tidyr::complete(year = 2004:2020, nesting(id, unit))
  }
  
  if(country %in% "canada") diff_intervals <- 1:6
  if(country %in% "mexico") diff_intervals <- c(1:6,13:16)
  
  # First difference -----------------------------------------------------------
  for(i in diff_intervals){
    print(paste(country, i))
    
    ## Make Out Path
    if(country %in% "canada"){
      diff_year <- i*2
    } else{
      diff_year <- i
    }
    
    OUT_PATH <- file.path(data_file_path, "Grid", "FinalData", country, 
                          "merged_datasets", paste0("firm_ntl_diff_",diff_year,".Rds"))
    
    ## Check if should calculate first difference
    if(!file.exists(OUT_PATH) | OVERWRITE_FILE){
      
      calc_diffi <- function(var, i) var - lag(var, i)
      
      df_diffi <- df %>%
        mutate(keep_obs = 1) %>%
        
        ## Select to relevant variables 
        dplyr::select_at(vars(id, unit, year,
                              keep_obs,
                              contains("n_firms"),
                              contains("employment"),
                              contains("viirs"),
                              contains("dmsp"))) %>%
        
        ## First difference
        arrange(year) %>%
        group_by(id, unit) %>%
        mutate_at(vars(-id, -unit, -year), ~calc_diffi(., i)) %>%
        dplyr::ungroup() %>%
        as.data.frame() %>%
        dplyr::filter(!is.na(keep_obs))
      
      # Export -----------------------------------------------------------------
      saveRDS(df_diffi, OUT_PATH)  
    }
  }
}



