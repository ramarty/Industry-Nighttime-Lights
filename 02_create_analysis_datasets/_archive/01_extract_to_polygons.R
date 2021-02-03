# Summarize data in polygons

# Take average nighttime lights and firm level data within hexagons and GADM polygons
EXTRACT_FIRMS_ALL        <- T
EXTRACT_FIRMS_CATEGORIES <- T
EXTRACT_DMSPOLS          <- T
EXTRACT_VIIRS            <- T
EXTRACT_VIIRS_CORRECTED  <- T
REPLACE_FILES            <- F

country <- "mexico"

country_cap <- capitalize(country)

if(country %in% "canada"){
  FIRM_YEARS <- c(2001, 2003, 2005, 2007, 2009, 2011, 2013)
}

if(country %in% "mexico"){
  FIRM_YEARS <- c(2004, 2009, 2014, 2017, 2018, 2019, 2020)
}

type_codes <- readRDS(file.path(data_file_path, paste0(country_cap, " Industry Data"), "FinalData", "naics2_types.Rds"))

# Load Firm Data ---------------------------------------------------------------
#firms <- readRDS(file.path(data_file_path, paste0(capitalize(country), " Industry Data"), "FinalData", "firms.Rds"))
#firms$firms <- 1 # when aggregating, counts total number of firms in area

# Extract Data -----------------------------------------------------------------
#### Dataset names
grid_files <- list.files(file.path(data_file_path, "Grid", "RawData"), pattern = "*.Rds") %>%
  str_replace_all(".Rds", "") %>%
  str_subset(country %>% substring(1,3)) # "can" or "mex"

gadm_files <- list.files(file.path(data_file_path, "GADM", "RawData"), pattern = "*.rds") %>%
  str_replace_all(".rds", "") %>%
  str_subset(country %>% substring(1,3) %>% toupper()) # "CAN" or "MEX"

#### Loop through datasets and process
for(dataset in c(grid_files)){ 
  
  print(paste(dataset, "-----------------------------------------------------"))
  
  #### Load Data
  # 1. Make sure in WGS84 CRS
  # 2. Add a group variable - velox done in "group" chunks.
  if(grepl("hex", dataset)){
    polygon <- readRDS(file.path(data_file_path, "Grid", "RawData", paste0(dataset, ".Rds")))
    polygon <- spTransform(polygon, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    polygon$group <- 1
    
    OUT_PATH <- file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets")
  }
  
  if(grepl("gadm", dataset)){
    polygon <- readRDS(file.path(data_file_path, "GADM", "RawData", paste0(dataset, ".Rds")))
    polygon$id <- 1:nrow(polygon)
    
    if(dataset %in% c("gadm36_CAN_0_sp")) polygon$group <- polygon$NAME_0
    if(dataset %in% c("gadm36_CAN_1_sp")) polygon$group <- polygon$NAME_1
    if(dataset %in% c("gadm36_CAN_2_sp", "gadm36_CAN_3_sp")) polygon$group <- polygon$NAME_1
    
    OUT_PATH <- file.path(data_file_path, "GADM", "FinalData", country, "individual_datasets")
  }
  
  #### All Firms
  if(EXTRACT_FIRMS_ALL){
    for(year_i in FIRM_YEARS){
      
      OUT_PATH_i <- file.path(OUT_PATH, paste0(dataset,"_firms_",year_i,".Rds"))
      
      if(!file.exists(OUT_PATH_i) | REPLACE_FILES){
        
        firms_i <- readRDS(file.path(data_file_path, 
                                     paste0(capitalize(country), " Industry Data"), "FinalData",
                                     paste0("firms_", year, ".Rds")))
        
        a <- extract_firm_stats(polygon[1:10,], firms_i)
        
        polygon_firms_all <- lapply(FIRM_YEARS, extract_firm_stats, 
                                    polygon, 
                                    file.path(data_file_path, paste0(capitalize(country), " Industry Data"), "FinalData"), 
                                    "_all")
        
      }
      
      
      
    }
    
    
    
    
    
    
    if(!file.exists(OUT_PATH_i) | REPLACE_FILES){
      polygon_firms_all <- lapply(FIRM_YEARS, extract_firm_stats, 
                                  polygon, 
                                  file.path(data_file_path, paste0(capitalize(country), " Industry Data"), "FinalData"), 
                                  "_all") %>% 
        bind_rows()
      saveRDS(polygon_firms_all, OUT_PATH_i)
    }
    rm(OUT_PATH_i)
  }
  
  #### Firm Categories
  if(EXTRACT_FIRMS_CATEGORIES | F){
    
    for(type_i in type_codes){
      print(paste("type:", type_i, "-----------------------------------------"))
      
      OUT_PATH_i <- file.path(OUT_PATH, paste0(dataset,"_firms_t",type_i,".Rds"))
      
      if(!file.exists(OUT_PATH_i) | REPLACE_FILES){
        polygon_firms_typei <- lapply(FIRM_YEARS, 
                                      extract_firm_stats, 
                                      polygon, 
                                      file.path(data_file_path, paste0(capitalize(country), " Industry Data"), "FinalData"),
                                      type_i, 
                                      paste0("_t",type_i)) %>% 
          bind_rows()
        saveRDS(polygon_firms_typei, OUT_PATH_i)
      }
      rm(OUT_PATH_i)
    }
    
  }
  
  #### DMSPOLS
  print("dmspols -------------------------------------------------------------")
  if(EXTRACT_DMSPOLS){
    OUT_PATH_i <- file.path(OUT_PATH, paste0(dataset,"_dmspols",".Rds"))
    
    if(!file.exists(OUT_PATH_i) | REPLACE_FILES){
      polygon_dmspols <- lapply(FIRM_YEARS[FIRM_YEARS <= 2014], extract_ntl, polygon, country, "dmspols") %>% bind_rows()
      saveRDS(polygon_dmspols, OUT_PATH_i)
    }
    rm(OUT_PATH_i)
  }
  
  #### VIIRS
  print("viirs -------------------------------------------------------------")
  if(EXTRACT_VIIRS){
    OUT_PATH_i <- file.path(OUT_PATH, paste0(dataset,"_viirs",".Rds"))
    
    if(!file.exists(OUT_PATH_i) | REPLACE_FILES){
      polygon_viirs <- lapply(FIRM_YEARS[FIRM_YEARS >= 2011], extract_ntl, polygon, country, "viirs") %>% bind_rows()
      saveRDS(polygon_viirs, OUT_PATH_i)
    }
    
    rm(OUT_PATH_i)
  }
  
  #### VIIRS Corrected
  print("viirs corrected -----------------------------------------------------")
  if(EXTRACT_VIIRS_CORRECTED & country %in% "mexico"){
    OUT_PATH_i <- file.path(OUT_PATH, paste0(dataset,"_viirs_corrected",".Rds"))
    
    if(!file.exists(OUT_PATH_i) | REPLACE_FILES){
      polygon_viirs_c <- lapply(FIRM_YEARS[FIRM_YEARS >= 2014], extract_ntl, polygon, country, "viirs_corrected") %>% bind_rows()
      saveRDS(polygon_viirs_c, OUT_PATH_i)
    }
    rm(OUT_PATH_i)
  }
  
  
}



