# Summarize data in polygons

# Take average nighttime lights and firm level data within hexagons and GADM polygons

FIRM_YEARS <- c(2001, 2003, 2005, 2007, 2009, 2011, 2013)

EXTRACT_FIRMS_ALL <- T
EXTRACT_DMSPOLS   <- T

# Load Firm Data --------------------------------------------------------------------
firms <- readRDS(file.path(data_file_path, "Canada Industry Data", "FinalData", "firms_clean.Rds"))
firms <- spTransform(firms, CRS(PROJ_canada))
firms@data <- firms@data %>%
  mutate(N_firms = 1) %>%
  dplyr::select(employment, N_firms, year)
firms <- spTransform(firms, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Extraction Functions ---------------------------------------------------------
extract_firm_stats <- function(year, polygon, firms, firm_name_suffix){
  
  #### Where are we?
  print(paste(year, "------------------------------------------------------"))
  
  #### Grab firm data in year i
  firms_i <- firms[firms$year %in% year,]
  
  #### Country Level
  if(nrow(polygon) %in% 1){
    
    polygon_OVER_firm_mean <- apply(firms_i@data, 2, mean, na.rm=T) %>% t %>% as.data.frame()
    polygon_OVER_firm_sum <- apply(firms_i@data, 2, sum, na.rm=T) %>% t %>% as.data.frame()
    
    polygon_OVER_firm_mean$year <- NULL
    polygon_OVER_firm_sum$year <- NULL
    
  } else{
    polygon_OVER_firm_mean <- over_chunks(polygon, firms_i, "mean", 100) 
    polygon_OVER_firm_sum <- over_chunks(polygon, firms_i, "sum", 100) 
  }
  
  names(polygon_OVER_firm_mean) <- paste0(names(polygon_OVER_firm_mean), "_mean")
  names(polygon_OVER_firm_sum) <- paste0(names(polygon_OVER_firm_sum), "_sum")
  
  polygon_OVER_firm <- bind_cols(polygon_OVER_firm_mean, 
                                 polygon_OVER_firm_sum)
  names(polygon_OVER_firm) <- paste0(names(polygon_OVER_firm), firm_name_suffix)
  
  polygon_OVER_firm$id <- polygon$id
  polygon_OVER_firm$year <- year
  
  return(polygon_OVER_firm)
}

extract_dmspols <- function(year, polygon){
  print(paste(year, "------------------------------------------------------"))
  
  dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("canada_dmspols_",year,".tif")))
  
  if(nrow(polygon) == 1){
    polygon$dmspols_mean <- mean(dmspols[], na.rm=T)
    polygon$dmspols_median <- median(dmspols[], na.rm=T)
    polygon$dmspols_sum <- sum(dmspols[], na.rm=T)
    
    polygon_data <- polygon@data
  } else{
    
    # velox extraction is slow with all of dmspols. So break into chunks.
    polygon_data <- lapply(unique(polygon$group), function(i){
      print(paste("velox", i))
      
      polygon_i <- polygon[polygon$group %in% i,]
      dmspols_i <- dmspols %>% crop(polygon_i) %>% velox()
      
      polygon_i$dmspol_mean <- dmspols_i$extract(sp=polygon_i, fun = function(x) mean(x, na.rm=T), small=T) %>% as.vector()
      polygon_i$dmspol_median <- dmspols_i$extract(sp=polygon_i, fun = function(x) median(x, na.rm=T), small=T) %>% as.vector()
      polygon_i$dmspol_sum <- dmspols_i$extract(sp=polygon_i, fun = function(x) sum(x, na.rm=T), small=T) %>% as.vector()
      
      return(polygon_i@data)
    }) %>%
      bind_rows()
    
  }
  
  polygon_data$year <- year
  
  return(polygon_data)
}

extract_firms_dmspols <- function(polygon, firms, firm_name_suffix){
  
  polygon_firms   <- lapply(FIRM_YEARS, extract_firm_stats, polygon, firms, firm_name_suffix) %>% bind_rows()
  polygon_dmspols <- lapply(FIRM_YEARS, extract_dmspols, polygon) %>% bind_rows()
  
  polygon_data <- merge(polygon_firms, polygon_dmspols, by=c("year", "id"))
  return(polygon_data)
}

# Extract Data -----------------------------------------------------------------
#### Dataset names
grid_files <- list.files(file.path(data_file_path, "Grid", "RawData"), pattern = "*.Rds") %>%
  str_replace_all(".Rds", "")

gadm_files <- list.files(file.path(data_file_path, "GADM", "RawData"), pattern = "*.rds") %>%
  str_replace_all(".rds", "")

#### Loop through datasets and process
for(dataset in c(grid_files,
                 gadm_files)){
  
  print(paste(dataset, "-----------------------------------------------------"))
  
  #### Load Data
  # 1. Make sure in WGS84 CRS
  # 2. Add a group variable - velox done in "group" chunks.
  if(grepl("hex", dataset)){
    polygon <- readRDS(file.path(data_file_path, "Grid", "RawData", paste0(dataset, ".Rds")))
    polygon <- spTransform(polygon, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    polygon$group <- 1
    
    OUT_PATH <- file.path(data_file_path, "Grid", "FinalData", "individual_datasets")
  }
  
  if(grepl("gadm", dataset)){
    polygon <- readRDS(file.path(data_file_path, "GADM", "RawData", paste0(dataset, ".Rds")))
    polygon$id <- 1:nrow(polygon)
    
    if(dataset %in% c("gadm36_CAN_0_sp")) polygon$group <- polygon$NAME_0
    if(dataset %in% c("gadm36_CAN_1_sp")) polygon$group <- polygon$NAME_1
    if(dataset %in% c("gadm36_CAN_2_sp", "gadm36_CAN_3_sp")) polygon$group <- polygon$NAME_1
    
    OUT_PATH <- file.path(data_file_path, "GADM", "FinalData", "individual_datasets")
  }
  
  #### All Firms
  if(EXTRACT_FIRMS_ALL){
    polygon_firms_all   <- lapply(FIRM_YEARS, extract_firm_stats, polygon, firms, "_all") %>% bind_rows()
    saveRDS(polygon_firms_all, file.path(OUT_PATH, paste0(dataset,"_firms_all",".Rds")))
  }
  
  #### DMSPOLS
  if(EXTRACT_DMSPOLS){
    polygon_dmspols <- lapply(FIRM_YEARS, extract_dmspols, polygon) %>% bind_rows()
    saveRDS(polygon_dmspols, file.path(OUT_PATH, paste0(dataset,"_dmspols",".Rds")))
  }
  
  
}



