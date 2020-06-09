# Summarize data in polygons

# Take average nighttime lights and firm level data within hexagons and GADM polygons

source("~/Documents/Github/Industry-Nighttime-Lights/Code/_industry_ntl_master.R")

FIRM_YEARS <- c(2004, 2009, 2014)

EXTRACT_FIRMS_ALL <- T
EXTRACT_DMSPOLS   <- T

# Load Firm Data --------------------------------------------------------------------
firms <- readRDS(file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_clean.Rds"))
firms <- spTransform(firms, CRS(PROJ_canada))
firms@data <- firms@data %>%
  mutate(N_firms = 1) %>%
  dplyr::select(employment, N_firms, year)
firms <- spTransform(firms, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Extract Data -----------------------------------------------------------------
#### Dataset names
grid_files <- list.files(file.path(data_file_path, "Grid", "RawData"), pattern = "*.Rds") %>%
  str_replace_all(".Rds", "") %>%
  str_subset("mex") 

gadm_files <- list.files(file.path(data_file_path, "GADM", "RawData"), pattern = "*.rds") %>%
  str_replace_all(".rds", "") %>%
  str_subset("MEX")

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
    
    OUT_PATH <- file.path(data_file_path, "Grid", "FinalData", "mexico", "individual_datasets")
  }
  
  if(grepl("gadm", dataset)){
    polygon <- readRDS(file.path(data_file_path, "GADM", "RawData", paste0(dataset, ".Rds")))
    polygon$id <- 1:nrow(polygon)
    
    if(dataset %in% c("gadm36_CAN_0_sp")) polygon$group <- polygon$NAME_0
    if(dataset %in% c("gadm36_CAN_1_sp")) polygon$group <- polygon$NAME_1
    if(dataset %in% c("gadm36_CAN_2_sp", "gadm36_CAN_3_sp")) polygon$group <- polygon$NAME_1
    
    OUT_PATH <- file.path(data_file_path, "GADM", "FinalData", "mexico", "individual_datasets")
  }
  
  #### All Firms
  if(EXTRACT_FIRMS_ALL){
    polygon_firms_all   <- lapply(FIRM_YEARS, extract_firm_stats, polygon, firms, "_all") %>% bind_rows()
    saveRDS(polygon_firms_all, file.path(OUT_PATH, paste0(dataset,"_firms_all",".Rds")))
  }
  
  #### DMSPOLS
  if(EXTRACT_DMSPOLS){
    polygon_dmspols <- lapply(FIRM_YEARS, extract_dmspols, polygon, "mexico") %>% bind_rows()
    saveRDS(polygon_dmspols, file.path(OUT_PATH, paste0(dataset,"_dmspols",".Rds")))
  }
  
  
}



