# Summarize data in polygons

OVERWRITE_FILE <- F

# Define functions -------------------------------------------------------------

# https://github.com/r-spatial/sf/issues/1525
st_erase = function(x, y) st_difference(x, st_union(st_combine(y))) 

rm_center_polygon <- function(polygon, polygon_buff){
  # Uses polygon and polygon_buff as inputs. polygon_buff is a buffered version
  # of polygon. Removes the 'polygon' area from 'polygon_buff'
  
  lapply(1:nrow(polygon), function(i){
    if((i %% 100) %in% 0) print(paste0(i, "/", nrow(polygon), " Erase Center"))
    
    st_erase(polygon_buff[i,], polygon[i,])
  }) %>% 
    bind_rows()
  
}

st_buffer_rm_center <- function(polygon,
                                width,
                                chunk_size = 2000){
  
  if(FALSE %in% st_is_valid(polygon)){
    # TODO: If takes too long, can do in chunks
    polygon <- st_make_valid(polygon)
  }
  
  polygon_buff <- st_buffer_chunks(polygon, width, chunk_size) 
  polygon_buff_rm_c <- rm_center_polygon(polygon, polygon_buff)
  
}

# Process data -----------------------------------------------------------------
for(country in c("canada", "mexico")){
  for(buffer_times_width in c(0, 1, 2)){
    for(res in 1:7){
      
      #### Country details
      if(country %in% "canada") FIRM_YEARS <- FIRM_YEARS_CANADA
      if(country %in% "mexico") FIRM_YEARS <- FIRM_YEARS_MEXICO
      
      iso <- country %>% substring(1,3)
      
      if(res %in% 1) width <- 418.676005500*1000*2
      if(res %in% 2) width <- 158.244655800*1000*2
      if(res %in% 3) width <- 59.810857940*1000*2
      if(res %in% 4) width <- 22.606379400*1000*2
      if(res %in% 5) width <- 8.544408276*1000*2
      if(res %in% 6) width <- 3.229482772*1000*2
      if(res %in% 7) width <- 1.220629759*1000*2
      if(res %in% 8) width <- 0.461354684*1000*2
      
      #### Load grid
      grid_sf <- readRDS(file.path(data_file_path, "Grid", "FinalData", country, "grids_blank",
                                   paste0("hexgrid_", res, ".Rds")))
      
      #### Names for dataset
      dset <- paste0("hexgrid", res)
      suffix <- ""
      
      #### Out files and check if need to process any file
      ## Buffer suffix
      if(buffer_times_width != 0){
        suffix <- paste0("_splag_wdt", buffer_times_width)
      }
      
      OUT_FILE_VIIRS <- file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets", "ntl", 
                                  paste0(dset, "_viirs", suffix, ".Rds"))
      
      OUT_FILE_VIIRS_C <- file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets", "ntl", 
                                    paste0(dset, "_viirs_corrected", suffix, ".Rds"))
      
      OUT_FILE_DMSP_HARMON <- file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets", "ntl", 
                                        paste0(dset, "_dmspols_harmon", suffix, ".Rds"))
      
      OUT_FILE_DMSP_CAL <- file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets", "ntl", 
                                     paste0(dset, "_dmspols_harmon_caldmsp", suffix, ".Rds"))
      
      if(country %in% "mexico"){
        EXTRACT_ANY <- (!file.exists(OUT_FILE_VIIRS) |
                          !file.exists(OUT_FILE_VIIRS_C) | 
                          !file.exists(OUT_FILE_DMSP_HARMON) |
                          !file.exists(OUT_FILE_DMSP_CAL) | 
                          OVERWRITE_FILE)
      } else{
        EXTRACT_ANY <- (!file.exists(OUT_FILE_VIIRS) |
                          #!file.exists(OUT_FILE_VIIRS_C) | ## Don't compute for Canada
                          !file.exists(OUT_FILE_DMSP_HARMON) |
                          !file.exists(OUT_FILE_DMSP_CAL) | 
                          OVERWRITE_FILE)
        
      }
      
      #### Extract data 
      if(EXTRACT_ANY){
        print(paste(country, buffer_times_width, res))
        
        #### Buffer
        if(buffer_times_width != 0){
          grid_sf <- st_buffer_rm_center(grid_sf, width*buffer_times_width)
        }
        
        ## VIIRS
        if(!file.exists(OUT_FILE_VIIRS) | OVERWRITE_FILE){
          
          ntl_df <- map_df(FIRM_YEARS[FIRM_YEARS >= 2012], function(year){
            print(year)
            r <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0(iso, "_viirs_mean_",year,".tif")))
            grid_sf[[paste0("viirs", suffix)]] <- exact_extract(r, grid_sf, 'mean')
            grid_sf$year <- year
            grid_sf$geometry <- NULL
            return(grid_sf)
          })
          
          saveRDS(ntl_df, OUT_FILE_VIIRS)
          
          rm(ntl_df)
          
        }
        
        ## VIIRS Corrected
        if(country %in% "mexico"){ # Canada doesn't have years that overlap
          if(!file.exists(OUT_FILE_VIIRS_C) | OVERWRITE_FILE){
            
            ntl_df <- map_df(FIRM_YEARS[FIRM_YEARS >= 2014], function(year){
              print(year)
              r <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0(iso, "_viirs_corrected_mean_",year,".tif")))
              grid_sf[[paste0("viirs_c", suffix)]] <- exact_extract(r, grid_sf, 'mean')
              grid_sf$year <- year
              grid_sf$geometry <- NULL
              return(grid_sf)
            })
            
            saveRDS(ntl_df, OUT_FILE_VIIRS_C)
            
            rm(ntl_df)
            
          }
        }
        
        ## DMSP Harmonized
        if(!file.exists(OUT_FILE_DMSP_HARMON) | OVERWRITE_FILE){
          
          ntl_df <- map_df(FIRM_YEARS, function(year){
            print(year)
            r <- readRDS(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_VIIRS_LI_HARMONIZED",
                                   "FinalData",
                                   paste0(country, "_dmspols_harmon_", year, ".Rds")))
            grid_sf[[paste0("dmspharmon", suffix)]] <- exact_extract(r, grid_sf, 'mean')
            grid_sf$year <- year
            grid_sf$geometry <- NULL
            return(grid_sf)
          })
          
          saveRDS(ntl_df, OUT_FILE_DMSP_HARMON)
          
          rm(ntl_df)
          
        }
        
        ## DMSP Harmonized - calSIM Only
        # Mexico has data in 2004, 2009, 2014, etc. To look at trends from 2004 to 2014,
        # use calDMSP in 2013 rather than simVIIRS in 2014.
        if(!file.exists(OUT_FILE_DMSP_CAL) | OVERWRITE_FILE){
          
          ntl_df <- map_df(FIRM_YEARS[FIRM_YEARS <= 2014], function(year){
            print(year)
            
            if(year %in% 2014){
              year_ntl <- 2013
            } else{
              year_ntl <- year
            }
            
            r <- readRDS(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_VIIRS_LI_HARMONIZED",
                                   "FinalData",
                                   paste0(country, "_dmspols_harmon_", year_ntl, ".Rds")))
            grid_sf[[paste0("caldmsp", suffix)]] <- exact_extract(r, grid_sf, 'mean')
            grid_sf$year <- year
            grid_sf$geometry <- NULL
            return(grid_sf)
          })
          
          saveRDS(ntl_df, OUT_FILE_DMSP_CAL)
          
          rm(ntl_df)
          
        }
        
      }
    }
  }
}


