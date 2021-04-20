# Industry and Nighttime Lights
# Master Script

RUN_SCRIPTS <- F

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "robmarty") overleaf_file_path <- "~/Dropbox/Apps/Overleaf/Industry and Nighttime Lights"

if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/Methods Papers/Industry and Nighttime lights Project"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/Methods Papers/Industry and Nighttime lights Project"

if(Sys.info()[["user"]] == "WB521633") github_file_path <- "C:/Users/wb521633/Documents/Github/Industry-Nighttime-Lights"
if(Sys.info()[["user"]] == "robmarty") github_file_path <- "~/Documents/Github/Industry-Nighttime-Lights"

data_file_path       <- file.path(project_file_path, "Data")
raw_data_file_path   <- file.path(project_file_path, "Data", "RawData")
final_data_file_path <- file.path(project_file_path, "Data", "FinalData")
figures_file_path    <- file.path(overleaf_file_path, "figures")
tables_file_path     <- file.path(overleaf_file_path, "tables")

merged_data_grid <- file.path(data_file_path, "Grid", "FinalData", "merged_datasets")
merged_data_gadm <- file.path(data_file_path, "GADM", "FinalData", "merged_datasets")

PROJ_canada <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=0 +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
PROJ_mexico <- "+init=epsg:4485"

FIRM_YEARS_CANADA <- c(2001, 2003, 2005, 2007, 2009, 2011, 2013)
FIRM_YEARS_MEXICO <- c(2004, 2009, 2014, 2017, 2018, 2019, 2020)

# Libraries --------------------------------------------------------------------
library(raster)
library(dplyr)
library(parallel)
library(pbmcapply)
library(rgdal)
library(rgeos)
library(geosphere)
library(sf)
library(ggplot2)
library(data.table)
library(gtools)
library(reshape)
library(doBy)
library(velox)
library(readstata13)
library(lfe)
library(haven)
library(ggplot2)
library(mapproj)
library(ggpubr)
library(stringr)
library(hrbrthemes)
library(Hmisc)
library(wesanderson)
library(spgwr)
library(broom)
library(viridis)
library(stargazer)
library(labelled)
library(estimatr)
library(readr)
library(tidyr)
library(ggpubr)
library(leaflet)

# User Defined Functions -------------------------------------------------------
source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")
source(file.path(github_file_path, "functions", "functions.R"))

# Scripts ----------------------------------------------------------------------
if(RUN_SCRIPTS){
  
  # 1. CLEAN INDIVIDUAL DATASETS =================================================
  clean_indiv_code_dir <- file.path(github_file_path, "01_clean_individual_datasets")
  
  # 1.1 GADM ---------------------------------------------------------------------
  # Download GADM files for each country and make a convex hull around Canada.
  
  ## Download GADM
  source(file.path(clean_indiv_code_dir, "GADM", "download_gadm.R"))
  
  ## Make convex hull around Canada. This polygon is used to extract nighttime lights
  # from Google Earth Engine. We only do this for Canada (not Mexico) as Canada
  # is big and causes computational problems.
  source(file.path(clean_indiv_code_dir, "GADM", "canada_chull.R"))
  
  # 1.2 Firms Data ---------------------------------------------------------------
  # Clean firm data and add value of nighttime lights pixel to each firm. For each
  # country, save a file for each year; this helps minimize computational burdens
  # in later steps (having to only have to load one year as opposed to load
  # the full data---which is large---then subset).
  
  #### Canada
  # Clean Data
  source(file.path(clean_indiv_code_dir, "Canada Firm Data", "01_append_firm_data.R"))
  
  # Create file for each year
  source(file.path(clean_indiv_code_dir, "Canada Firm Data", "02_split_separate_years.R"))
  
  #### Mexico
  # Separately clean 2004 - 2014 and 2017 - 2017 files as these have slightly
  # different formatting
  source(file.path(clean_indiv_code_dir, "Mexico Firm Data", "01_append_firm_data_04_14.R"))
  source(file.path(clean_indiv_code_dir, "Mexico Firm Data", "01_append_firm_data_17_20.R"))
  
  # Create file for each year
  source(file.path(clean_indiv_code_dir, "Mexico Firm Data", "02_split_separate_years.R"))
  
  # 1.3 Grid Data ----------------------------------------------------------------
  # Create hexagonal grids for each country of different sizes (5km up to 1000km
  # diameters). Then, rasterize the polygons---where the raster value is the grid
  # ID. Rasterizing helps in later steps to minimize the computational burden; it's
  # faster to extract a value from a raster than a polygon. Hexagonal grids are
  # produced for the full county, but grids where there are no nighttime lights
  # or no firms are removed.
  
  ## Create grids
  source(file.path(clean_indiv_code_dir, "Grids", "01_create_hexagons_canada.R"))
  source(file.path(clean_indiv_code_dir, "Grids", "01_create_hexagons_mexico.R"))
  
  ## Rasterize grid polygons
  source(file.path(clean_indiv_code_dir, "Grids", "02_rasterize.R"))
  
  # 2. CREATE ANALYSIS DATASETS ================================================
  # Extracts firm employment and nighttime light values to polygons. Creates
  # a dataset at the polygon-year level with these variables
  
  create_anlys_dta_code <- file.path(github_file_path, "02_create_analysis_datasets")
  
  ## Extract firm data to grids
  source(file.path(create_anlys_dta_code, "01_summarize_firm_data_to_grids.R"))
  
  # Extract nighttime lights data to grids
  source(file.path(create_anlys_dta_code, "01_summarize_ntl_to_grids.R"))
  
  # Merge firm and nighttime lights data together
  source(file.path(create_anlys_dta_code, "02_merge_datasets.R"))
  
  # Clean datasets Includes constructing variables, including logging variables
  # and constructing first differences. This code takes about 4 hours to run.
  source(file.path(create_anlys_dta_code, "Grids", "03_clean_datasets.R"))
  
  # 3. ANALYSIS ================================================================
  analysis_code <- file.path(github_file_path, "03_analysis")
  
}








