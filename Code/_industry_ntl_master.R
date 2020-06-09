# Industry and Nighttime Lights
# Master Script

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/Methods Papers/Industry and Nighttime lights Project"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/Methods Papers/Industry and Nighttime lights Project"

if(Sys.info()[["user"]] == "WB521633") github_file_path <- "C:/Users/wb521633/Documents/Github/Industry-Nighttime-Lights"
if(Sys.info()[["user"]] == "robmarty") github_file_path <- "~/Documents/Github/Industry-Nighttime-Lights"

data_file_path <- file.path(project_file_path, "Data")
raw_data_file_path <- file.path(project_file_path, "Data", "RawData")
final_data_file_path <- file.path(project_file_path, "Data", "FinalData")
figures_file_path <- file.path(project_file_path, "Paper", "figures")
tables_file_path <- file.path(project_file_path, "Paper", "tables")

merged_data_grid <- file.path(data_file_path, "Grid", "FinalData", "merged_datasets")
merged_data_gadm <- file.path(data_file_path, "GADM", "FinalData", "merged_datasets")

PROJ_canada <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=0 +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
PROJ_mexico <- "+init=epsg:4485"

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

# User Defined Functions -------------------------------------------------------
source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")
source(file.path(github_file_path, "Code", "functions", "functions.R"))

