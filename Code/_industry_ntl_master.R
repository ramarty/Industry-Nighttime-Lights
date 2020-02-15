# Industry and Nighttime Lights
# Master Script

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/Methods Papers/Industry and Nighttime lights Project"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/Methods Papers/Industry and Nighttime lights Project"

raw_data_file_path <- file.path(project_file_path, "Data", "RawData")
final_data_file_path <- file.path(project_file_path, "Data", "FinalData")
figures_file_path <- file.path(project_file_path, "Paper", "figures")
tables_file_path <- file.path(project_file_path, "Paper", "tables")

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
library(haven)
library(ggplot2)
library(mapproj)

# User Defined Functions -------------------------------------------------------
source("https://raw.githubusercontent.com/ramarty/rgeos_chunks/master/R/rgeos_chunks.R")

