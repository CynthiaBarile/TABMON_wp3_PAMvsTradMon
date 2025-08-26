## -----------------------------
## TABMON_PAMvsTerMap.Rproject
## 01_config.R
## 🔧 Sets up the environment (path, credentials, constants)
## -----------------------------

# Load required libraries
library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(here)
library(hms)                                              # This must be loaded or the start and end_time columns' format breaks
library(lubridate)
library(sf)                                               # This must be loaded or the geometry breaks when opening the visits_data object

# Define core file paths based on project root
project_root <- here::here()                              # Automatically finds root of project (e.g., RStudio project or Git repo)
data_dir <- "01_data"

# Processed data, input of this comparison_project
visits_data_path <- "../TABMON_dataprep/territoryMappingData/01_data/processed/05_visits_only_data.rds" # Visits from territory mapping data
bugg_db_path   <- "../TABMON_dataprep/buggData/01_data/buggdata_db.duckdb"                              # BUGG data

# Define constants
crs_projected <- 28992                                                                 # Projected local CRS Amersfoort / RD New
buffer <- 100                                                                          # Buffer radius in meters to filter TM sightings around sensors
