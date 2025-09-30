## -----------------------------
## TABMON_PAMvsTerMap.Rproject
## 01_config.R
## 🔧 Sets up the environment (path, credentials, constants)
## -----------------------------

# --- Libraries ---

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(glue)
library(here)
library(hms)                       # This must be loaded or the start and end_time columns' format breaks
library(leaflet)
library(lubridate)
library(RColorBrewer)
library(scales)
library(sf)                        # This must be loaded or the geometry breaks when opening the visits_data object
library(stringr)
library(tidyr)

# --- Paths ---

project_root <- here()             # Automatically finds root of project (e.g., RStudio project or Git repo)
data_dir <- "01_data"
# Processed data, input of this comparison_project
termap_data_path <- "../TABMON_dataprep/territoryMappingData/01_data/processed/04_data_enriched.rds"      # Full territory mapping data (cleaned, translated, enriched)
visits_data_path <- "../TABMON_dataprep/territoryMappingData/01_data/processed/05_visits_only_data.rds"   # Visits from territory mapping data
bugg_db_path     <- "../TABMON_dataprep/buggData/01_data/buggdata_db.duckdb"                              # BUGG data

# Paths to filtered data after 02_prepare_comparison_data.R
ldv_termap_path         <- file.path(data_dir, "01_ldv_termap.rds")
ldv_termap_100m_path    <- file.path(data_dir, "02_ldv_termap_100m.rds")

ldv_pam_season_path     <- file.path(data_dir, "03_ldv_pam_season.rds")
ldv_pam_tw1_exact_path  <- file.path(data_dir, "04_ldv_pam_tw1_exact.rds")  
ldv_pam_tw2_day_path    <- file.path(data_dir, "05_ldv_pam_tw2_day.rds") 
ldv_pam_tw3_week_path   <- file.path(data_dir, "06_ldv_pam_tw3_week.rds") 

# Path to presence absence matrices
pa_matrices_path        <- file.path(data_dir, "07_presence_absence_matrices.rds")

# --- Constants ---

crs_projected <- 28992             # Projected local CRS Amersfoort / RD New
buffer100 <- 100                   # Buffer radius in meters to filter TM sightings around sensors