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

# --- Paths ---

project_root <- here()             # Automatically finds root of project (e.g., RStudio project or Git repo)
data_dir <- "01_data"
# Processed data, input of this comparison_project
termap_data_path <- "../TABMON_dataprep/territoryMappingData/01_data/processed/04_data_enriched.rds"      # Full territory mapping data (cleaned, translated, enriched)
visits_data_path <- "../TABMON_dataprep/territoryMappingData/01_data/processed/05_visits_only_data.rds"   # Visits from territory mapping data
bugg_db_path     <- "../TABMON_dataprep/buggData/01_data/buggdata_db.duckdb"                              # BUGG data
# Paths to filtered data after 02_prepare_comparison_data.R
ldv_pam_season_path     <- file.path(data_dir, "01_ldv_pam_season.rds")
ldv_termap_100m_path    <- file.path(data_dir, "02_ldv_termap_100m.rds")
ldv_pam_tw1_exact_path  <- file.path(data_dir, "03_ldv_pam_tw1_exact.rds")  
ldv_pam_tw2_day_path    <- file.path(data_dir, "04_ldv_pam_tw2_day.rds") 
ldv_pam_tw3_week_path   <- file.path(data_dir, "05_ldv_pam_tw3_week.rds") 

# --- Constants ---

crs_projected <- 28992             # Projected local CRS Amersfoort / RD New
buffer100 <- 100                   # Buffer radius in meters to filter TM sightings around sensors

# --- Helper functions ---

# Query BUGG data in the duckdb database, given time windows defined
subset_pam_time_windows <- function(tw_start, tw_end) {
  # Build SQL query
  query <- paste0(                                                    # paste0() space sensitive, do not return to line whenever or remove spaces or it will fail.
    "SELECT pam.*, win.visit_id, win.date ",                          # Select all PAM fields + visit_id + date from TM windows
    "FROM all_data_with_metadata pam ",                               # From main PAM table, alias as 'pam'
    "JOIN ldv_termap_tw win ",                                        # Join with the termap visit windows table, alias as 'win'
    # Condition: PAM detection timestamp must fall between the chosen start/end column of the TM visit window
    "ON pam.detection_time_utc BETWEEN win.", tw_start, " AND win.", tw_end, " ",
    "WHERE pam.cluster = 'loenderveen'"                               # Only keep PAM detections from the 'loenderveen' cluster
  )
  dbGetQuery(bugg_db_connect, query)
}
