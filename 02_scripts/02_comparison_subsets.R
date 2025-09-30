## -----------------------------
## TABMON_PAMvsTerMap.Rproject
## 02_comparison_subsets.R
## 🦆 Loads cleaned acoustic data database
## 📌 Loads cleaned territory mapping data
## 🔍 Prepare for comparison using Loenderveen as case study
## Input:  TABMON_dataprep > territoryMappingData > 01_data > processed > 05_visits_only_data.rds
##         TABMON_dataprep > buggData > 01_data > buggdata_db.duckdb
## Output: TABMON_PAMvsTerMap > 01_data > 01_ldv_pam_filtered_season.rds
##         TABMON_PAMvsTerMap > 01_data > 02_ldv_termap_100m.rds
##         TABMON_PAMvsTerMap > 01_data > 03_ldv_pam_tw1_exact.rds
##         TABMON_PAMvsTerMap > 01_data > 04_ldv_pam_tw2_day.rds
##         TABMON_PAMvsTerMap > 01_data > 05_ldv_pam_tw3_week.rds
## -----------------------------

# Load config file
source("02_scripts/01_config.R")

# Load data
visits_data <- readRDS(visits_data_path)                    # Visits from territory mapping data (cleaned & enriched)
bugg_db_connect <- dbConnect(duckdb(), bugg_db_path)        # BUGG database connection (cleaned)

# Export Loenderveen only from `visits_data`
ldv_visits_data <- visits_data %>%
  filter(cluster == "Loenderveen") %>%
  select(plot_name, cluster, visit_id, date, start_time, end_time, visit_duration_minutes, 
         obs_id, scientific_name, english_name, territory_cluster, observation_type, breeding_code, 
         notes, start_datetime_local, end_datetime_local, start_datetime_utc, end_datetime_utc, geometry)
saveRDS(ldv_visits_data, file = file.path(project_root, data_dir, "01_ldv_termap.rds"))

# Query BUGG data
dbListTables(bugg_db_connect)                                                       # See what's inside the db
dbListFields(bugg_db_connect, "all_data_with_metadata")                             # List the columns of the table
dbGetQuery(bugg_db_connect, "SELECT * FROM all_data_with_metadata LIMIT 10")        # First rows

# ----------------------------------------------
# FILTER TERRITORY MAPPING SIGHTINGS TO 100m AROUND BUGGs
# ----------------------------------------------
# Get coordinates of individual sensors locations, projected to local EPSG

pam_sensors_sf <- ldv_pam_season %>%
  distinct(device_id, site, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs_projected)                                            # EPSG:28992 – Amersfoort / RD New, project for measuring buffers

pam_buffers <- pam_sensors_sf %>%                                        # Create the buffers; 100 meters radius
  st_buffer(dist = buffer100)

ldv_visits_data_proj <- ldv_visits_data %>%                              # Project territory mapping data to the same CRS
  st_transform(crs_projected)

sightings_within_100m <- ldv_visits_data_proj %>%                        # Spatially join the sightings falling within and exactly on those buffer zones
  st_join(pam_buffers, join = st_intersects, left = FALSE)                # left = FALSE drops non-matches

ldv_termap_filtered_100m <- sightings_within_100m %>%
  rename(pam_site = site) %>%                                            # Rename PAM site to avoid confusion with territory mapping sites
  relocate(device_id, pam_site, .after = visit_id)                       # Organise columns for clarity

# Checking that this looks good
ggplot() +
  geom_sf(data = pam_buffers, fill = "grey", color = "grey30", alpha = 0.3) +                 # Buffers
  geom_sf(data = pam_sensors_sf, color = "grey30", size = 2) +                                # Sensor points
  geom_sf(data = ldv_termap_filtered_100m, aes(color = scientific_name), size = 1) +          # Filtered TM sightings
  theme_minimal() +
  labs(title = "Territory mapping sightings within 100m of PAM sensors (Loenderveen)",
       color = "Species")

saveRDS(ldv_termap_filtered_100m, file = file.path(project_root, data_dir, "02_ldv_termap_100m.rds"))


# ----------------------------------------------
# TEMPORAL FILTERING: Define 'season' (from territory mapping visits)
# ----------------------------------------------

# Get the first and last territory mapping visit dates for Loenderveen
season_start <- min(ldv_visits_data$date, na.rm = TRUE)
season_end   <- max(ldv_visits_data$date, na.rm = TRUE)
message("Territory mapping season spans from ", season_start, " to ", season_end)

# Subset PAM data to this territory mapping season
ldv_pam_season <- dbGetQuery(
  bugg_db_connect,
  glue("
    SELECT *
    FROM all_data_with_metadata
    WHERE cluster = 'loenderveen'
      AND detection_time_utc >= '{season_start}'                  --- include detections from season_start onwards
      AND detection_time_utc <  '{season_end + 1}'                --- to season_end, including that day (else, using BETWEEN excludes it)
  ")
)

# Save baseline seasonal PAM subset
saveRDS(ldv_pam_season, file = file.path(project_root, data_dir, "03_ldv_pam_season.rds"))

# ----------------------------------------------
# TEMPORAL SUBSETS OF PAM DATA
# ----------------------------------------------

# Several time windows (centered around midpoint of territory mapping visits times) will be tested; (TW1) exact, (TW2) ±12h, (TW3) ±3.5 days.
ldv_termap_tw <- ldv_visits_data %>%
  st_drop_geometry() %>%   # drop sf geometry
  mutate(
    visit_mid_utc = start_datetime_utc + (end_datetime_utc - start_datetime_utc) / 2,
    # TW1: exact visit start–end
    tw1_start   = start_datetime_utc,
    tw1_end     = end_datetime_utc,
    # TW2: ±12h around midpoint
    tw2_start   = visit_mid_utc - hours(12),
    tw2_end     = visit_mid_utc + hours(12),
    # TW3: ±3.5 days (7d window) around midpoint
    tw3_start   = visit_mid_utc - hours(24 * 3.5), # 84 hours = 3.5 days
    tw3_end     = visit_mid_utc + hours(24 * 3.5)
  ) %>%
  select(visit_id, plot_name, date, starts_with("tw")) %>% # keep only relevant columns
  distinct() # remove duplicate rows (one per visit)

# Quick check
head(ldv_termap_tw, 3)

# Register ldv_termap_tw as a temporary table in DuckDB
duckdb_register(bugg_db_connect, "ldv_termap_tw", ldv_termap_tw)

# ----------------------------------------------
# PERFORM QUERIES
# ----------------------------------------------
# QUERYING HELPER FUNCTION
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

# PERFORM THE QUERIES
# Extract PAM detections for TW1, TW2, TW3, calling the function
ldv_pam_tw1 <- subset_pam_time_windows("tw1_start", "tw1_end")
ldv_pam_tw2 <- subset_pam_time_windows("tw2_start", "tw2_end")
ldv_pam_tw3 <- subset_pam_time_windows("tw3_start", "tw3_end")

# CHECKS BEFORE SAVING
ldv_pam_tw1 %>% count(visit_id, sort = TRUE) # check all visits are there (also for tw2, tw3)
# pick one visit and make sure PAM detections actually fall in the intended window:
example_id <- unique(ldv_pam_tw1$visit_id)[15]
ldv_pam_tw1 %>%
  filter(visit_id == example_id) %>%
  summarise(
    min_det = min(detection_time_utc),
    max_det = max(detection_time_utc)
  )
ldv_termap_tw %>%
  filter(visit_id == example_id) %>%
  select(tw1_start, tw1_end)

# Save PAM (temporal filtering only)
saveRDS(ldv_pam_tw1, file = file.path(data_dir, "04_ldv_pam_tw1_exact.rds"))
saveRDS(ldv_pam_tw2, file = file.path(data_dir, "05_ldv_pam_tw2_day.rds"))
saveRDS(ldv_pam_tw3, file = file.path(data_dir, "06_ldv_pam_tw3_week.rds"))
