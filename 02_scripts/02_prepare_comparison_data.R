## -----------------------------
## 02_prepare_comparison_data.R
## 🦆 Loads cleaned acoustic data database
## 📌 Loads cleaned territory mapping data
## 🆚 Prepare for comparison using Loenderveen as case study
## Input:  01_data > 
## Output: 01_data > 
## -----------------------------

# ----------------------------------
# LOADING LIBRARIES, CONFIG, DATA
# ----------------------------------

# Load config file
source("02_scripts/01_config.R")

# Load data
visits_data <- readRDS(visits_data_path)                    # Visits from territory mapping data (cleaned & enriched)
bugg_db_connect <- dbConnect(duckdb(), bugg_db_path)        # BUGG database connection (cleaned)

# Export Loenderveen only from `visits_data`
ldv_visits_data <- visits_data %>%
  filter(cluster == "Loenderveen") %>%
  select(plot_name, visit_id, date, start_time, end_time, visit_duration_minutes, 
         obs_id, scientific_name, english_name, observation_type, breeding_code, 
         notes, territory_cluster, x_coord, y_coord, geometry)

# Query BUGG data
dbListTables(db_connect)                                                       # See what's inside the db
dbListFields(db_connect, "all_data_with_metadata")                             # List the columns of the table
dbGetQuery(db_connect, "SELECT * FROM all_data_with_metadata LIMIT 10")        # First rows

# ----------------------------------------------
# SUBSET PAM DATA TO TERRITORY MAPPING PERIODS
# ----------------------------------------------

# Get a df of unique time interval of +/- 24 hours around territory mapping visits
tm_windows <- ldv_visits_data %>%
  mutate(
    visit_start  = as_datetime(date) + start_time,
    visit_end    = as_datetime(date) + end_time,
    window_start = visit_start - hours(24),                             # 24 hours before the start of TM visits
    window_end   = visit_end + hours(24)                                # 24 hours after the end of TM visits
  ) %>%
  distinct(visit_id, window_start, window_end)                          # Deduplicate

dbWriteTable(db_connect, "tm_visit_windows", tm_windows, temporary = TRUE)     # Write as temporary table in database

# Proceed to filtering the db for data matching those time intervals, in Loenderveen only
ldv_pam_filtered <- dbGetQuery(db_connect, "                                
  SELECT pam.*                                                        -- Select all columns (*) from the BUGG table
  FROM all_data_with_metadata pam                                     -- Use the table all_data_with_metadata, and assign it the alias BUGG
  JOIN tm_visit_windows win                                           -- Join this with the tm_visit_windows table (alias: win)
    ON pam.detection_time_utc 
    BETWEEN win.window_start AND win.window_end                       -- Only keep BUGG rows where detection time falls within any TM visit window
  WHERE pam.cluster = 'loenderveen'                                   -- Only keep BUGG rows from the 'loenderveen' cluster
")

saveRDS(ldv_pam_filtered, file = file.path(processed_data_dir, "ldv_pam_filtered.rds"))
# This file containing Loenderveen data temporally filtered to contain detections occurring only within ±24h of individual TM visits.

# ----------------------------------------------
# FILTER TM SIGHTINGS TO 100m AROUND BUGGs
# ----------------------------------------------
# Get coordinates of individual sensors locations, projected to local EPSG

pam_sensors_sf <- ldv_pam_filtered %>%
  distinct(device_id, site, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs_projected)                                            # EPSG:28992 – Amersfoort / RD New, project for measuring buffers

pam_buffers <- pam_sensors_sf %>%                                        # Create the buffers; 100 meters radius
  st_buffer(dist = buffer)

ldv_visits_data_proj <- ldv_visits_data %>%                              # Project territory mapping data to the same CRS
  st_transform(crs_projected)

sightings_within_100m <- ldv_visits_data_proj %>%                        # Join spatially the sightings falling within those buffer zones
  st_join(pam_buffers, join = st_within, left = FALSE)                   # left = FALSE drops non-matches

ldv_tm_filtered <- sightings_within_100m %>%
  rename(pam_site = site) %>%                                            # Rename PAM site to avoid confusion with territory mapping sites
  relocate(device_id, pam_site, .after = visit_id)                       # Organise columns for clarity

# Checking that this looks good
ggplot() +
  geom_sf(data = pam_buffers, fill = "grey", color = "grey30", alpha = 0.3) +        # Buffers
  geom_sf(data = pam_sensors_sf, color = "grey30", size = 2) +                       # Sensor points
  geom_sf(data = ldv_tm_filtered, aes(color = scientific_name), size = 1) +          # Filtered TM sightings
  theme_minimal() +
  labs(title = "TM sightings within 100m of PAM sensors (Loenderveen)",
       color = "Species")

saveRDS(ldv_tm_filtered, file = file.path(processed_data_dir, "ldv_tm_filtered.rds"))
