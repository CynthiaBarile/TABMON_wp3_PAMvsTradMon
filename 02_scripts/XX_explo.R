## -----------------------------
## TABMON_PAMvsTerMap.Rproject
## ??_exploration.R
## 
## Input:  01_data > 
## Output: 
## -----------------------------

source("02_scripts/01_config.R")
termap_data <- readRDS(termap_data_path)                      # Full territory mapping dataset
visits_data <- readRDS(visits_data_path)                      # Visits only
# ldv_pam_season <- readRDS(ldv_pam_season_path)              # PAM data filtered around territory mapping season - Loenderveen

## For now, easier to re-open this. Later, might be better to move to the duckdb site_metadata sheet when updated
library(googlesheets4)
sheet_url <- "https://docs.google.com/spreadsheets/d/1Wqz7g0I2cqoMFSI-CkN3Fss1h62ceoos6_OtARi490c/edit?gid=208765239#gid=208765239" # BUGG sheet
sensor_metadata <- read_sheet(sheet_url, sheet = "Deployments details")

# Convert to sf object using WGS84 (standard lon-lat CRS)
sensors_sf <- sensor_metadata %>%
  filter(Active == TRUE) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#############################################
## --- All clusters' territory mapping ---
#############################################

# Function to build cluster map

build_cluster_map <- function(cluster_name, datasets) {
  pal <- brewer.pal(5, "Set2")
  
  # Filter sensor points for the cluster
  sensors_cluster <- sensors_sf %>% filter(cluster == cluster_name)
  # Project sensors to metric CRS for buffering (EPSG:3857)
  sensors_proj <- st_transform(sensors_cluster, 3857)
  buffer_100m <- st_buffer(sensors_proj, dist = buffer100) %>% st_transform(4326)
  
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addPolygons(
      data = datasets$plots_sf %>% filter(cluster == cluster_name),
      color = pal[1], weight = 1, fillOpacity = 0.4,
      group = "Plots"
    ) %>%
    
    addPolylines(
      data = datasets$transects_sf %>% filter(cluster == cluster_name),
      color = pal[2], weight = 2,
      group = "Transects"
    ) %>%
    
    # Visits with popup
    addCircleMarkers(
      data = datasets$visits_sf_en %>% filter(cluster == cluster_name),
      radius = 2, color = pal[3], fillOpacity = 0.3, group = "Visits",
      popup = ~paste0(
        "<b>Species:</b> ", english_name, "<br>",
        "<b>Date:</b> ", date, "<br>"
      )
    ) %>%
    
    addCircleMarkers(
      data = datasets$territories_sf %>% filter(cluster == cluster_name),
      color = pal[4], radius = 1, fillOpacity = 0.4,
      group = "Territories"
    ) %>%
    
    addPolygons(
      data = buffer_100m,
      fillColor = pal[5],
      fillOpacity = 0.2,
      stroke = FALSE,
      group = "Sensors: 100m buffer"
    ) %>%

    addCircleMarkers(
      data = sensors_cluster,
      color = pal[5],
      radius = 6,
      fillOpacity = 1,
      stroke = FALSE,
      popup = ~as.character(TABMONDeploymentID),
      group = "Sensors"
    ) %>%
    
    addLayersControl(
      overlayGroups = c("Plots", "Transects", "Visits", "Territories", 
                        "Sensors: 100m buffer", "Sensors"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    
    addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
}

map_AWD <- build_cluster_map("Amsterdamse Waterleidingduinen", termap_data)
map_LDV <- build_cluster_map("Loenderveen", termap_data)
map_OVP <- build_cluster_map("Oostvaardersplassen", termap_data)
map_HV <- build_cluster_map("Hoge Veluwe", termap_data)
map_OL <- build_cluster_map("De Onlanden", termap_data)

#############################################
## --- Timing of visits ---
#############################################

# For Loenderveen
ldv_visits_data <- visits_data %>%
  filter(cluster == "Loenderveen") %>%
  distinct(visit_id, .keep_all = TRUE) %>%        # only keep one row per visit, keep all columns
  group_by(date) %>%                             # group visit by date
  arrange(start_time) %>%                        # order visits chronologically
  mutate(visit_order = row_number()) %>%         # add number to indicate order
  ungroup()

# Create a distinct dates data frame for labeling
# Find the rightmost 'end' time per date for placing the label
date_labels <- ldv_visits_data %>%             # create df with distinct date for labels
  group_by(date) %>%
  summarise(max_end = max(end_time), .groups = 'drop') %>%
  mutate(label_x = max_end + as_hms("00:20:00"))  # shift label 5 minutes to the right

ggplot(ldv_visits_data, aes(y = date)) +
  geom_linerange(
    aes(xmin = start_time, xmax = end_time, color = factor(visit_order)),
    linewidth = 3, alpha = 0.6
  ) +
  geom_point(
    aes(x = start_time, color = factor(visit_order)),
    shape = 21, fill = "white", size = 3, stroke = 1
  ) +
  geom_point(
    aes(x = end_time, color = factor(visit_order)),
    size = 3
  ) +
  geom_text(
    data = date_labels,
    aes(x = label_x, y = date, label = format(date, "%d %b")),  # no year
    inherit.aes = FALSE,
    hjust = 0, size = 3.5, color = "black"
  ) +
  scale_x_time(
    breaks = breaks_width("1 hour"),
    labels = label_time("%H:%M"),
    limits = c(as_hms("04:45:00"), as_hms("23:45:00")),
    expand = expansion(mult = c(0.01, 0.06))
  ) +
  scale_color_brewer(palette = "Set2", name = "Visit #") +
  scale_y_date(
    date_breaks = "1 month",
    date_labels = "%b",
    expand = expansion(mult = c(0.03, 0.01))
  ) +
  labs(
    title = "Survey dates & times — Loenderveen",
    x = "Time of Day",
    y = "Month"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted", color = "grey80"),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 15, face = "bold", margin = margin(b = 10))
  )




### random things from before

# Summary of visits
summary_stats <- termap_data$Overview %>%
  summarise(
    n_visits = n_distinct(visit_id),
    n_days = n_distinct(date),
    n_observers = n_distinct(observer_id),
    total_duration_h = sum(visit_duration_minutes, na.rm = TRUE) / 60,
    average_distinct_species = mean(n_species, na.rm = TRUE),
    total_records = sum(n_records, na.rm = TRUE)
  )
print(summary_stats)



