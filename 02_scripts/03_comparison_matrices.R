## -----------------------------
## TABMON_PAMvsTerMap.Rproject
## 03_comparison_matrices.R
## 🧮 Building presence/absence matrices for the different levels of investigation
## Input:  TABMON_PAMvsTerMap > 01_data > 01_ldv_termap
##         TABMON_PAMvsTerMap > 01_data > 02_ldv_pam_season
##         TABMON_PAMvsTerMap > 01_data > 03_ldv_pam_tw1_exact
##         TABMON_PAMvsTerMap > 01_data > 04_ldv_pam_tw2_day
##         TABMON_PAMvsTerMap > 01_data > 05_ldv_pam_tw3_week
##         TABMON_PAMvsTerMap > 01_data > 06_ldv_termap_100m
## -----------------------------

source("02_scripts/01_config.R")

# Load data
ldv_visits <- readRDS(ldv_termap_path) 
ldv_visits_100m <- readRDS(ldv_termap_100m_path) 
ldv_pam_season <- readRDS(ldv_pam_season_path) 
ldv_pam_tw1 <- readRDS(ldv_pam_tw1_exact_path) 
ldv_pam_tw2 <- readRDS(ldv_pam_tw2_day_path) 
ldv_pam_tw3 <- readRDS(ldv_pam_tw3_week_path) 

# ----------------------------------------------
# HELPER FUNCTIONS & OBJECTS
# ----------------------------------------------

# Build presence/absence matrix
build_pa_matrix <- function(df, row_unit, species_col = "scientific_name") {
  df_distinct <- df %>%
    distinct(.data[[row_unit]], .data[[species_col]]) %>%                       # Collapse duplicates (i.e. keep one row per row_unit and species
    mutate(presence = 1)                                                        # Add a "presence" column (filled with 1) to indicate species was detected
    
  matrix_wide <- df_distinct %>%
    pivot_wider(names_from = all_of(species_col),                               # Pivot from long to wide: one column per species; species names become column names
                values_from = presence,                                         # Values come from "presence" column
                values_fill = list(presence = 0)) %>%                           # Fill missing combinations with 0
    arrange(.data[[row_unit]])                                                  # Arrange rows by the row unit for consistency
    
  matrix_df <- as.data.frame(matrix_wide)                                       # Convert tibble to dataframe
  rownames(matrix_df) <- matrix_df[[row_unit]]                                  # Set rownames
  matrix_df[[row_unit]] <- NULL                                                 # Remove original row_unit column, because info is now in row names
  return(matrix_df)                                                             # Return the matrix
}

# Align species columns between PAM & TM
align_cols <- function(matrix1, matrix2) {
  all_spp <- union(colnames(matrix1), colnames(matrix2))                        # Get full list of species from both matrices
  for(sp in setdiff(all_spp, colnames(matrix1))) matrix1[[sp]] <- 0             # Add missing species columns to matrix1, fill with 0
  for(sp in setdiff(all_spp, colnames(matrix2))) matrix2[[sp]] <- 0             # Same for matrix2
  matrix1 <- matrix1[, sort(colnames(matrix1))]                                 # Sort columns alphabetically
  matrix2 <- matrix2[, sort(colnames(matrix2))]
  list(matrix1 = matrix1, matrix2 = matrix2)                                    # Return aligned matrices as list
}

# Define inputs for the different levels of analysis
pam_subsets <- list(
  sensor_tw1      = ldv_pam_tw1,               # PAM, sensor-level, exact visit
  sensor_tw2      = ldv_pam_tw2,               # PAM, sensor-level, ±12 h
  sensor_tw3      = ldv_pam_tw3,               # PAM, sensor-level, ±3.5 d
  sensor_season   = ldv_pam_season,            # PAM, sensor-level, full season
  cluster_tw1     = ldv_pam_tw1,               # PAM, cluster-level, exact visit
  cluster_tw2     = ldv_pam_tw2,               # PAM, cluster-level, ±12 h
  cluster_tw3     = ldv_pam_tw3,               # PAM, cluster-level, ±3.5 d
  cluster_season  = ldv_pam_season)            # PAM, cluster-level, full season

tm_subsets <- list(
  sensor_tw1      = ldv_visits_100m,           # TM, sensor-level
  sensor_tw2      = ldv_visits_100m,
  sensor_tw3      = ldv_visits_100m,
  sensor_season   = ldv_visits_100m,
  cluster_tw1     = ldv_visits,                # TM, cluster-level (full area)
  cluster_tw2     = ldv_visits,
  cluster_tw3     = ldv_visits,
  cluster_season  = ldv_visits)

row_units <- list(
  sensor_tw1      = "device_id",               # sensor-level rows
  sensor_tw2      = "device_id",
  sensor_tw3      = "device_id",
  sensor_season   = "device_id",
  cluster_tw1     = "date",                    # cluster-level per visit day rows
  cluster_tw2     = "date",
  cluster_tw3     = "date",
  cluster_season  = "cluster")                 # cluster-level seasonal = single row

# ----------------------------------------------
# BUILD AND ALIGN MATRICES
# ----------------------------------------------
matrix_list <- list()

for(subset in names(pam_subsets)) {
  pam_matrix <- build_pa_matrix(pam_subsets[[subset]], row_units[[subset]])           # Build PAM matrix for this level
  tm_matrix  <- build_pa_matrix(tm_subsets[[subset]], row_units[[subset]])            # Build TM matrix for this level
  
  aligned <- align_cols(pam_matrix, tm_matrix)                                        # Align species columns between PAM and TM matrices
  pam_matrix <- aligned$matrix1
  tm_matrix  <- aligned$matrix2
  
  matrix_list[[paste0(subset, "_pam")]] <- pam_matrix                                 # Save with informative names
  matrix_list[[paste0(subset, "_tm")]]  <- tm_matrix
}

sapply(matrix_list, dim)

# This helps track which PAM/TM matrix corresponds to each analysis level
summary_tbl <- tibble(
  matrix_name   = names(matrix_list),
  n_rows        = sapply(matrix_list, nrow),
  n_cols        = sapply(matrix_list, ncol)) %>%
  mutate(
    # Extract the subset key (e.g. "sensor_tw1") from names like "sensor_tw1_pam"
    subset_key = sub("_(pam|tm)$", "", matrix_name),
    method     = ifelse(str_ends(matrix_name, "pam"), "PAM", "TM"),
    level      = ifelse(str_starts(matrix_name, "sensor"), "Sensor-level", "Cluster-level"),
    window     = case_when(
      str_detect(matrix_name, "tw1")    ~ "exact visit",
      str_detect(matrix_name, "tw2")    ~ "±12 h",
      str_detect(matrix_name, "tw3")    ~ "±3.5 d",
      str_detect(matrix_name, "season") ~ "full season"),
    row_unit   = case_when(
      level == "Sensor-level"                            ~ "device_id",
      window == "full season" & level == "Cluster-level" ~ "cluster",
      TRUE                                               ~ "date"),
    
    # Names of the original data subsets used to build this matrix
    pam_subset = names(pam_subsets)[match(subset_key, names(pam_subsets))],
    tm_subset  = names(tm_subsets)[match(subset_key, names(tm_subsets))]) %>%
  select(matrix_name, method, level, window, row_unit,
         pam_subset, tm_subset, n_rows, n_cols) %>%
  arrange(level, window, method)

summary_tbl

# Quick checks
head(matrix_list[["sensor_tw1_pam"]])
head(matrix_list[["sensor_tw1_tm"]])

# Make sure TM and PAM for the same subsets overlap perfectly
m1 <- matrix_list[["sensor_tw1_pam"]]
m2 <- matrix_list[["sensor_tw1_tm"]]
identical(colnames(m1), colnames(m2))  # should be TRUE

# Species richness per sensor
rowSums(matrix_list[["sensor_tw1_pam"]])  # number of species per sensor (PAM)
rowSums(matrix_list[["sensor_tw1_tm"]])   # number of species per sensor (TM)

saveRDS(matrix_list, file = file.path(project_root, data_dir, "07_presence_absence_matrices.rds"))
