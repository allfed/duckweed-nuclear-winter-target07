# ============================================================
# Target 7 / 150 Tg Nuclear Winter Duckweed Map Summaries
# Uses annual maps from Years 0005-0020
# Outputs:
#   1. Cumulative yield over 16 years
#   2. Average annual yield over 16 years
#   3. Grid-point validation (yields at the paper's grid locations)
# ============================================================

rm(list = ls())
gc()

library(terra)

if (!require("maps")) {
  install.packages("maps", quiet = TRUE)
}
library(maps)

# 1. INPUT / OUTPUT FOLDER
tif_folder <- "C:/Users/wsad4/Downloads/Duckweed_Project/Target7_Carryover"

expected_years <- 5:20
n_years <- length(expected_years)

# [v3] Grid-point file for validation (yields at the paper's locations).
# Set the two column names below to whatever your CSV uses.
grid_file <- "C:/Users/wsad4/Downloads/Duckweed_Project/Grid_latlong.csv"
grid_lat_col <- "Latitude"    # matches Grid_latlong.csv header
grid_lon_col <- "Longitude"   # matches Grid_latlong.csv header

# 2. FIND EXPECTED YEARLY MAPS
tif_files <- file.path(
  tif_folder,
  paste0("NW_Yield_Year_", sprintf("%04d", expected_years), ".tif")
)

missing_tifs <- tif_files[!file.exists(tif_files)]

if (length(missing_tifs) > 0) {
  stop(paste(
    "Missing yearly GeoTIFF files:",
    paste(basename(missing_tifs), collapse = ", ")
  ))
}

print(paste("Found", length(tif_files), "yearly maps."))
print(paste("Calculating cumulative and average annual yield for Years 0005-0020:", n_years, "years."))

# 3. LOAD YEARLY MAPS
yield_stack <- rast(tif_files)
names(yield_stack) <- paste0("Year_", sprintf("%04d", expected_years))   # [v3]

# 4. CUMULATIVE TOTAL
# Input yearly maps are MT/ha/year.
# Sum over 16 years gives MT/ha over the full Years 5-20 period.
cumulative_total_yield <- sum(yield_stack, na.rm = TRUE)

cumulative_output <- file.path(
  tif_folder,
  "NW_Cumulative_Yield_Years_05_20_MT_ha.tif"
)

writeRaster(cumulative_total_yield, cumulative_output, overwrite = TRUE)

# 5. AVERAGE ANNUAL YIELD
# [v3] Divide by the per-pixel count of non-NA years, not a hardcoded 16, so a
# missing pixel-year isn't silently treated as a zero. With complete data this
# equals dividing by n_years.
n_valid <- sum(!is.na(yield_stack))
average_annual_yield <- cumulative_total_yield / n_valid
average_annual_yield[n_valid == 0] <- NA

average_output <- file.path(
  tif_folder,
  "NW_Average_Annual_Yield_Years_05_20_MT_ha_yr.tif"
)

writeRaster(average_annual_yield, average_output, overwrite = TRUE)

# 6. SUMMARY STATS
# [v3] Peak only. Global mean dropped (ocean-contaminated; no counterpart in the
# paper). For a central-tendency number, use the grid-point values in step 8.
peak_cumulative <- global(cumulative_total_yield, "max", na.rm = TRUE)[1, 1]
peak_average <- global(average_annual_yield, "max", na.rm = TRUE)[1, 1]

summary_table <- data.frame(
  Metric = c(
    "Peak cumulative yield",
    "Peak average annual yield"
  ),
  Value = c(
    peak_cumulative,
    peak_average
  ),
  Units = c(
    "MT/ha over Years 5-20",
    "MT/ha/yr averaged over Years 5-20"
  )
)

print(summary_table)

write.csv(
  summary_table,
  file.path(tif_folder, "NW_Cumulative_and_Average_Summary_Years_05_20.csv"),
  row.names = FALSE
)

# 7. PLOTS
# The coastline overlay is also the georeferencing check: if the yield pattern
# lines up with the continents, the extent/orientation is right.
plot(
  cumulative_total_yield,
  main = "Cumulative Duckweed Yield, Years 5-20",
  axes = TRUE
)
map("world", add = TRUE, col = "white", lwd = 0.8)

plot(
  average_annual_yield,
  main = "Average Annual Duckweed Yield, Years 5-20",
  axes = TRUE
)
map("world", add = TRUE, col = "white", lwd = 0.8)

# 8. GRID-POINT VALIDATION
# [v3] Pull each year's yield at the paper's grid locations so the 16-year
# trajectory can be compared to the paper (esp. Target 07, Fig. 7). These are
# the base grid points; the paper nudged them slightly for WACCM's coarser grid,
# so treat them as approximate locators rather than exact cells.
if (!file.exists(grid_file)) {
  warning(paste("Grid point file not found, skipping validation extract:", grid_file))
} else {
  grid_pts <- read.csv(grid_file)
  
  if (!all(c(grid_lat_col, grid_lon_col) %in% names(grid_pts))) {
    stop(paste0(
      "Grid_latlong.csv columns not found. Looked for '", grid_lat_col,
      "' and '", grid_lon_col, "'. Columns present: ",
      paste(names(grid_pts), collapse = ", ")
    ))
  }
  
  # terra::extract wants coordinates as (x = lon, y = lat)
  pts_xy <- cbind(grid_pts[[grid_lon_col]], grid_pts[[grid_lat_col]])
  extracted <- terra::extract(yield_stack, pts_xy)
  
  grid_validation <- data.frame(
    Grid = seq_len(nrow(grid_pts)),
    Lat = grid_pts[[grid_lat_col]],
    Long = grid_pts[[grid_lon_col]]
  )
  grid_validation <- cbind(grid_validation, round(extracted, 3))
  
  write.csv(
    grid_validation,
    file.path(tif_folder, "NW_Grid_Point_Validation.csv"),
    row.names = FALSE
  )
  
  print("Grid-point validation written: NW_Grid_Point_Validation.csv")
}

print(paste("Cumulative map saved as:", cumulative_output))
print(paste("Average annual map saved as:", average_output))
print(paste("Summary CSV saved in:", tif_folder))
