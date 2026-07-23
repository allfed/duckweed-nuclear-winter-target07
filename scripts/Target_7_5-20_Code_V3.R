# ============================================================
# Target 7 / 150 Tg Nuclear Winter Duckweed Spatial Model
# Years 0005-0020 after Year 0 soot injection
# Outputs annual climatic potential yield maps
# Units: metric tons / ha / year
# ============================================================

# 0. INITIALIZATION
rm(list = ls())
gc()

library(terra)

# 1. FILE PATHS
input_folder <- "C:/Users/wsad4/Downloads/Duckweed_Project/nw_ur_150_07_mini"

output_folder <- "C:/Users/wsad4/Downloads/Duckweed_Project/Target7_Carryover"
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# 2. EXPECTED YEARS / MONTHS
expected_years <- 5:20
expected_months <- 1:12
standard_months <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# Find all NetCDF files
all_files <- list.files(path = input_folder, pattern = "\\.nc$", full.names = TRUE)

if (length(all_files) == 0) {
  stop("No .nc files found. Check input_folder.")
}

# Extract year and month from filenames like:
# nw_ur_150_07.cam.h0.0005-01.nc
file_info <- data.frame(
  file = all_files,
  name = basename(all_files),
  stringsAsFactors = FALSE
)

file_info$Year <- suppressWarnings(
  as.integer(sub(".*\\.(\\d{4})-(\\d{2})\\.nc$", "\\1", file_info$name))
)

file_info$Month <- suppressWarnings(
  as.integer(sub(".*\\.(\\d{4})-(\\d{2})\\.nc$", "\\2", file_info$name))
)

# Keep only files matching the expected year/month format
file_info <- file_info[!is.na(file_info$Year) & !is.na(file_info$Month), ]

# Keep only intended years 0005-0020
file_info <- file_info[file_info$Year %in% expected_years, ]

# Check for duplicate year-month files
year_month_key <- paste(file_info$Year, file_info$Month, sep = "-")
if (any(duplicated(year_month_key))) {
  stop("Duplicate year-month NetCDF files found. Clean the input folder before running.")
}

# Check that all expected files exist
expected_table <- expand.grid(
  Year = expected_years,
  Month = expected_months
)

file_table <- merge(
  expected_table,
  file_info[, c("Year", "Month", "file")],
  by = c("Year", "Month"),
  all.x = TRUE
)

missing_files <- file_table[is.na(file_table$file), ]

if (nrow(missing_files) > 0) {
  missing_list <- paste(
    sprintf("%04d-%02d", missing_files$Year, missing_files$Month),
    collapse = ", "
  )
  stop(paste("Missing expected monthly files:", missing_list))
}

file_table <- file_table[order(file_table$Year, file_table$Month), ]

print(paste("Detected", nrow(file_table), "monthly files."))
print(paste("Detected years:", paste(expected_years, collapse = ", ")))

# 3. BIOLOGICAL CONSTANTS
R <- 0.62
teta_1 <- 0.0025
teta_2 <- 0.66
teta_3 <- 0.0073
teta_4 <- 0.65

T_opt <- 26
E_opt <- 13

C_P <- 14
K_P <- 0.31
K_IP <- 101

C_N <- 90
K_N <- 0.95
K_IN <- 604

D_thresh <- 99
H_ratio <- 0.2
DL_dry <- 176
D_initial <- 0.1

A0 <- 0.222
A1 <- 0.05
A2 <- 0.681

light_epsilon <- 0.001
harvest_every_n_days <- 2

# [v3] RUN MODE
# FALSE = carryover: biomass persists across all 16 years (primary recovery run).
# TRUE  = annual reset: biomass re-initialized to D_initial each year, matching
#         the StellaR pipeline. Use this mode only to validate against the paper.
use_annual_reset <- FALSE

print(paste(
  "MODE:",
  if (use_annual_reset) "ANNUAL RESET (validation)" else "CARRYOVER (primary)"
))

# 4. INITIALIZE GLOBAL RASTERS
first_file <- file_table$file[1]

base_t <- rast(first_file, subds = "T")[["T_lev=992.5561"]]
ext(base_t) <- c(0, 360, -90, 90)
crs(base_t) <- "epsg:4326"
base_t <- rotate(base_t)

# [v3] Guard: these files are expected to be single-timestep monthly means, so
# selecting the near-surface T level and reading FSDS should each give 1 layer.
# If a file ever carries multiple timesteps, [[1]] downstream would silently
# grab only the first; stop instead so the mismatch is caught.
if (nlyr(base_t) != 1) {
  stop(paste(
    "Expected 1 near-surface T layer per file, got", nlyr(base_t),
    "- check the timestep count in the NetCDF files."
  ))
}
base_fsds_check <- rast(first_file, subds = "FSDS")
if (nlyr(base_fsds_check) != 1) {
  stop(paste(
    "Expected 1 FSDS layer per file, got", nlyr(base_fsds_check),
    "- check the timestep count in the NetCDF files."
  ))
}
rm(base_fsds_check)

# [v3] One-time georeferencing check. The extent is set by hand because the
# container reports no geotransform, so print the frame to confirm it. The
# coastline overlay in the map script is the visual confirmation of orientation.
print(paste("Grid dimensions (rows x cols):", nrow(base_t), "x", ncol(base_t)))
print("Extent after rotate:")
print(ext(base_t))

# Density persists continuously across all years (unless reset mode is on)
density_map <- base_t * 0 + D_initial

# Latitude map for photoperiod calculations
lat_map <- init(density_map, "y")

rm(base_t)
gc(verbose = FALSE)

global_day <- 0

# [v3] Global mean dropped: it averaged over the whole globe including ocean,
# which is not a meaningful yield and has no counterpart in the paper (which
# reports discrete land grid points). Peak is kept; grid-point values are
# extracted in the map script.
annual_results <- data.frame(
  Year = expected_years,
  PeakYield_MT_ha_yr = NA_real_
)

# 5. MULTI-YEAR SIMULATION LOOP
for (idx in seq_along(expected_years)) {
  
  y <- expected_years[idx]
  year_str <- sprintf("%04d", y)
  
  print("==================================================")
  print(paste("STARTING SIMULATION FOR YEAR", year_str))
  print("==================================================")
  
  # [v3] In reset mode, re-initialize biomass each year the way StellaR does.
  # In carryover mode (default) density_map is left to persist.
  if (use_annual_reset) {
    density_map <- density_map * 0 + D_initial
  }
  
  # [v3] Day-of-year counter, reset each year (used for harvest timing in reset
  # mode). global_day keeps running continuously as before.
  day_of_year <- 0
  
  # Reset annual harvested yield
  yield_sum <- density_map * 0
  
  # Get 12 monthly files for this year
  year_files <- file_table[file_table$Year == y, ]
  year_files <- year_files[order(year_files$Month), ]
  
  # 6. MONTHLY CLIMATE LOOP
  for (m in expected_months) {
    
    current_file <- year_files$file[year_files$Month == m]
    sim_days <- standard_months[m]
    
    print(paste("Processing Year", year_str, "Month", sprintf("%02d", m)))
    
    # Temperature: near-surface WACCM level, Kelvin -> Celsius
    t_stack <- rast(current_file, subds = "T")[["T_lev=992.5561"]]
    ext(t_stack) <- c(0, 360, -90, 90)
    crs(t_stack) <- "epsg:4326"
    t_stack <- rotate(t_stack) - 273.15
    Temp <- t_stack[[1]]   # single timestep (checked in init)
    
    # Light: FSDS W/m2 -> PAR-like LI using 2.02 conversion
    r_stack <- rast(current_file, subds = "FSDS")
    ext(r_stack) <- c(0, 360, -90, 90)
    crs(r_stack) <- "epsg:4326"
    r_stack <- rotate(r_stack)
    LI <- r_stack[[1]] * 2.02   # single timestep (checked in init)
    
    # 7. DAILY BIOLOGY LOOP
    for (i in 1:sim_days) {
      
      global_day <- global_day + 1
      day_of_year <- day_of_year + 1   # [v3]
      
      # [v3] Harvest timing. Carryover uses the continuous day count (phase
      # drifts slightly across years); reset uses day-of-year so the harvest
      # phase repeats each year, matching StellaR's independent per-year runs.
      harvest_counter <- if (use_annual_reset) day_of_year else global_day
      
      # Daily photoperiod calculation
      # (declination is periodic in 365 days, so global_day and day-of-year
      #  give identical values here)
      declination <- 23.45 * sin(pi / 180 * (360 / 365 * (global_day + 284)))
      arg <- clamp(
        -tan(lat_map * pi / 180) * tan(declination * pi / 180),
        -1,
        1
      )
      E <- (24 / pi) * acos(arg)
      
      # Intrinsic growth rate
      # Temp <= 5 C means growth shutdown, not biomass reset
      ri <- ifel(
        Temp > 5,
        (
          R *
            teta_1^(((Temp - T_opt) / T_opt)^2) *
            teta_2^((Temp - T_opt) / T_opt) *
            teta_3^(((E - E_opt) / E_opt)^2) *
            teta_4^((E - E_opt) / E_opt) *
            (C_P / (C_P + K_P)) *
            (K_IP / (K_IP + C_P)) *
            (C_N / (C_N + K_N)) *
            (K_IN / (K_IN + C_N)) +
            A0
        ) *
          A1 *
          ((log(LI + light_epsilon) - A2) / A2),
        0
      )
      
      # Discrete logistic density update
      density_map <- (DL_dry * density_map) /
        ((DL_dry - density_map) * exp(-ri) + density_map)
      
      # Harvest every other day, only if warm enough and above threshold
      harvest <- ifel(
        (Temp > 5) &
          (density_map > D_thresh) &
          (harvest_counter %% harvest_every_n_days == 0),   # [v3] counter
        density_map * H_ratio,
        0
      )
      
      density_map <- density_map - harvest
      yield_sum <- yield_sum + harvest
    }
    
    rm(t_stack, r_stack, Temp, LI, declination, arg, E, ri, harvest)
    gc(verbose = FALSE)
  }
  
  # 8. ANNUAL EXPORT
  print(paste("Finalizing Year", year_str, "..."))
  
  # g/m2 -> metric tons/ha
  final_yield_nw <- (yield_sum * 10000) / 1000000
  
  output_name <- file.path(output_folder, paste0("NW_Yield_Year_", year_str, ".tif"))
  writeRaster(final_yield_nw, output_name, overwrite = TRUE)
  
  peak_yield <- global(final_yield_nw, "max", na.rm = TRUE)[1, 1]   # [v3] peak only
  annual_results$PeakYield_MT_ha_yr[idx] <- peak_yield
  
  print(paste(
    "Year", year_str,
    "Peak Yield:", round(peak_yield, 2), "MT/ha/yr",
    "| Saved as", output_name
  ))
  
  write.csv(
    annual_results,
    file.path(output_folder, "NW_Annual_Summary_Years_05_20.csv"),
    row.names = FALSE
  )
  
  rm(yield_sum, final_yield_nw)
  gc(verbose = FALSE)
}

# 9. FINAL SUMMARY
print("==================================================")
print("ALL YEARS COMPLETE: RECOVERY SUMMARY")
print("==================================================")
print(annual_results)

write.csv(
  annual_results,
  file.path(output_folder, "NW_Annual_Summary_Years_05_20.csv"),
  row.names = FALSE
)

print(paste("All outputs saved in:", output_folder))
