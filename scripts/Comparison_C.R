# ============================================================
# Comparison C:
# Fair validation of OUR DISCRETE/SPATIAL MODEL LOGIC
# against Femeena/StellaR RCP8.5 annual outputs.
#
# Purpose:
#   Test whether our discrete nuclear-winter-style calculation
#   can reproduce Femeena/StellaR annual yield values after
#   controlling comparison-specific mismatches.
#
# Main fair-validation assumptions:
#   - Use prepared Excel Rsds sheet directly as LI.
#     Do NOT multiply by 2.02 again.
#   - Reset density each grid-year, because StellaR does.
#   - Use daily harvest in main comparison, because StellaR code
#     uses Harvest_frequency = 1.0.
#   - Keep our discrete logistic update, because this is the
#     core logic being validated.
#   - Temp <= 5 C means growth/harvest shutdown, not biomass death,
#     because shared StellaR code behaves this way.
#
# Outputs:
#   1. Detailed grid-year comparison CSV
#   2. Model-level summary CSV
#   3. Grid-level summary CSV
#   4. Year-level summary CSV
#   5. StellaR target diagnostics
#   6. Scatter plots
#   7. Annual mean trajectory plots
# ============================================================

rm(list = ls())
gc()

# -----------------------------
# 1. PACKAGES
# -----------------------------
if (!require("openxlsx")) install.packages("openxlsx", quiet = TRUE)
library(openxlsx)

# -----------------------------
# 2. FILE PATHS
# -----------------------------
# Change this only if your Duckweed_Project folder is elsewhere.
data_folder <- "C:/Users/wsad4/Downloads/Duckweed_Project"

grid_file    <- file.path(data_folder, "Grid_latlong.csv")
year_file    <- file.path(data_folder, "Year_rows.xlsx")
climate_file <- file.path(data_folder, "AllData_Rcp85_Tavg.xlsx")
result_file  <- file.path(data_folder, "Results_Rcp85_Tavg.csv")

output_folder <- file.path(data_folder, "Comparison_C_Fair_Discrete_Validation")
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 3. CHECK FILES EXIST
# -----------------------------
required_files <- c(grid_file, year_file, climate_file, result_file)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(paste(
    "Missing required file(s):",
    paste(missing_files, collapse = "; ")
  ))
}

# -----------------------------
# 4. READ INPUT FILES
# -----------------------------
Latread <- read.csv(grid_file)

Yearread <- read.xlsx(year_file, sheet = "Rcp")

LIread <- read.xlsx(climate_file, colNames = FALSE, sheet = "Rsds")
Tempread <- read.xlsx(climate_file, colNames = FALSE, sheet = "Tavg")

# Important: check.names = FALSE because StellaR output has duplicate names.
StellaR_results <- read.csv(result_file, check.names = FALSE)

# -----------------------------
# 5. BASIC INPUT CHECKS
# -----------------------------
if (nrow(Latread) != 20) {
  stop("Grid_latlong.csv should contain exactly 20 grid locations.")
}

if (ncol(LIread) != 20) {
  stop("Rsds sheet should contain exactly 20 grid columns.")
}

if (ncol(Tempread) != 20) {
  stop("Tavg sheet should contain exactly 20 grid columns.")
}

required_year_cols <- c("Year", "Days", "StRow", "EndRow")

if (!all(required_year_cols %in% names(Yearread))) {
  stop("Year_rows.xlsx sheet 'Rcp' must contain Year, Days, StRow, and EndRow columns.")
}

Yearread$Year <- as.integer(Yearread$Year)
Yearread$Days <- as.integer(Yearread$Days)
Yearread$StRow <- as.integer(Yearread$StRow)
Yearread$EndRow <- as.integer(Yearread$EndRow)

expected_total_days <- sum(Yearread$Days)

if (nrow(StellaR_results) != expected_total_days) {
  warning(paste(
    "Results_Rcp85_Tavg.csv has", nrow(StellaR_results),
    "rows, but Year_rows.xlsx sums to", expected_total_days,
    "days. Continuing, but check this if results look strange."
  ))
}

# -----------------------------
# 6. IDENTIFY STELLAR OUTPUT COLUMNS
# -----------------------------
# Original StellaR master likely wrote:
# column 1 = row names from write.csv
# column 2 = day index
# columns 3:22 = 20 grid outputs
#
# We use integer positions, not names, because names are duplicated.

if (ncol(StellaR_results) == 22) {
  stellar_grid_cols <- 3:22
} else if (ncol(StellaR_results) == 21) {
  stellar_grid_cols <- 2:21
} else if (ncol(StellaR_results) > 22) {
  warning("Unexpected number of Results columns. Using final 20 columns as StellaR grid outputs.")
  stellar_grid_cols <- tail(seq_len(ncol(StellaR_results)), 20)
} else {
  stop("Results_Rcp85_Tavg.csv does not have enough columns for 20 grid outputs.")
}

StellaR_daily <- StellaR_results[, stellar_grid_cols, drop = FALSE]
names(StellaR_daily) <- paste0("Grid_", 1:20)

# Annual cumulative depot values are at the final day of each year.
annual_end_rows <- cumsum(Yearread$Days)

if (max(annual_end_rows) > nrow(StellaR_daily)) {
  stop("Annual end rows exceed number of rows in Results_Rcp85_Tavg.csv.")
}

stellar_annual_matrix <- StellaR_daily[annual_end_rows, , drop = FALSE]
stellar_annual_matrix <- as.data.frame(lapply(stellar_annual_matrix, as.numeric))
names(stellar_annual_matrix) <- paste0("Grid_", 1:20)

unique_counts_by_year <- apply(
  stellar_annual_matrix,
  1,
  function(x) length(unique(round(as.numeric(x), 6)))
)

stellar_diag <- data.frame(
  Year = Yearread$Year,
  Days = Yearread$Days,
  EndRow_Used = annual_end_rows,
  Unique_Grid_Values_At_Year_End = unique_counts_by_year
)

write.csv(
  stellar_diag,
  file.path(output_folder, "ComparisonC_StellaR_Output_Diagnostic.csv"),
  row.names = FALSE
)

write.csv(
  stellar_annual_matrix,
  file.path(output_folder, "ComparisonC_StellaR_Annual_EndRows_Raw_g_m2.csv"),
  row.names = FALSE
)

print("==================================================")
print("STELLAR OUTPUT DIAGNOSTIC")
print("First 10 annual end-row StellaR values:")
print(round(stellar_annual_matrix[1:min(10, nrow(stellar_annual_matrix)), ], 3))
print("Unique StellaR grid values per year:")
print(stellar_diag)
print("==================================================")

if (all(unique_counts_by_year == 1)) {
  warning(
    "Every annual row has identical StellaR grid values. That would suggest column extraction is still wrong or the file itself is odd."
  )
}

# -----------------------------
# 7. BIOLOGICAL CONSTANTS
# -----------------------------
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

# Keep our numerical-safety epsilon.
# Since Excel LI values are positive, this should have almost no effect.
light_epsilon <- 0.001

# StellaR photoperiod parameter.
p_stellar <- 0.8333

# -----------------------------
# 8. MODEL VARIANTS
# -----------------------------
# Main validation:
#   no extra 2.02, yearly reset, daily harvest, our photoperiod.
#
# Sensitivities:
#   paper prose harvest: every other day
#   carryover: density persists across years
#   StellaR photoperiod: same as main but daylength formula from StellaR
#   NW-style reference: closer to what we originally did, included only for context

model_variants <- data.frame(
  Model = c(
    "Main_FairValidation_Discrete",
    "Sensitivity_EveryOtherDayHarvest",
    "Sensitivity_Carryover",
    "Sensitivity_StellaRPhotoperiod",
    "Reference_NWStyle_2p02_Carryover_EOD"
  ),
  Description = c(
    "Main fair validation: no extra 2.02, yearly reset, daily harvest, our photoperiod, discrete update",
    "Same as main, but harvest every other day to match paper-prose interpretation",
    "Same as main, but density carries over across years",
    "Same as main, but uses StellaR photoperiod equation",
    "Reference only: extra 2.02, carryover, every-other-day harvest, global day"
  ),
  light_multiplier = c(1.00, 1.00, 1.00, 1.00, 2.02),
  density_persists_across_years = c(FALSE, FALSE, TRUE, FALSE, TRUE),
  harvest_every_n_days = c(1, 2, 1, 1, 2),
  photoperiod_method = c("ours", "ours", "ours", "stellar", "ours"),
  day_counter_mode = c("year_day", "year_day", "year_day", "year_day", "global_day"),
  stringsAsFactors = FALSE
)

primary_model <- "Main_FairValidation_Discrete"

write.csv(
  model_variants,
  file.path(output_folder, "ComparisonC_Model_Variants_Description.csv"),
  row.names = FALSE
)

# -----------------------------
# 9. HELPER FUNCTIONS
# -----------------------------
to_numeric <- function(x) {
  as.numeric(as.character(unlist(x)))
}

get_lat_lon <- function(Latread, grid) {
  if ("Latitude" %in% names(Latread)) {
    latitude <- Latread$Latitude[grid]
  } else {
    latitude <- Latread[grid, 2]
  }
  
  if ("Longitude" %in% names(Latread)) {
    longitude <- Latread$Longitude[grid]
  } else if (ncol(Latread) >= 3) {
    longitude <- Latread[grid, 3]
  } else {
    longitude <- NA_real_
  }
  
  list(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )
}

safe_cor <- function(x, y) {
  keep <- complete.cases(x, y)
  
  if (sum(keep) < 2) {
    return(NA_real_)
  }
  
  if (sd(x[keep]) == 0 || sd(y[keep]) == 0) {
    return(NA_real_)
  }
  
  cor(x[keep], y[keep])
}

rmse <- function(x) {
  sqrt(mean(x^2, na.rm = TRUE))
}

calc_photoperiod_ours <- function(day_number, latitude) {
  declination <- 23.45 * sin(pi / 180 * (360 / 365 * (day_number + 284)))
  arg <- -tan(latitude * pi / 180) * tan(declination * pi / 180)
  arg <- max(min(arg, 1), -1)
  E <- (24 / pi) * acos(arg)
  return(E)
}

calc_photoperiod_stellar <- function(day_number, latitude) {
  # This follows the shared StellaR R code structure.
  J <- day_number
  L <- latitude
  p <- p_stellar
  
  teta <- 0.2163108 + 2 * atan((0.9671396 * tan(0.00860 * (J - 186))))
  fi <- asin((0.39795 * cos(teta)))
  
  arg <- (
    sin(p * pi / 180) +
      sin(L * pi / 180 * sin(fi))
  ) / (
    cos(L * pi / 180 * cos(fi))
  )
  
  # Clamp only for numerical safety.
  arg <- max(min(arg, 1), -1)
  
  E <- 24 - (24 / pi) * acos(arg)
  return(E)
}

calc_ri <- function(Temp, LI, E) {
  if (!is.finite(Temp) || !is.finite(LI) || !is.finite(E)) {
    return(0)
  }
  
  if (Temp <= 5) {
    return(0)
  }
  
  LI_for_log <- max(LI, 0) + light_epsilon
  
  ri <- (
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
    ((log(LI_for_log) - A2) / A2)
  
  return(ri)
}

# -----------------------------
# 10. BUILD STELLAR ANNUAL TARGET TABLE
# -----------------------------
stellar_annual_list <- list()
stellar_index <- 1

for (grid in 1:20) {
  loc <- get_lat_lon(Latread, grid)
  
  for (year_index in 1:nrow(Yearread)) {
    year <- Yearread$Year[year_index]
    end_row <- annual_end_rows[year_index]
    
    stellar_g_m2 <- as.numeric(StellaR_daily[end_row, grid])
    
    stellar_annual_list[[stellar_index]] <- data.frame(
      Grid = grid,
      Latitude = loc$latitude,
      Longitude = loc$longitude,
      Year = year,
      StellaR_g_m2 = stellar_g_m2,
      StellaR_MT_ha_yr = stellar_g_m2 * 0.01,
      stringsAsFactors = FALSE
    )
    
    stellar_index <- stellar_index + 1
  }
}

stellar_annual <- do.call(rbind, stellar_annual_list)

write.csv(
  stellar_annual,
  file.path(output_folder, "ComparisonC_StellaR_Annual_Target.csv"),
  row.names = FALSE
)

# -----------------------------
# 11. RUN ONE VARIANT
# -----------------------------
run_one_variant <- function(variant_row) {
  
  model_name <- variant_row$Model
  description <- variant_row$Description
  light_multiplier <- as.numeric(variant_row$light_multiplier)
  density_persists <- as.logical(variant_row$density_persists_across_years)
  harvest_every_n_days <- as.numeric(variant_row$harvest_every_n_days)
  photoperiod_method <- variant_row$photoperiod_method
  day_counter_mode <- variant_row$day_counter_mode
  
  print("==================================================")
  print(paste("RUNNING:", model_name))
  print(description)
  print("==================================================")
  
  results <- list()
  result_index <- 1
  
  for (grid in 1:20) {
    
    loc <- get_lat_lon(Latread, grid)
    latitude <- loc$latitude
    longitude <- loc$longitude
    
    print(paste(
      "Grid", grid,
      "| Lat:", round(latitude, 3),
      "| Lon:", round(longitude, 3)
    ))
    
    # For carryover variants, density persists across years within each grid.
    density <- D_initial
    global_day <- 0
    
    for (year_index in 1:nrow(Yearread)) {
      
      year <- Yearread$Year[year_index]
      ndays <- Yearread$Days[year_index]
      
      # Follow the original master script convention.
      sRow <- Yearread$StRow[year_index] - 1
      eRow <- Yearread$EndRow[year_index] - 1
      
      if (sRow < 1 || eRow > nrow(Tempread) || eRow > nrow(LIread)) {
        stop(paste(
          "Invalid row range for year", year,
          "| sRow:", sRow,
          "| eRow:", eRow
        ))
      }
      
      temp_vec <- to_numeric(Tempread[sRow:eRow, grid])
      li_vec_raw <- to_numeric(LIread[sRow:eRow, grid])
      
      if (length(temp_vec) != ndays) {
        stop(paste(
          "Temperature length mismatch for grid", grid,
          "year", year,
          "| expected", ndays,
          "got", length(temp_vec)
        ))
      }
      
      if (length(li_vec_raw) != ndays) {
        stop(paste(
          "Light length mismatch for grid", grid,
          "year", year,
          "| expected", ndays,
          "got", length(li_vec_raw)
        ))
      }
      
      # Main validation uses yearly reset.
      # Carryover sensitivity keeps density from previous year.
      if (!density_persists) {
        density <- D_initial
      }
      
      annual_yield_g_m2 <- 0
      
      for (day in 1:ndays) {
        
        global_day <- global_day + 1
        
        if (day_counter_mode == "global_day") {
          day_number <- global_day
          harvest_counter <- global_day
        } else {
          day_number <- day
          harvest_counter <- day
        }
        
        Temp <- temp_vec[day]
        
        # Important:
        # For main RCP comparison, light_multiplier = 1.
        # The Excel Rsds sheet appears already converted to model LI/PAR.
        LI <- li_vec_raw[day] * light_multiplier
        
        if (!is.finite(Temp) || !is.finite(LI) || !is.finite(latitude)) {
          next
        }
        
        if (photoperiod_method == "stellar") {
          E <- calc_photoperiod_stellar(day_number, latitude)
        } else {
          E <- calc_photoperiod_ours(day_number, latitude)
        }
        
        ri <- calc_ri(Temp, LI, E)
        
        # Our discrete closed-form logistic density update.
        density <- (DL_dry * density) /
          ((DL_dry - density) * exp(-ri) + density)
        
        # Growth/harvest shutdown below 5 C.
        # This follows StellaR code behavior, not the paper's wording of "death".
        harvest <- ifelse(
          (Temp > 5) &&
            (density > D_thresh) &&
            (harvest_counter %% harvest_every_n_days == 0),
          density * H_ratio,
          0
        )
        
        density <- density - harvest
        annual_yield_g_m2 <- annual_yield_g_m2 + harvest
      }
      
      results[[result_index]] <- data.frame(
        Model = model_name,
        Grid = grid,
        Latitude = latitude,
        Longitude = longitude,
        Year = year,
        Our_g_m2 = annual_yield_g_m2,
        Our_MT_ha_yr = annual_yield_g_m2 * 0.01,
        light_multiplier = light_multiplier,
        density_persists_across_years = density_persists,
        harvest_every_n_days = harvest_every_n_days,
        photoperiod_method = photoperiod_method,
        day_counter_mode = day_counter_mode,
        stringsAsFactors = FALSE
      )
      
      result_index <- result_index + 1
    }
  }
  
  do.call(rbind, results)
}

# -----------------------------
# 12. RUN ALL VARIANTS
# -----------------------------
all_variant_results <- list()

for (i in 1:nrow(model_variants)) {
  all_variant_results[[i]] <- run_one_variant(model_variants[i, ])
}

our_results <- do.call(rbind, all_variant_results)

write.csv(
  our_results,
  file.path(output_folder, "ComparisonC_Our_Discrete_Model_Outputs.csv"),
  row.names = FALSE
)

# -----------------------------
# 13. MERGE WITH STELLAR TARGET
# -----------------------------
comparison <- merge(
  our_results,
  stellar_annual[, c("Grid", "Year", "StellaR_g_m2", "StellaR_MT_ha_yr")],
  by = c("Grid", "Year"),
  all.x = TRUE
)

comparison$Difference_MT_ha_yr <- comparison$Our_MT_ha_yr - comparison$StellaR_MT_ha_yr
comparison$Abs_Difference_MT_ha_yr <- abs(comparison$Difference_MT_ha_yr)

comparison$Percent_Difference <- ifelse(
  comparison$StellaR_MT_ha_yr == 0,
  NA,
  100 * comparison$Difference_MT_ha_yr / comparison$StellaR_MT_ha_yr
)

comparison$Abs_Percent_Difference <- abs(comparison$Percent_Difference)

comparison <- comparison[, c(
  "Model",
  "Grid",
  "Latitude",
  "Longitude",
  "Year",
  "StellaR_g_m2",
  "Our_g_m2",
  "StellaR_MT_ha_yr",
  "Our_MT_ha_yr",
  "Difference_MT_ha_yr",
  "Abs_Difference_MT_ha_yr",
  "Percent_Difference",
  "Abs_Percent_Difference",
  "light_multiplier",
  "density_persists_across_years",
  "harvest_every_n_days",
  "photoperiod_method",
  "day_counter_mode"
)]

detail_file <- file.path(output_folder, "ComparisonC_Detailed_Annual_Comparison.csv")

write.csv(
  comparison,
  detail_file,
  row.names = FALSE
)

# -----------------------------
# 14. MODEL-LEVEL SUMMARY
# -----------------------------
summary_list <- list()
summary_index <- 1

for (model_name in unique(comparison$Model)) {
  
  sub <- comparison[comparison$Model == model_name, ]
  
  summary_list[[summary_index]] <- data.frame(
    Model = model_name,
    Number_of_grid_year_comparisons = nrow(sub),
    Mean_StellaR_MT_ha_yr = mean(sub$StellaR_MT_ha_yr, na.rm = TRUE),
    Mean_Our_MT_ha_yr = mean(sub$Our_MT_ha_yr, na.rm = TRUE),
    Mean_Difference_Our_minus_StellaR = mean(sub$Difference_MT_ha_yr, na.rm = TRUE),
    Mean_Absolute_Difference = mean(sub$Abs_Difference_MT_ha_yr, na.rm = TRUE),
    Median_Absolute_Difference = median(sub$Abs_Difference_MT_ha_yr, na.rm = TRUE),
    RMSE_MT_ha_yr = rmse(sub$Difference_MT_ha_yr),
    Mean_Percent_Difference = mean(sub$Percent_Difference, na.rm = TRUE),
    Mean_Absolute_Percent_Difference = mean(sub$Abs_Percent_Difference, na.rm = TRUE),
    Correlation = safe_cor(sub$StellaR_MT_ha_yr, sub$Our_MT_ha_yr),
    stringsAsFactors = FALSE
  )
  
  summary_index <- summary_index + 1
}

model_summary <- do.call(rbind, summary_list)

model_summary_file <- file.path(output_folder, "ComparisonC_Model_Level_Summary.csv")

write.csv(
  model_summary,
  model_summary_file,
  row.names = FALSE
)

print("==================================================")
print("MODEL-LEVEL SUMMARY")
print(model_summary)
print("==================================================")

# -----------------------------
# 15. GRID-LEVEL SUMMARY
# -----------------------------
grid_summary <- aggregate(
  cbind(
    StellaR_MT_ha_yr,
    Our_MT_ha_yr,
    Difference_MT_ha_yr,
    Abs_Difference_MT_ha_yr,
    Percent_Difference,
    Abs_Percent_Difference
  ) ~ Model + Grid + Latitude + Longitude,
  data = comparison,
  FUN = function(x) mean(x, na.rm = TRUE)
)

grid_summary_file <- file.path(output_folder, "ComparisonC_Grid_Level_Summary.csv")

write.csv(
  grid_summary,
  grid_summary_file,
  row.names = FALSE
)

# -----------------------------
# 16. YEAR-LEVEL SUMMARY
# -----------------------------
year_summary <- aggregate(
  cbind(
    StellaR_MT_ha_yr,
    Our_MT_ha_yr,
    Difference_MT_ha_yr,
    Abs_Difference_MT_ha_yr,
    Percent_Difference,
    Abs_Percent_Difference
  ) ~ Model + Year,
  data = comparison,
  FUN = function(x) mean(x, na.rm = TRUE)
)

year_summary_file <- file.path(output_folder, "ComparisonC_Year_Level_Summary.csv")

write.csv(
  year_summary,
  year_summary_file,
  row.names = FALSE
)

# -----------------------------
# 17. PLOTS: MAIN MODEL SCATTER
# -----------------------------
primary <- comparison[comparison$Model == primary_model, ]

if (nrow(primary) > 0) {
  
  max_val <- max(
    c(primary$StellaR_MT_ha_yr, primary$Our_MT_ha_yr),
    na.rm = TRUE
  )
  
  png(
    filename = file.path(output_folder, "ComparisonC_Main_Scatter_StellaR_vs_OurDiscrete.png"),
    width = 900,
    height = 800
  )
  
  plot(
    primary$StellaR_MT_ha_yr,
    primary$Our_MT_ha_yr,
    xlab = "StellaR output yield (MT/ha/yr)",
    ylab = "Our fair-validation discrete yield (MT/ha/yr)",
    main = "Comparison C: StellaR vs Our Fair-Validation Discrete Model",
    pch = 16,
    xlim = c(0, max_val),
    ylim = c(0, max_val)
  )
  
  abline(0, 1, lwd = 2)
  
  dev.off()
}

# -----------------------------
# 18. PLOT: MAIN ANNUAL MEAN TRAJECTORY
# -----------------------------
primary_year <- year_summary[year_summary$Model == primary_model, ]

if (nrow(primary_year) > 0) {
  
  png(
    filename = file.path(output_folder, "ComparisonC_Main_Annual_Mean_Trajectory.png"),
    width = 1000,
    height = 700
  )
  
  plot(
    primary_year$Year,
    primary_year$StellaR_MT_ha_yr,
    type = "l",
    lwd = 2,
    ylim = range(
      c(primary_year$StellaR_MT_ha_yr, primary_year$Our_MT_ha_yr),
      na.rm = TRUE
    ),
    xlab = "Year",
    ylab = "Mean yield across 20 grids (MT/ha/yr)",
    main = "Mean Annual Yield: StellaR vs Main Fair-Validation Discrete Model"
  )
  
  lines(
    primary_year$Year,
    primary_year$Our_MT_ha_yr,
    lwd = 2,
    lty = 2
  )
  
  legend(
    "topleft",
    legend = c("StellaR output", "Our fair-validation discrete model"),
    lwd = 2,
    lty = c(1, 2),
    bty = "n"
  )
  
  dev.off()
}

# -----------------------------
# 19. PLOT: ALL VARIANTS ANNUAL MEAN
# -----------------------------
model_names <- unique(year_summary$Model)

stella_year <- aggregate(
  StellaR_MT_ha_yr ~ Year,
  data = year_summary,
  FUN = mean,
  na.rm = TRUE
)

ylim_all <- range(
  c(year_summary$StellaR_MT_ha_yr, year_summary$Our_MT_ha_yr),
  na.rm = TRUE
)

png(
  filename = file.path(output_folder, "ComparisonC_AllVariants_Annual_Mean_Trajectory.png"),
  width = 1200,
  height = 800
)

plot(
  stella_year$Year,
  stella_year$StellaR_MT_ha_yr,
  type = "l",
  lwd = 3,
  ylim = ylim_all,
  xlab = "Year",
  ylab = "Mean yield across 20 grids (MT/ha/yr)",
  main = "Comparison C: Diagnostic Variants vs StellaR"
)

lty_values <- 2:(length(model_names) + 1)

for (i in seq_along(model_names)) {
  this_model <- model_names[i]
  sub_year <- year_summary[year_summary$Model == this_model, ]
  
  lines(
    sub_year$Year,
    sub_year$Our_MT_ha_yr,
    lwd = 2,
    lty = lty_values[i]
  )
}

legend(
  "topleft",
  legend = c("StellaR output", model_names),
  lwd = c(3, rep(2, length(model_names))),
  lty = c(1, lty_values),
  bty = "n",
  cex = 0.7
)

dev.off()

# -----------------------------
# 20. PLOT: GRID MEAN DIFFERENCE FOR MAIN MODEL
# -----------------------------
primary_grid <- grid_summary[grid_summary$Model == primary_model, ]

if (nrow(primary_grid) > 0) {
  
  png(
    filename = file.path(output_folder, "ComparisonC_Main_Grid_Mean_Difference.png"),
    width = 1000,
    height = 700
  )
  
  plot(
    primary_grid$Grid,
    primary_grid$Difference_MT_ha_yr,
    type = "b",
    pch = 16,
    xlab = "Grid",
    ylab = "Mean difference: Our model - StellaR (MT/ha/yr)",
    main = "Main Fair-Validation Model: Mean Difference by Grid"
  )
  
  abline(h = 0, lwd = 2, lty = 2)
  
  dev.off()
}

# -----------------------------
# 21. WRITE README
# -----------------------------
readme_lines <- c(
  "Comparison C README",
  "===================",
  "",
  "Purpose:",
  "This comparison tests whether our discrete/spatial duckweed calculation logic can reproduce the provided StellaR RCP8.5 annual outputs after controlling comparison-specific mismatches.",
  "",
  "Main model:",
  "Main_FairValidation_Discrete",
  "",
  "Main model settings:",
  "- Uses prepared AllData_Rcp85_Tavg.xlsx Rsds sheet directly as LI.",
  "- Does NOT apply an extra 2.02 multiplier to the Excel Rsds sheet.",
  "- Resets density to 0.1 g/m2 at the start of each grid-year.",
  "- Uses daily harvest eligibility to match StellaR's Harvest_frequency = 1.0.",
  "- Uses our discrete closed-form logistic density update.",
  "- Uses our photoperiod formula.",
  "- Temp <= 5 C shuts down growth and harvest but does not kill/reset biomass.",
  "",
  "Why no extra 2.02 here?",
  "The Excel Rsds values appear already converted to PAR-like LI. The original raw ISIMIP rsds variable is W/m2, but the paper says preprocessing converted shortwave radiation to PAR for the R model. Therefore, for this RCP Excel comparison, applying 2.02 again would likely double-convert light.",
  "",
  "Why yearly reset here?",
  "The provided StellaR output is generated by annual grid-year runs. The StellaR code initializes D_dry = 0.1 and Depot = 0 each run. Therefore, yearly reset is used for the main validation comparison.",
  "",
  "Why keep discrete update?",
  "The goal is not to reproduce StellaR's ODE/RK4 solver exactly. The goal is to test our discrete/spatial model logic after correcting irrelevant comparison mismatches.",
  "",
  "Sensitivity variants included:",
  "- Sensitivity_EveryOtherDayHarvest: tests paper-prose harvest assumption.",
  "- Sensitivity_Carryover: tests the nuclear-winter recovery carryover assumption.",
  "- Sensitivity_StellaRPhotoperiod: tests daylength formula differences.",
  "- Reference_NWStyle_2p02_Carryover_EOD: reference version closer to the earlier nuclear-winter code assumptions.",
  "",
  "Most important output files:",
  "- ComparisonC_Model_Level_Summary.csv",
  "- ComparisonC_Detailed_Annual_Comparison.csv",
  "- ComparisonC_Grid_Level_Summary.csv",
  "- ComparisonC_Year_Level_Summary.csv",
  "- ComparisonC_Main_Scatter_StellaR_vs_OurDiscrete.png",
  "- ComparisonC_Main_Annual_Mean_Trajectory.png",
  "- ComparisonC_AllVariants_Annual_Mean_Trajectory.png"
)

writeLines(
  readme_lines,
  con = file.path(output_folder, "ComparisonC_README.txt")
)

# -----------------------------
# 22. DONE
# -----------------------------
print("==================================================")
print("COMPARISON C COMPLETE")
print(paste("Detailed comparison saved to:", detail_file))
print(paste("Model-level summary saved to:", model_summary_file))
print(paste("Grid-level summary saved to:", grid_summary_file))
print(paste("Year-level summary saved to:", year_summary_file))
print(paste("All outputs saved in:", output_folder))
print("==================================================")