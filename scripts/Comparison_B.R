# ============================================================
# Comparison B:
# Our final paper-aligned / spatial-model logic
# vs Femeena/StellaR provided RCP8.5 output
#
# Goal:
#   Use the same 20-grid RCP8.5 Tavg/Rsds input files,
#   run OUR final spatial-model logic on those inputs,
#   then compare against Results_Rcp85_Tavg.csv.
#
# Important:
#   This is NOT an exact StellaR reproduction.
#   StellaR code uses RK4 ODE, daily harvest, and resets each year.
#   Our final spatial model uses discrete logistic update,
#   every-other-day harvest, LI multiplier 2.02, and persistent density.
#
# Outputs:
#   1. Detailed comparison table
#   2. Model-level summary
#   3. Grid-level summary
#   4. Year-level summary
#   5. StellaR output diagnostics
#   6. Scatter plot
#   7. Annual mean plot
#   8. Optional diagnostic-variant plot
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
# Change this only if your Duckweed_Project folder is somewhere else.
data_folder <- "C:/Users/wsad4/Downloads/Duckweed_Project"

grid_file    <- file.path(data_folder, "Grid_latlong.csv")
year_file    <- file.path(data_folder, "Year_rows.xlsx")
climate_file <- file.path(data_folder, "AllData_Rcp85_Tavg.xlsx")
result_file  <- file.path(data_folder, "Results_Rcp85_Tavg.csv")

output_folder <- file.path(data_folder, "Comparison_B_OurSpatial_vs_StellaR")
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 3. USER OPTIONS
# -----------------------------
# TRUE = also run variants that help diagnose discrepancies.
# FALSE = only run the main model.
run_diagnostic_variants <- TRUE

# Main model name used for main plots.
primary_model <- "Our_FinalSpatial_Main"

# -----------------------------
# 4. READ INPUT FILES
# -----------------------------
Latread <- read.csv(grid_file)

Yearread <- read.xlsx(year_file, sheet = "Rcp")

LIread <- read.xlsx(climate_file, colNames = FALSE, sheet = "Rsds")
Tempread <- read.xlsx(climate_file, colNames = FALSE, sheet = "Tavg")

# Use check.names = FALSE because the StellaR output file has duplicate names.
StellaR_results <- read.csv(result_file, check.names = FALSE)

# -----------------------------
# 5. BASIC FILE CHECKS
# -----------------------------
required_files <- c(grid_file, year_file, climate_file, result_file)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(paste(
    "Missing required file(s):",
    paste(missing_files, collapse = "; ")
  ))
}

if (nrow(Latread) != 20) {
  stop("Grid_latlong.csv should contain exactly 20 grid locations.")
}

if (ncol(LIread) != 20) {
  stop("Rsds sheet in AllData_Rcp85_Tavg.xlsx should contain exactly 20 grid columns.")
}

if (ncol(Tempread) != 20) {
  stop("Tavg sheet in AllData_Rcp85_Tavg.xlsx should contain exactly 20 grid columns.")
}

if (!all(c("Year", "Days", "StRow", "EndRow") %in% names(Yearread))) {
  stop("Year_rows.xlsx sheet 'Rcp' must contain Year, Days, StRow, and EndRow columns.")
}

expected_total_days <- sum(as.integer(Yearread$Days))

if (nrow(StellaR_results) != expected_total_days) {
  warning(paste(
    "StellaR Results row count is", nrow(StellaR_results),
    "but Yearread Days sum to", expected_total_days,
    ". Check whether this is expected."
  ))
}

# -----------------------------
# 6. CORRECTLY IDENTIFY STELLAR OUTPUT COLUMNS
# -----------------------------
# The original StellaR master script wrote:
#   out_all <- matrix(1:13149, nrow = 13149, ncol = 1)
#   out_all <- cbind(out_all, out_gridvec) for each of 20 grids
#   write.csv(out_all, "Results_Rcp85_Tavg.csv")
#
# Because write.csv includes row names by default, the CSV usually has:
#   column 1 = row names
#   column 2 = day index
#   columns 3:22 = 20 grid outputs
#
# We use integer positions, not names, because the names are duplicated.

if (ncol(StellaR_results) == 22) {
  stellar_grid_cols <- 3:22
} else if (ncol(StellaR_results) == 21) {
  # In case the file was saved without row names.
  stellar_grid_cols <- 2:21
} else if (ncol(StellaR_results) > 22) {
  warning("Unexpected number of columns in Results_Rcp85_Tavg.csv. Using final 20 columns as StellaR grid outputs.")
  stellar_grid_cols <- tail(seq_len(ncol(StellaR_results)), 20)
} else {
  stop("Results_Rcp85_Tavg.csv does not have enough columns to contain 20 grid outputs.")
}

if (length(stellar_grid_cols) != 20) {
  stop("Could not identify exactly 20 StellaR grid-output columns.")
}

StellaR_daily <- StellaR_results[, stellar_grid_cols, drop = FALSE]
names(StellaR_daily) <- paste0("Grid_", 1:20)

# End rows for annual cumulative depot values.
end_rows_from_yearfile <- as.integer(Yearread$EndRow) - 1
end_rows_from_cumsum <- cumsum(as.integer(Yearread$Days))

if (!all(end_rows_from_yearfile == end_rows_from_cumsum)) {
  warning(
    "Yearread$EndRow - 1 does not match cumulative Days. Using cumulative Days for StellaR annual output extraction."
  )
  annual_end_rows <- end_rows_from_cumsum
} else {
  annual_end_rows <- end_rows_from_yearfile
}

# StellaR annual output diagnostic.
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
  EndRow_Used = annual_end_rows,
  Unique_Grid_Values_At_Year_End = unique_counts_by_year
)

write.csv(
  stellar_diag,
  file.path(output_folder, "ComparisonB_StellaR_Output_Diagnostic.csv"),
  row.names = FALSE
)

write.csv(
  stellar_annual_matrix,
  file.path(output_folder, "ComparisonB_StellaR_Annual_EndRows_Raw_g_m2.csv"),
  row.names = FALSE
)

print("==================================================")
print("STELLAR OUTPUT COLUMN DIAGNOSTIC")
print("First 10 annual end-row StellaR values:")
print(round(stellar_annual_matrix[1:min(10, nrow(stellar_annual_matrix)), ], 3))
print("Unique StellaR grid values per year:")
print(stellar_diag)
print("==================================================")

if (all(unique_counts_by_year == 1)) {
  warning(
    "All StellaR grid outputs are identical at each annual end row. This may mean the provided Results file itself has identical grid columns, or the output format differs from what we expect."
  )
}

# -----------------------------
# 7. BIOLOGICAL CONSTANTS
# -----------------------------
# These follow our final spatial model script.

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

# -----------------------------
# 8. MODEL VARIANTS
# -----------------------------
# Main model:
#   our final spatial-model logic:
#   persistent density, every-other-day harvest, LI multiplier 2.02.
#
# Diagnostic variants:
#   help explain why our output differs from StellaR.
#   These are not the main result.

model_variants <- data.frame(
  Model = c(
    "Our_FinalSpatial_Main",
    "Diagnostic_DailyHarvest",
    "Diagnostic_ResetEachYear",
    "Diagnostic_NoLightMultiplier",
    "Diagnostic_ClosestToStellaR_Discrete"
  ),
  Description = c(
    "Main: persistent density, every-other-day harvest, LI multiplier 2.02, global day",
    "Same as main, but harvest checked daily",
    "Same as main, but density resets to 0.1 each year",
    "Same as main, but Rsds is used directly with no 2.02 multiplier",
    "Closest discrete diagnostic: daily harvest, yearly reset, no 2.02 multiplier, day resets each year"
  ),
  harvest_every_n_days = c(2, 1, 2, 2, 1),
  density_persists_across_years = c(TRUE, TRUE, FALSE, TRUE, FALSE),
  light_multiplier = c(2.02, 2.02, 2.02, 1.00, 1.00),
  use_global_day_for_solar = c(TRUE, TRUE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

if (!run_diagnostic_variants) {
  model_variants <- model_variants[model_variants$Model == primary_model, ]
}

write.csv(
  model_variants,
  file.path(output_folder, "ComparisonB_Model_Variants_Description.csv"),
  row.names = FALSE
)

# -----------------------------
# 9. HELPER FUNCTIONS
# -----------------------------
to_numeric <- function(x) {
  as.numeric(as.character(x))
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
  
  return(list(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ))
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

run_our_model_one_variant <- function(variant_row) {
  
  model_name <- variant_row$Model
  harvest_every_n_days <- as.numeric(variant_row$harvest_every_n_days)
  density_persists <- as.logical(variant_row$density_persists_across_years)
  light_multiplier <- as.numeric(variant_row$light_multiplier)
  use_global_day_for_solar <- as.logical(variant_row$use_global_day_for_solar)
  
  print("==================================================")
  print(paste("RUNNING MODEL VARIANT:", model_name))
  print(paste("Description:", variant_row$Description))
  print("==================================================")
  
  results_list <- list()
  result_index <- 1
  
  for (grid in 1:20) {
    
    loc <- get_lat_lon(Latread, grid)
    latitude <- loc$latitude
    longitude <- loc$longitude
    
    print(paste(
      "Running", model_name,
      "| Grid", grid,
      "| Latitude:", latitude,
      "| Longitude:", longitude
    ))
    
    # In our final spatial model, density persists across years.
    # For reset diagnostic variants, it is reset at the start of each year.
    density <- D_initial
    global_day <- 0
    
    for (year_index in 1:nrow(Yearread)) {
      
      year <- Yearread$Year[year_index]
      ndays <- as.integer(Yearread$Days[year_index])
      
      sRow <- as.integer(Yearread$StRow[year_index]) - 1
      eRow <- as.integer(Yearread$EndRow[year_index]) - 1
      
      if (sRow < 1 || eRow > nrow(Tempread) || eRow > nrow(LIread)) {
        stop(paste(
          "Invalid row range for year", year,
          "| sRow:", sRow,
          "| eRow:", eRow
        ))
      }
      
      temp_vec <- to_numeric(Tempread[sRow:eRow, grid])
      li_raw_vec <- to_numeric(LIread[sRow:eRow, grid])
      
      if (length(temp_vec) != ndays) {
        stop(paste(
          "Temperature length mismatch for grid", grid,
          "year", year,
          "| expected", ndays,
          "got", length(temp_vec)
        ))
      }
      
      if (length(li_raw_vec) != ndays) {
        stop(paste(
          "Light length mismatch for grid", grid,
          "year", year,
          "| expected", ndays,
          "got", length(li_raw_vec)
        ))
      }
      
      if (!density_persists) {
        density <- D_initial
      }
      
      yield_sum_g_m2 <- 0
      
      for (day in 1:ndays) {
        
        global_day <- global_day + 1
        
        solar_day <- ifelse(
          use_global_day_for_solar,
          global_day,
          day
        )
        
        harvest_day_counter <- ifelse(
          use_global_day_for_solar,
          global_day,
          day
        )
        
        Temp <- temp_vec[day]
        LI <- li_raw_vec[day] * light_multiplier
        
        # If there are missing climate values, skip growth/harvest for that day.
        if (!is.finite(Temp) || !is.finite(LI) || !is.finite(latitude)) {
          next
        }
        
        # Photoperiod calculation from our final spatial code.
        declination <- 23.45 * sin(pi / 180 * (360 / 365 * (solar_day + 284)))
        arg <- -tan(latitude * pi / 180) * tan(declination * pi / 180)
        arg <- max(min(arg, 1), -1)
        E <- (24 / pi) * acos(arg)
        
        # Intrinsic growth rate.
        # Temp <= 5 C means growth shutdown, not biomass reset.
        if (Temp > 5) {
          
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
          
        } else {
          ri <- 0
        }
        
        # Discrete logistic density update from our spatial model.
        density <- (DL_dry * density) /
          ((DL_dry - density) * exp(-ri) + density)
        
        # Harvest logic from our spatial model.
        harvest <- ifelse(
          (Temp > 5) &&
            (density > D_thresh) &&
            (harvest_day_counter %% harvest_every_n_days == 0),
          density * H_ratio,
          0
        )
        
        density <- density - harvest
        yield_sum_g_m2 <- yield_sum_g_m2 + harvest
      }
      
      results_list[[result_index]] <- data.frame(
        Model = model_name,
        Grid = grid,
        Latitude = latitude,
        Longitude = longitude,
        Year = year,
        Our_g_m2 = yield_sum_g_m2,
        Our_MT_ha_yr = yield_sum_g_m2 * 0.01,
        harvest_every_n_days = harvest_every_n_days,
        density_persists_across_years = density_persists,
        light_multiplier = light_multiplier,
        use_global_day_for_solar = use_global_day_for_solar,
        stringsAsFactors = FALSE
      )
      
      result_index <- result_index + 1
    }
  }
  
  do.call(rbind, results_list)
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
  file.path(output_folder, "ComparisonB_StellaR_Annual_Target.csv"),
  row.names = FALSE
)

# -----------------------------
# 11. RUN OUR MODEL VARIANTS
# -----------------------------
all_model_results <- list()

for (i in 1:nrow(model_variants)) {
  all_model_results[[i]] <- run_our_model_one_variant(model_variants[i, ])
}

our_results <- do.call(rbind, all_model_results)

write.csv(
  our_results,
  file.path(output_folder, "ComparisonB_Our_Model_Annual_Outputs.csv"),
  row.names = FALSE
)

# -----------------------------
# 12. MERGE OUR OUTPUT WITH STELLAR TARGET
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

# Reorder columns for readability.
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
  "harvest_every_n_days",
  "density_persists_across_years",
  "light_multiplier",
  "use_global_day_for_solar"
)]

detail_file <- file.path(output_folder, "ComparisonB_Detailed_Annual_Comparison.csv")

write.csv(
  comparison,
  detail_file,
  row.names = FALSE
)

# -----------------------------
# 13. MODEL-LEVEL SUMMARY
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
    Mean_Percent_Difference = mean(sub$Percent_Difference, na.rm = TRUE),
    Mean_Absolute_Percent_Difference = mean(sub$Abs_Percent_Difference, na.rm = TRUE),
    Correlation = safe_cor(sub$StellaR_MT_ha_yr, sub$Our_MT_ha_yr),
    stringsAsFactors = FALSE
  )
  
  summary_index <- summary_index + 1
}

model_summary <- do.call(rbind, summary_list)

model_summary_file <- file.path(output_folder, "ComparisonB_Model_Level_Summary.csv")

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
# 14. GRID-LEVEL SUMMARY
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

grid_summary_file <- file.path(output_folder, "ComparisonB_Grid_Level_Summary.csv")

write.csv(
  grid_summary,
  grid_summary_file,
  row.names = FALSE
)

# -----------------------------
# 15. YEAR-LEVEL SUMMARY
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

year_summary_file <- file.path(output_folder, "ComparisonB_Year_Level_Summary.csv")

write.csv(
  year_summary,
  year_summary_file,
  row.names = FALSE
)

# -----------------------------
# 16. PLOTS: MAIN MODEL ONLY
# -----------------------------
primary <- comparison[comparison$Model == primary_model, ]

if (nrow(primary) > 0) {
  
  max_val <- max(
    c(primary$StellaR_MT_ha_yr, primary$Our_MT_ha_yr),
    na.rm = TRUE
  )
  
  # Scatter plot.
  png(
    filename = file.path(output_folder, "ComparisonB_Main_Scatter_StellaR_vs_Our.png"),
    width = 900,
    height = 800
  )
  
  plot(
    primary$StellaR_MT_ha_yr,
    primary$Our_MT_ha_yr,
    xlab = "StellaR output yield (MT/ha/yr)",
    ylab = "Our final spatial-model yield (MT/ha/yr)",
    main = "Comparison B: StellaR Output vs Our Final Spatial Model",
    pch = 16,
    xlim = c(0, max_val),
    ylim = c(0, max_val)
  )
  
  abline(0, 1, lwd = 2)
  
  dev.off()
  
  # Annual mean trajectory.
  primary_year <- year_summary[year_summary$Model == primary_model, ]
  
  png(
    filename = file.path(output_folder, "ComparisonB_Main_Annual_Mean_Trajectory.png"),
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
    main = "Mean Annual Yield: StellaR vs Our Final Spatial Model"
  )
  
  lines(
    primary_year$Year,
    primary_year$Our_MT_ha_yr,
    lwd = 2,
    lty = 2
  )
  
  legend(
    "topleft",
    legend = c("StellaR output", "Our final spatial model"),
    lwd = 2,
    lty = c(1, 2),
    bty = "n"
  )
  
  dev.off()
}

# -----------------------------
# 17. PLOT: ALL VARIANTS ANNUAL MEAN
# -----------------------------
if (run_diagnostic_variants) {
  
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
    filename = file.path(output_folder, "ComparisonB_AllVariants_Annual_Mean_Trajectory.png"),
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
    main = "Diagnostic Variants: Mean Annual Yield vs StellaR"
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
    cex = 0.75
  )
  
  dev.off()
}

# -----------------------------
# 18. WRITE README / INTERPRETATION NOTES
# -----------------------------
readme_lines <- c(
  "Comparison B README",
  "===================",
  "",
  "Purpose:",
  "This comparison runs our final paper-aligned/spatial duckweed model logic on the provided 20-grid RCP8.5 Tavg/Rsds inputs, then compares the annual yields against Results_Rcp85_Tavg.csv.",
  "",
  "Important interpretation:",
  "This is not an exact StellaR-code reproduction. It is our final spatial-model logic tested on the same 20-grid input data.",
  "",
  "Main model settings:",
  "- Discrete logistic density update",
  "- Density persists across years",
  "- Annual harvested yield resets each year",
  "- Temp <= 5 C shuts off growth but does not reset biomass",
  "- Harvest every other day",
  "- Harvest only if Temp > 5 C and density > 99 g/m2",
  "- Harvest removes 20% of density",
  "- Rsds/LI input multiplied by 2.02, following the nuclear-winter spatial code's FSDS-to-LI conversion",
  "- Light term uses log(LI + 0.001)",
  "",
  "Known StellaR-code differences:",
  "- StellaR uses deSolve ODE with RK4",
  "- StellaR resets D_dry = 0.1 and Depot = 0 for each grid-year",
  "- StellaR checks harvest daily because Harvest_frequency = 1.0",
  "- StellaR uses log(LI) without the +0.001 epsilon",
  "- StellaR appears to use the Rsds sheet directly as LI, without multiplying by 2.02",
  "",
  "Most important output files:",
  "- ComparisonB_Detailed_Annual_Comparison.csv",
  "- ComparisonB_Model_Level_Summary.csv",
  "- ComparisonB_Grid_Level_Summary.csv",
  "- ComparisonB_Year_Level_Summary.csv",
  "- ComparisonB_StellaR_Output_Diagnostic.csv",
  "- ComparisonB_Main_Scatter_StellaR_vs_Our.png",
  "- ComparisonB_Main_Annual_Mean_Trajectory.png",
  "- ComparisonB_AllVariants_Annual_Mean_Trajectory.png",
  "",
  "How to read the results:",
  "If the main model differs strongly from StellaR, that does not automatically mean our model is wrong. It may reflect real implementation differences between the paper-aligned spatial model and the provided StellaR code."
)

writeLines(
  readme_lines,
  con = file.path(output_folder, "ComparisonB_README.txt")
)

# -----------------------------
# 19. DONE
# -----------------------------
print("==================================================")
print("COMPARISON B COMPLETE")
print(paste("Detailed comparison saved to:", detail_file))
print(paste("Model-level summary saved to:", model_summary_file))
print(paste("Grid-level summary saved to:", grid_summary_file))
print(paste("Year-level summary saved to:", year_summary_file))
print(paste("Outputs saved in:", output_folder))
print("==================================================")