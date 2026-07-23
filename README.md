# Duckweed Spatial Growth Model — Nuclear Winter (Target 07, 150 Tg)

A spatial extension of the duckweed growth model from Femeena & Brennan (2025),
*Lemnaceae as a resilient crop to improve food security under climate extremes*
(Agriculture & Food Security 14:8). The original model simulates duckweed yield at
20 discrete point locations using ODEs solved in StellaR. This version applies the
same biology to a global raster grid and to a post-nuclear-war climate scenario.

## What this does

- Reads monthly WACCM4 climate (near-surface temperature and downwelling shortwave
  radiation) for the 150 Tg US–Russia nuclear winter scenario.
- Applies the duckweed growth model per pixel across recovery years 5–20 after the
  soot injection, producing annual yield maps in metric tons/ha/yr.
- Summarizes the annual maps into cumulative and average-annual yield rasters, and
  extracts yields at the 20 paper grid locations for validation.

## Repository layout

- `scripts/Target_7_5-20_Code_V3.R` — runs the per-pixel model; writes one GeoTIFF
  per year plus an annual peak-yield summary.
- `scripts/Target_7_5-20_Map_V3.R` — reads the annual GeoTIFFs; writes cumulative and
  average-annual rasters and extracts grid-point yields for validation.
- `scripts/Comparison_B.R`, `scripts/Comparison_C.R` — validation scripts comparing
  this discrete implementation against the reference StellaR output (see "Known
  differences" below).
- `data/Grid_latlong.csv` — the 20 paper grid coordinates, used for the validation extract.
- The comparison scripts require the reference StellaR model outputs from Femeena & Brennan, which are not redistributed here; they are included to document the validation method.

## Installation and running

Requires R (4.x) with the `terra` package:

```r
install.packages("terra")
```

The map script also uses `maps`, which it installs automatically if missing. The
comparison scripts load their own packages at the top — check their `library()` lines
and install any you are missing.

Before running, set the folder paths at the top of each script (input climate folder,
output folder, and the path to `data/Grid_latlong.csv`). Then run in order:

1. Run `Target_7_5-20_Code_V3.R` to completion (all 16 years).
2. Run `Target_7_5-20_Map_V3.R`.

`Code` has one run-mode switch, `use_annual_reset` (see "Run modes").

## Data (not included)

The WACCM4 150 Tg climate files are not in this repository (large, and not ours to
redistribute). They are from Coupe et al. (2019), J. Geophys. Res. Atmos. 124(15),
available at: https://figshare.com/articles/dataset/WACCM4_150_Tg_US-Russia/7742735

## Run modes

`use_annual_reset` in `Code_V3` selects between two initializations:

- `FALSE` (default) — biomass persists continuously across all 16 years. This is the
  intended recovery model: it captures duckweed banking biomass through bad years and
  regrowing, the multi-year recovery dynamic the scenario is about.
- `TRUE` — biomass re-initializes each year, matching the original StellaR pipeline.
  Used only to validate against the paper.

## Known differences from the reference implementation

Reset-mode output reproduces the paper's Target 07 spatial pattern, latitudinal
ordering, trajectory shape, and per-grid behavior. Absolute yields on well-growing
pixels run ~15–20% above the paper, for three deliberate, documented reasons:

1. **Harvest amount.** The reference StellaR model applies harvest as a flow inside an
   rk4-integrated ODE, which effectively removes ~6.7% of biomass per harvest rather
   than the 20% stated in the paper. This model applies the stated 20% exactly, so it
   sits above the reference on productive pixels. The gap shrinks toward zero at the
   winter troughs, where little is harvested — consistent with this being the cause.
2. **Harvest cadence.** Harvesting occurs every other day, following the paper's stated
   rule. The shared StellaR code harvests every day.
3. **Photoperiod.** Daylength uses the standard Cooper declination / sunrise-hour-angle
   formula.

Growth integration uses the closed-form logistic solution stepped daily, which matches
the reference rk4 integration to ~1e-4 on pure growth.

## License

Licensed under the Apache License 2.0 — see [LICENSE](LICENSE). Copyright 2026 ALLFED.
