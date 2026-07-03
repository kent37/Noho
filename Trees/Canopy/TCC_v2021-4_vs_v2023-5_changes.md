# Why NLCD Tree Canopy Cover v2021.4 and v2023.5 disagree for 2011-2021

Source: `TCC_v2023-5_Methods.pdf` (Housman et al. 2025, USDA Forest Service, GO-10268-RPT2), specifically its Release Notes (p.6), Model Predictor Data (pp.10-15), Modeling (pp.17-18), and NLCD TCC Post-processing Methods (pp.20-24) sections.

## Summary

`Tree_canopy_change_2011_2021.qmd` (v2021.4) and `Tree_canopy_change_1985_2023.qmd` (v2023.5) report meaningfully different canopy coverage for the same years, 2011-2021, even though both cover that period. The random forest model itself did **not** change between releases — the methods doc's release notes explicitly list "no changes" for computing platforms, calibration data, and modeling. The disagreement instead comes from the model's **inputs** and **post-processing** being redone with each release, in ways that reach back and change already-published years.

## How the pipeline works

1.  **LandTrendr temporal segmentation.** Annual Landsat/Sentinel-2 composites are fed through LandTrendr, which fits each pixel's time series as a small number of straight-line segments (start year, end year, fitted value at each vertex). The value used for any given year is read off whichever segment it falls in — not that year's raw reflectance.
2.  **A single random forest model, calibrated once on 2011 data.** NLCD TCC does not retrain per year. One model, built from 2011 FIA reference plots and 2011 LandTrendr-fitted composites, is applied unchanged to predict TCC for every year 1985-2023. This relies on LandTrendr's fitting making composites radiometrically comparable across years, so the 2011 model can be "transferred" to other years without retraining.
3.  **Post-processing / map assemblage.** Predicted values are then masked and smoothed: non-tree features are zeroed out, low-confidence urban pixels are zeroed out using a model-uncertainty statistic (τ, "tau"), and a 10%-change temporal filter suppresses small year-to-year noise.

## What actually changed between v2021.4 and v2023.5

Per the release notes (p.6):

| Stage | Change |
|----|----|
| Computing platforms | No change |
| Model calibration data | No change |
| Modeling | No change |
| Model predictor data | LandTrendr **rerun** with composites extended to 1984-2024 (v2021.4 used a shorter window) |
| Map assemblage | **Different τ percentile thresholds** for low-density and high-density urban TCC masking |
| Map assemblage | Non-urban TCC no longer has a **clump-and-eliminate minimum-mapping-unit filter** applied (v2021.4 had one) |

Two mechanisms explain why *already-published* years like 2011-2021 shifted:

- **LandTrendr refit changes old years' fitted values.** Because segmentation is fit over the whole series per pixel, adding 2022-2024 data can move where a segment's breakpoints fall throughout the series — including within 2011-2021 — even though the raw imagery for those years didn't change. That shifted composite then feeds the same unchanged 2011-model, producing a different predicted TCC for the same pixel-year.
- **Post-processing masking rules changed independently of the model.** The τ-based urban masking now uses different (and, per the doc, "adjusted through time" rather than constant) percentile cutoffs for low- vs. high-density developed areas, and the non-urban minimum-mapping-unit filter that used to remove small fragmented canopy patches was dropped entirely in v2023.5. Both change how much land gets zeroed out, on top of whatever the model predicted.

## Urban vs. non-urban classification (relevant to the τ masking change)

The urban mask combines two independent sources (p.20):

- **Census Bureau TIGER/Line "Urban Areas" boundaries** (2018 vintage, block-level) — a demographic/administrative delineation based on population and housing-unit density.
- **LCMS land-use "Developed" class** — a separate, imagery-derived classification from the Forest Service's Landscape Change Monitoring System.

Pixels flagged urban by either source are then further split into **low-density vs. high-density developed** using NLCD's developed-intensity classes, and each tier gets its own τ threshold (p.22: roughly 90th→86th percentile for high-density, 88th→85th for low-density in v2023.5). Non-urban pixels instead get a separate tree/non-tree mask built from LCMS land cover, NLCD cultivated-crop/pasture classes, and CDL tree-crop classes (p.20-21).

For Northampton, this means the citywide and city-center comparisons are governed mostly by the changed urban τ thresholds, while the zoning-category and outlying-area comparisons are more affected by the dropped non-urban minimum-mapping-unit filter — different mechanisms driving different parts of the same overall discrepancy.

## Bottom line

The gap between v2021.4 and v2023.5 for identical 2011-2021 years is a real, documented consequence of NLCD reprocessing the full time series with revised predictor data and masking rules each release — not measurement noise, not a bug in either report, and not evidence that actual canopy trends differed. It reflects the standard NLCD practice of superseding all prior-year maps with each new release rather than only adding new years.
