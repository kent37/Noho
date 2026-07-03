# Tree canopy dataset comparison report — design

## Background

`Trees/Canopy/` has two tree-canopy-change reports built from different NLCD
Tree Canopy Cover (TCC) data releases:

- `Tree_canopy_change_2011_2021.qmd` — old series, `data/nlcd_tcc_CONUS_all`
  (`nlcd_tcc_conus_YYYY_v2021-4.tif`), years 2011-2021.
- `Tree_canopy_change_1985_2023.qmd` — new series,
  `data/nlcd_tcc_CONUS_1985_2023_v2023-5_wgs84`
  (`nlcd_tcc_CONUS_YYYY_v2023-5_wgs84.zip`), years 1985-2023.

Both reports summarize canopy coverage over the same 2011-2021 window but
report meaningfully different values for it. A raster-level viewer already
exists (`Trees/Canopy/Canopy_compare/app.R`, a Shiny app) for visually
diffing any two individual year-layers on a map. This report instead
produces the same kind of aggregate trend charts as the two existing
reports, but with both datasets' values plotted together, so the size and
pattern of the discrepancy is visible at the summary level the existing
reports already report at.

## Scope

New file: `Trees/Compare/Tree_canopy_dataset_comparison.qmd`
Title: "Comparing NLCD tree canopy datasets, 2011-2021"

Breakdowns covered (matching sections in the existing reports):
- City overall
- City centers (Leeds / Florence / Northampton)
- Zoning category (5 aggregated classes)

Wards/precincts are out of scope (14 series would be visually noisy for a
diagnostic comparison; can be added later if needed).

For each breakdown: a raw-coverage chart (coverage % by year) and a
change-since-2011 chart, both with old and new dataset overlaid — six
charts total, mirroring the two chart types per breakdown in the existing
reports.

Additionally, one `gt` summary table for the city-overall breakdown: Year,
Old % (v2021-4), New % (v2023-5), Difference (percentage points),
2011-2021.

No raster/map output in this report — that's what the existing
`Canopy_compare` Shiny app is for.

## Data pipeline

Reuse `Trees/Canopy/NLCD_helpers.R` unmodified: `noho`, `read_layer`,
`clip_and_mean_all`, `aggregated_zoning()`, `zoning_categories`,
`all_zoning`. These functions look up a module-level `lc_layers` variable
by lexical scoping at call time (not at definition time), so re-assigning
`lc_layers` in the qmd's global env between calls is the established
pattern the existing reports already rely on — no changes to the helper
file are needed.

Steps:
1. Load the old series into `lc_layers` (11 tif files, 2011-2021).
   Apply the same `disagg(layer, res_bump=4)` and 254→0 treatment as the
   existing reports.
2. Compute `clip_and_mean_all()` for `noho`, each city center, each
   zoning class. Tag result rows `Dataset = "2011-2021 (v2021-4)"`.
3. Re-assign `lc_layers` to the new series, filtered to years 2011-2021
   only (11 of the 39 available zip files — filter the file list by a
   `2011`-`2021` year regex, not by loading and discarding).
4. Recompute the same three breakdowns. Tag `Dataset = "1985-2023
   (v2023-5)"`.
5. `bind_rows()` old + new per breakdown into `noho_compare`,
   `center_compare`, `zone_compare`. Compute `Change` (coverage minus each
   series' own 2011 value) within `Dataset` (and category, for
   center/zone) groups.

City centers (`Shapefiles/CityCenters.gpkg`) and zoning polygons
(`aggregated_zoning()`) are independent of `lc_layers` and are computed
once, before the two data-loading passes.

## Charts and styling

- Same `theme_minimal(base_size=12)` + bottom legend + bold titles as the
  existing reports; same `plot_caption = 'Data: mlrc.gov | Analysis: Kent
  Johnson'`.
- X-axis: `scale_x_continuous(breaks=2011:2021, minor_breaks=NULL)` (the
  2011-2021 report's axis, not the wider 1985-2023 one), since this
  report only ever covers 2011-2021.
- Line color: same mapping as the originals — fixed `'darkgreen'` for the
  city-overall chart; default hue mapped to `City` / `Class` for the
  center/zoning charts (mapping is unchanged by adding a second dataset,
  since color depends only on category, not on Dataset).
- Line type: `linetype = Dataset`, old series (`v2021-4`) dashed, new
  series (`v2023-5`) solid, via `scale_linetype_manual()`.
- Labeling: standard ggplot legends for both `color` (category) and
  `linetype` (dataset), rather than the originals' direct on-plot text
  labels. Direct labels made sense with one line per category; with two
  lines per category (same color, different linetype) a direct label
  would be ambiguous about which line it names, and hand-tuning label
  offsets for this specific combined dataset isn't something to guess at
  without live rendering. This is the one deliberate visual departure
  from the originals' style.
- `Change` charts use the same construction as the raw-coverage charts
  (same aesthetics, `y = Change` instead of `y = Coverage`,
  `labels=label_percent(accuracy=0.1)`).

## Summary table

One `gt` table, city-overall only, columns: Year, `2011-2021 (v2021-4)`
%, `1985-2023 (v2023-5)` %, Difference (pp) = new − old. Percent
formatting via `scales::percent()`/`fmt_percent()` consistent with
`change_table()` in the existing reports, though this is a new table
function (existing `change_table()` is shaped for a single dataset's
2011-vs-2021 comparison, not a per-year two-dataset comparison — no
attempt to reuse it beyond formatting conventions).

## Out of scope

- No changes to `Trees/Canopy/NLCD_helpers.R`, the two existing reports,
  or `Canopy_compare/app.R`.
- No ward/precinct breakdown.
- No raster/map visuals (existing Shiny app covers that).
- No narrative interpretation of *why* the datasets differ (NLCD
  algorithm version changes between v2021-4 and v2023-5) beyond a brief
  factual note in the introduction — this report's job is to show the
  size and shape of the discrepancy, not diagnose its cause.
