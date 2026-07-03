# Tree Canopy Dataset Comparison Report Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `Trees/Compare/Tree_canopy_dataset_comparison.qmd`, a Quarto report that plots citywide, city-center, and zoning-category tree canopy coverage for 2011-2021 from both the old (`v2021-4`) and new (`v2023-5`) NLCD datasets on the same charts, so the size of the discrepancy between the two data releases is visible.

**Architecture:** Single self-contained `.qmd` file that sources the existing `Trees/Canopy/NLCD_helpers.R` unmodified, loads each dataset's rasters into the module-level `lc_layers` variable in turn (the pattern the two existing canopy-change reports already use), computes coverage via the existing `clip_and_mean_all()` helper for both series, combines them into three tidy comparison data frames, and renders six ggplot charts (raw coverage + change-since-2011, for city/centers/zoning) plus one `gt` summary table.

**Tech Stack:** R, Quarto, tidyverse, terra, sf, gt, scales — all already used by `Trees/Canopy/*.qmd`.

## Global Constraints

- Do not modify `Trees/Canopy/NLCD_helpers.R`, `Trees/Canopy/Tree_canopy_change_2011_2021.qmd`, `Trees/Canopy/Tree_canopy_change_1985_2023.qmd`, or `Trees/Canopy/Canopy_compare/app.R`.
- No git commits. The user's global `~/.claude/CLAUDE.md` says not to commit or make other repository changes unless asked — every task ends with a verification step, not a commit. Leave the working tree as-is for the user to review and commit themselves.
- No generic "run R code in the live session" tool is available in this environment (the `r-btw` MCP only exposes structured tools like `describe_data_frame`, `files_edit`, `pkg_test` — none of them evaluate arbitrary code). Verification therefore runs R non-interactively via `Rscript`/`quarto render` through Bash, not through the live RStudio session.
- Reuse `here::here()`-relative paths exactly as the existing reports do — the project root is `/Users/kent/Dev/Noho` (location of `Noho.Rproj`).
- Old series: `data/nlcd_tcc_CONUS_all/**/nlcd_tcc_conus_YYYY_v2021-4.tif`, years 2011-2021 (11 files).
- New series: `data/nlcd_tcc_CONUS_1985_2023_v2023-5_wgs84/nlcd_tcc_CONUS_YYYY_v2023-5_wgs84.zip`, filtered to years 2011-2021 only (11 of 39 files).
- Dataset labels used verbatim throughout: `"2011-2021 (v2021-4)"` (old, dashed line) and `"1985-2023 (v2023-5)"` (new, solid line).
- Wards/precincts are out of scope. No raster/map output (the existing `Canopy_compare` Shiny app covers that).

---

### Task 1: Scaffold the report and load the old-series data

**Files:**
- Create: `Trees/Compare/Tree_canopy_dataset_comparison.qmd`
- Create: `Trees/Compare/icons8-oak-tree-48.png` (copy of `Trees/icons8-oak-tree-48.png`, so the favicon reference resolves relative to this report's own directory — see the "Quadrant maps" directory for the same per-directory-copy convention)
- Test (temporary, deleted at end of task): `Trees/Compare/.tmp_check.R`

**Interfaces:**
- Consumes: `Trees/Canopy/NLCD_helpers.R` exports `noho` (sf object), `read_layer(path, mask_layer=noho)`, `clip_and_mean_all(mask)` (returns tibble with columns `Area`, `Coverage`, `Year`), `aggregated_zoning()` (returns sf tibble with columns `Class`, `geometry`).
- Produces: qmd global-env variables `res_bump` (4), `read_tree_layer(layer_path)` (function), `lc_layers` (named list of SpatRaster, old series after this task), `centers` (sf tibble: `City`, `geometry`), `zoning` (sf tibble: `Class`, `geometry`), `old_noho` (tibble: `Dataset`, `Area`, `Coverage`, `Year`), `old_centers` (tibble: `City`, `Dataset`, `Area`, `Coverage`, `Year`), `old_zones` (tibble: `Class`, `Dataset`, `Area`, `Coverage`, `Year`). These are consumed by Task 2.

- [ ] **Step 1: Create the directory and copy the favicon**

```bash
mkdir -p "/Users/kent/Dev/Noho/Trees/Compare"
cp "/Users/kent/Dev/Noho/Trees/icons8-oak-tree-48.png" "/Users/kent/Dev/Noho/Trees/Compare/icons8-oak-tree-48.png"
```

- [ ] **Step 2: Write the qmd skeleton**

Create `Trees/Compare/Tree_canopy_dataset_comparison.qmd` with this content:

````markdown
---
title: "Comparing NLCD tree canopy datasets, 2011-2021"
author: "Kent Johnson"
toc: true
callout-icon: false
format:
  html:
    embed-resources: true
    header-includes: '<link rel="icon" type="image/png" href="icons8-oak-tree-48.png">'
    link-external-newwindow: true
execute:
  echo: false
  message: false
  warning: false
---

<style>
table.gt_table {
    color: var(--quarto-body-color);
    font-size: 14px;
}
</style>

```{r libraries, include=FALSE}
library(tidyverse)
library(gt)
library(scales)
library(sf)
library(terra)

source(here::here('Trees/Canopy/NLCD_helpers.R'))
```

```{r helpers}
theme_set(theme_minimal(base_size=12) +
            theme(legend.position='bottom', plot.title=element_text(face='bold')))

dataset_linetypes = c('2011-2021 (v2021-4)'='dashed', '1985-2023 (v2023-5)'='solid')
plot_caption = 'Data: mlrc.gov | Analysis: Kent Johnson'
table_caption = md('Data: [Multi-Resolution Land Characteristics Consortium](https://www.mrlc.gov/) | Analysis: Kent S Johnson')
```

### Introduction

The two [tree canopy change reports](../Canopy/Tree_canopy_change_2011_2021.html)
for Northampton draw on two different releases of NLCD Tree Canopy Cover
(TCC) data: an older release covering 2011-2021 (`v2021-4`), and a newer
release covering 1985-2023 (`v2023-5`). Both releases report data for the
overlapping 2011-2021 period, but the reported coverage values differ
meaningfully between them.

This report plots both datasets' values for 2011-2021 together, so the
size and shape of the discrepancy is visible at the same summary level
the two source reports already report at — citywide, by city center, and
by zoning category. Dashed lines are the older (`v2021-4`) dataset;
solid lines are the newer (`v2023-5`) dataset restricted to 2011-2021.

For a raster-level, year-by-year comparison of any two individual layers
on a map, see the
[tree canopy layer comparison tool](../Canopy/Canopy_compare/app.R).

```{r read_data}
options("rlib_name_repair_verbosity"="quiet")

res_bump = 4

read_tree_layer = function(layer_path) {
  layer = read_layer(layer_path)

  # Some layers have 254 values for NA. Treat them as 0
  layer[layer==254] = 0
  disagg(layer, res_bump)
}

centers = read_sf(here::here('Shapefiles/CityCenters.gpkg'), layer='CityCenterPolys') |>
  filter(!Code %in% c('L4', 'L5')) |>
  st_make_valid() |>
  summarize(.by=City, geometry=st_union(geometry)) |>
  # Clean up Florence a bit
  st_buffer(1) |>
  st_buffer(-1)

zoning = aggregated_zoning()

# --- Old series: 2011-2021, v2021-4 ---
old_files = list.files(here::here('data/nlcd_tcc_CONUS_all'),
                        pattern = '^nlcd_tcc_conus_\\d{4}_v2021-4.tif$',
                        full.names=TRUE, recursive=TRUE)

lc_layers = map(set_names(old_files, basename(old_files)),
                read_tree_layer, .progress='Reading old-series layers')

old_noho = clip_and_mean_all(noho) |>
  mutate(Dataset='2011-2021 (v2021-4)', .before=1)

old_centers = pmap_dfr(centers, function(City, geometry) {
  clip_and_mean_all(st_sfc(geometry, crs=st_crs(centers))) |>
    mutate(City=City, Dataset='2011-2021 (v2021-4)', .before=1)
})

old_zones = pmap_dfr(zoning, function(Class, geometry) {
  clip_and_mean_all(st_sfc(geometry, crs=st_crs(zoning))) |>
    mutate(Class=Class, Dataset='2011-2021 (v2021-4)', .before=1)
})
```
````

Note: this is intentionally incomplete (no charts yet, and Task 2 will
add the new-series data and combine everything) — it should still render
without error since Quarto renders whatever chunks exist.

- [ ] **Step 3: Verify the old-series data loads and produces plausible coverage**

Write `Trees/Compare/.tmp_check.R`:

```r
source(here::here('Trees/Canopy/NLCD_helpers.R'))
library(tidyverse)
library(terra)
library(sf)

options("rlib_name_repair_verbosity"="quiet")
res_bump = 4
read_tree_layer = function(layer_path) {
  layer = read_layer(layer_path)
  layer[layer==254] = 0
  disagg(layer, res_bump)
}

old_files = list.files(here::here('data/nlcd_tcc_CONUS_all'),
                        pattern = '^nlcd_tcc_conus_\\d{4}_v2021-4.tif$',
                        full.names=TRUE, recursive=TRUE)
stopifnot(length(old_files) == 11)

lc_layers = map(set_names(old_files, basename(old_files)),
                read_tree_layer, .progress='Reading old-series layers')

old_noho = clip_and_mean_all(noho) |> mutate(Dataset='2011-2021 (v2021-4)', .before=1)
print(old_noho)
stopifnot(nrow(old_noho) == 11)
stopifnot(all(old_noho$Coverage > 0.40 & old_noho$Coverage < 0.70))
cat('OK: old-series citywide coverage in plausible range\n')
```

Run: `cd "/Users/kent/Dev/Noho" && Rscript "Trees/Compare/.tmp_check.R"`

Expected: prints an 11-row tibble with `Coverage` values roughly in the
0.55-0.58 range (matching the ~57% citywide figure quoted in
`Tree_canopy_change_2011_2021.qmd`), both `stopifnot()` calls pass
silently, and the script ends by printing `OK: old-series citywide
coverage in plausible range`. No errors.

- [ ] **Step 4: Delete the temporary check script**

```bash
rm "/Users/kent/Dev/Noho/Trees/Compare/.tmp_check.R"
```

---

### Task 2: Load the new-series data and build the comparison data frames

**Files:**
- Modify: `Trees/Compare/Tree_canopy_dataset_comparison.qmd` (append to the `read_data` chunk from Task 1, then add a new `combine_data` chunk)
- Test (temporary, deleted at end of task): `Trees/Compare/.tmp_check.R`

**Interfaces:**
- Consumes: everything Task 1 produces (`res_bump`, `read_tree_layer`, `centers`, `zoning`, `old_noho`, `old_centers`, `old_zones`), plus `noho` and `clip_and_mean_all()` from `NLCD_helpers.R`.
- Produces: `new_noho`, `new_centers`, `new_zones` (same shapes as the `old_*` versions but `Dataset='1985-2023 (v2023-5)'`); `noho_compare`, `center_compare`, `zone_compare` (bound old+new, `Dataset` as an ordered factor with levels `c('2011-2021 (v2021-4)', '1985-2023 (v2023-5)')`); `noho_change_compare`, `center_change_compare`, `zone_change_compare` (same columns plus `Change`, computed as coverage minus each series' own 2011 value, grouped by `Dataset` and — for centers/zones — also by `City`/`Class`). These are consumed by Tasks 3-5.

- [ ] **Step 1: Append the new-series loading code to the `read_data` chunk**

Add this to the end of the `read_data` chunk in
`Trees/Compare/Tree_canopy_dataset_comparison.qmd` (right after the
`old_zones = pmap_dfr(...)` block from Task 1, still inside the same
` ```{r read_data} ` chunk):

```r
# --- New series: 1985-2023, v2023-5, filtered to 2011-2021 ---
new_files = list.files(here::here('data/nlcd_tcc_CONUS_1985_2023_v2023-5_wgs84'),
                        pattern = '^nlcd_tcc_CONUS_20(1[1-9]|2[01])_v2023-5_wgs84\\.zip$',
                        full.names=TRUE)

lc_layers = map(set_names(new_files, basename(new_files)),
                read_tree_layer, .progress='Reading new-series layers')

new_noho = clip_and_mean_all(noho) |>
  mutate(Dataset='1985-2023 (v2023-5)', .before=1)

new_centers = pmap_dfr(centers, function(City, geometry) {
  clip_and_mean_all(st_sfc(geometry, crs=st_crs(centers))) |>
    mutate(City=City, Dataset='1985-2023 (v2023-5)', .before=1)
})

new_zones = pmap_dfr(zoning, function(Class, geometry) {
  clip_and_mean_all(st_sfc(geometry, crs=st_crs(zoning))) |>
    mutate(Class=Class, Dataset='1985-2023 (v2023-5)', .before=1)
})
```

- [ ] **Step 2: Add a `combine_data` chunk after `read_data`**

```{r combine_data}
dataset_levels = c('2011-2021 (v2021-4)', '1985-2023 (v2023-5)')

noho_compare = bind_rows(old_noho, new_noho) |>
  mutate(Dataset=factor(Dataset, levels=dataset_levels))

center_compare = bind_rows(old_centers, new_centers) |>
  mutate(Dataset=factor(Dataset, levels=dataset_levels))

zone_compare = bind_rows(old_zones, new_zones) |>
  mutate(Dataset=factor(Dataset, levels=dataset_levels))

noho_change_compare = noho_compare |>
  arrange(Dataset, Year) |>
  group_by(Dataset) |>
  mutate(Change = Coverage - Coverage[1]) |>
  ungroup()

center_change_compare = center_compare |>
  arrange(Dataset, City, Year) |>
  group_by(Dataset, City) |>
  mutate(Change = Coverage - Coverage[1]) |>
  ungroup()

zone_change_compare = zone_compare |>
  arrange(Dataset, Class, Year) |>
  group_by(Dataset, Class) |>
  mutate(Change = Coverage - Coverage[1]) |>
  ungroup()
```

- [ ] **Step 3: Verify the new series loads and the comparison frames are shaped correctly**

Write `Trees/Compare/.tmp_check.R` with the full Task-1 check script (Step
3 of Task 1) plus this appended:

```r
new_files = list.files(here::here('data/nlcd_tcc_CONUS_1985_2023_v2023-5_wgs84'),
                        pattern = '^nlcd_tcc_CONUS_20(1[1-9]|2[01])_v2023-5_wgs84\\.zip$',
                        full.names=TRUE)
stopifnot(length(new_files) == 11)

lc_layers = map(set_names(new_files, basename(new_files)),
                read_tree_layer, .progress='Reading new-series layers')

new_noho = clip_and_mean_all(noho) |> mutate(Dataset='1985-2023 (v2023-5)', .before=1)
print(new_noho)
stopifnot(nrow(new_noho) == 11)
stopifnot(all(new_noho$Coverage > 0.40 & new_noho$Coverage < 0.70))

dataset_levels = c('2011-2021 (v2021-4)', '1985-2023 (v2023-5)')
noho_compare = bind_rows(old_noho, new_noho) |>
  mutate(Dataset=factor(Dataset, levels=dataset_levels))
stopifnot(nrow(noho_compare) == 22)

comparison = noho_compare |>
  pivot_wider(id_cols=Year, names_from=Dataset, values_from=Coverage) |>
  mutate(Diff = .data[[dataset_levels[2]]] - .data[[dataset_levels[1]]])
print(comparison)
cat('Max abs difference between datasets:', max(abs(comparison$Diff)), '\n')
stopifnot(any(abs(comparison$Diff) > 0.001))
cat('OK: new-series data loads, comparison frame has the expected shape, and the two datasets differ\n')
```

Run: `cd "/Users/kent/Dev/Noho" && Rscript "Trees/Compare/.tmp_check.R"`

Expected: prints the 11-row `new_noho` tibble (coverage in a plausible
0.50-0.65 range), then a `comparison` tibble showing Year, the old and
new coverage columns, and `Diff` — with at least one year showing a
non-trivial difference (this is the discrepancy the report exists to
show) — then `OK: new-series data loads, comparison frame has the
expected shape, and the two datasets differ`. No errors.

- [ ] **Step 4: Delete the temporary check script**

```bash
rm "/Users/kent/Dev/Noho/Trees/Compare/.tmp_check.R"
```

---

### Task 3: City-overall charts and summary table

**Files:**
- Modify: `Trees/Compare/Tree_canopy_dataset_comparison.qmd` (add a `### Tree canopy in Northampton` section after the `combine_data` chunk)

**Interfaces:**
- Consumes: `noho_compare`, `noho_change_compare` (Task 2), `dataset_linetypes`, `plot_caption`, `table_caption` (Task 1).
- Produces: nothing consumed by later tasks — this section is self-contained. (Tasks 4-5 add their own sections in the same file and don't depend on this one.)

- [ ] **Step 1: Add the section**

Append this after the `combine_data` chunk in
`Trees/Compare/Tree_canopy_dataset_comparison.qmd`:

````markdown
### Tree canopy in Northampton

This chart shows citywide tree canopy coverage for 2011-2021 as reported
by each dataset.

```{r noho_chart}
ggplot(noho_compare, aes(Year, Coverage, linetype=Dataset)) +
  geom_line(color='darkgreen', linewidth=1) +
  scale_linetype_manual(values=dataset_linetypes, name='Dataset') +
  scale_x_continuous(breaks=2011:2021, minor_breaks=NULL) +
  scale_y_continuous(minor_breaks=NULL, labels=label_percent(accuracy=0.1)) +
  labs(x='Year', y='Tree canopy coverage (%)',
       title='Northampton tree canopy coverage, 2011-2021: old vs new dataset',
       caption=plot_caption)
```

The next chart shows the same data as change since the 2011 baseline,
computed separately within each dataset.

```{r noho_change_chart}
ggplot(noho_change_compare, aes(Year, Change, linetype=Dataset)) +
  geom_line(color='darkgreen', linewidth=1) +
  scale_linetype_manual(values=dataset_linetypes, name='Dataset') +
  scale_x_continuous(breaks=2011:2021, minor_breaks=NULL) +
  scale_y_continuous(minor_breaks=NULL, labels=label_percent(accuracy=0.1)) +
  labs(x='Year', y='Change in tree canopy coverage (%)',
       title='Change in Northampton tree canopy coverage since 2011: old vs new dataset',
       caption=plot_caption)
```

This table shows the citywide coverage percent reported by each dataset
for every year 2011-2021, and the difference between them.

```{r noho_table}
noho_table_data = noho_compare |>
  select(Dataset, Year, Coverage) |>
  pivot_wider(names_from=Dataset, values_from=Coverage) |>
  mutate(Difference = `1985-2023 (v2023-5)` - `2011-2021 (v2021-4)`) |>
  arrange(Year)

noho_table_data |>
  gt() |>
  fmt_percent(columns=-Year, decimals=1) |>
  cols_label(
    `2011-2021 (v2021-4)` = 'Old (v2021-4)',
    `1985-2023 (v2023-5)` = 'New (v2023-5)'
  ) |>
  tab_header(title='Citywide tree canopy coverage: old vs new dataset, 2011-2021') |>
  tab_source_note(table_caption)
```

---
````

- [ ] **Step 2: Render the report and verify it succeeds**

Run: `cd "/Users/kent/Dev/Noho" && quarto render "Trees/Compare/Tree_canopy_dataset_comparison.qmd"`

Expected: exit code 0, no `ERROR` in the output, and
`Trees/Compare/Tree_canopy_dataset_comparison.html` is created or
updated.

- [ ] **Step 3: Spot-check the rendered content**

Run: `grep -c "old vs new dataset" "/Users/kent/Dev/Noho/Trees/Compare/Tree_canopy_dataset_comparison.html"`

Expected: a positive count (the chart titles containing that phrase are
embedded in the rendered HTML — confirms the charts actually rendered
into the page rather than the chunk silently failing/being skipped).

---

### Task 4: City-centers charts

**Files:**
- Modify: `Trees/Compare/Tree_canopy_dataset_comparison.qmd` (add a `### Tree canopy in city centers` section after the Task 3 section)

**Interfaces:**
- Consumes: `center_compare`, `center_change_compare` (Task 2), `dataset_linetypes`, `plot_caption` (Task 1).
- Produces: nothing consumed by later tasks.

- [ ] **Step 1: Add the section**

Append this after the Task 3 section (after the `noho_table` chunk and
its trailing `---`) in
`Trees/Compare/Tree_canopy_dataset_comparison.qmd`:

````markdown
### Tree canopy in city centers

These charts show tree canopy coverage within the three city centers of
Northampton, as reported by each dataset.

```{r center_chart}
ggplot(center_compare, aes(Year, Coverage, color=City, linetype=Dataset,
                            group=interaction(City, Dataset))) +
  geom_line(linewidth=1) +
  scale_linetype_manual(values=dataset_linetypes, name='Dataset') +
  scale_x_continuous(breaks=2011:2021, minor_breaks=NULL) +
  scale_y_continuous(minor_breaks=NULL, limits=c(0, NA),
                     labels=label_percent(accuracy=1)) +
  labs(x='Year', y='Tree canopy coverage (%)', color='City center',
       title='Tree canopy coverage in city centers, 2011-2021: old vs new dataset',
       caption=plot_caption)
```

The next chart shows the change in coverage since the 2011 baseline,
computed separately within each dataset and city center.

```{r center_change_chart}
ggplot(center_change_compare, aes(Year, Change, color=City, linetype=Dataset,
                                   group=interaction(City, Dataset))) +
  geom_line(linewidth=1) +
  scale_linetype_manual(values=dataset_linetypes, name='Dataset') +
  scale_x_continuous(breaks=2011:2021, minor_breaks=NULL) +
  scale_y_continuous(minor_breaks=NULL, labels=label_percent(accuracy=0.1)) +
  labs(x='Year', y='Change in tree canopy coverage (%)', color='City center',
       title='Change in tree canopy coverage in city centers since 2011: old vs new dataset',
       caption=plot_caption)
```

---
````

- [ ] **Step 2: Render the report and verify it succeeds**

Run: `cd "/Users/kent/Dev/Noho" && quarto render "Trees/Compare/Tree_canopy_dataset_comparison.qmd"`

Expected: exit code 0, no `ERROR` in the output.

- [ ] **Step 3: Spot-check the rendered content**

Run: `grep -c "city centers" "/Users/kent/Dev/Noho/Trees/Compare/Tree_canopy_dataset_comparison.html"`

Expected: a positive count.

---

### Task 5: Zoning-category charts

**Files:**
- Modify: `Trees/Compare/Tree_canopy_dataset_comparison.qmd` (add a `### Tree canopy by zoning category` section after the Task 4 section, and close out the report)

**Interfaces:**
- Consumes: `zone_compare`, `zone_change_compare` (Task 2), `dataset_linetypes`, `plot_caption` (Task 1).
- Produces: nothing (final section).

- [ ] **Step 1: Add the section and closing footer**

Append this after the Task 4 section in
`Trees/Compare/Tree_canopy_dataset_comparison.qmd`:

````markdown
### Tree canopy by zoning category

These charts show tree canopy coverage within five aggregated zoning
categories, as reported by each dataset.

```{r zone_chart}
ggplot(zone_compare, aes(Year, Coverage, color=Class, linetype=Dataset,
                          group=interaction(Class, Dataset))) +
  geom_line(linewidth=1) +
  scale_linetype_manual(values=dataset_linetypes, name='Dataset') +
  scale_x_continuous(breaks=2011:2021, minor_breaks=NULL) +
  scale_y_continuous(minor_breaks=NULL, limits=c(0, 0.7),
                     labels=label_percent(accuracy=1)) +
  labs(x='Year', y='Tree canopy coverage (%)', color='Zoning class',
       title='Tree canopy coverage by zoning category, 2011-2021: old vs new dataset',
       caption=plot_caption)
```

The next chart shows the change in coverage since the 2011 baseline,
computed separately within each dataset and zoning category.

```{r zone_change_chart}
ggplot(zone_change_compare, aes(Year, Change, color=Class, linetype=Dataset,
                                 group=interaction(Class, Dataset))) +
  geom_line(linewidth=1) +
  scale_linetype_manual(values=dataset_linetypes, name='Dataset') +
  scale_x_continuous(breaks=2011:2021, minor_breaks=NULL) +
  scale_y_continuous(minor_breaks=NULL, labels=label_percent(accuracy=0.1)) +
  labs(x='Year', y='Change in tree canopy coverage (%)', color='Zoning class',
       title='Change in tree canopy coverage by zoning category since 2011: old vs new dataset',
       caption=plot_caption)
```

<small>Copyright `r format(Sys.Date(), '%Y')` Kent S Johnson
<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">
  <img alt="Creative Commons License" style="border-width:0;vertical-align:middle;display:inline"
  src="https://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png" target="_blank"/></a>
  <span style='float:right;font-style: italic;'>`r Sys.Date()`</span></small>
````

- [ ] **Step 2: Render the full report and verify it succeeds**

Run: `cd "/Users/kent/Dev/Noho" && quarto render "Trees/Compare/Tree_canopy_dataset_comparison.qmd"`

Expected: exit code 0, no `ERROR` in the output,
`Trees/Compare/Tree_canopy_dataset_comparison.html` created/updated.

- [ ] **Step 3: Spot-check the rendered content end-to-end**

Run:

```bash
grep -c "zoning category" "/Users/kent/Dev/Noho/Trees/Compare/Tree_canopy_dataset_comparison.html"
grep -o 'Copyright [0-9]* Kent S Johnson' "/Users/kent/Dev/Noho/Trees/Compare/Tree_canopy_dataset_comparison.html"
```

Expected: the first command returns a positive count; the second prints
`Copyright 2026 Kent S Johnson` (confirms the report rendered all the way
through to the closing footer, i.e. no chunk errored out partway).

- [ ] **Step 4: Open the rendered report for a visual check**

Run: `open "/Users/kent/Dev/Noho/Trees/Compare/Tree_canopy_dataset_comparison.html"`

Expected: report opens in the browser. Confirm by eye: dashed vs. solid
lines are visually distinguishable in each chart, the legend shows both
`Dataset` (linetype) and, where applicable, the category (color), and
the three sections (Northampton, city centers, zoning) all show visibly
different old-vs-new lines consistent with the discrepancy the report
exists to document.

---

## Done

At this point `Trees/Compare/Tree_canopy_dataset_comparison.qmd` is
complete: introduction, citywide/centers/zoning raw and change charts
comparing both datasets, and a citywide summary table. Nothing has been
committed — that's left to the user.
