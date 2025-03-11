# Look at property valuation per acre
# Focus on commercial and residential properties

library(tidyverse)
library(leaflet)
library(mapview)
library(RColorBrewer)
library(sf)

# Parcels and assessed value from 2023, from MassGIS
path = here::here('Shapefiles/M214_parcels_gdb.zip')
#st_layers(path)
lots = read_sf(path, 'M214TaxPar')

assess = read_sf(path, 'M214Assess')

# Look at just residential, commercial and industrial property
# Exclude agricultural, recreational, educational, government and utility parcels
# See AllParcelsFY2024part2.xlsx for annotated use codes
# See 'FPRA/Cambridge Open Data/State Codes/classificationcodebook.pdf'
# for detailed descriptions of use codes
# Filter before aggregating, some locations have multiple use codes
resid_comm = assess |> 
  filter(TOTAL_VAL > 0) |> 
  filter((str_sub(USE_CODE, 1, 1) %in% c('0', '1', '3')) |
           (USE_CODE %in% c('400', '401', '402', '440', '441'))) |> 
  filter(!USE_CODE %in% c('132', '389', '392', '393')) |> 
  filter(!PROP_ID %in% 
           c('31A-067-001', '31B-201-001') # Dorms classed as residential
         )

# Fairway Village parcels seem to be misplaced
# Properties that are listed in parcel M_101792_899847
# are really in M_101713_899760
resid_comm = resid_comm |> 
  mutate(LOC_ID = 
    if_else(LOC_ID=='M_101792_899847', 'M_101713_899760', LOC_ID))

# Compute total value per location
# This aggregates the properties on a single lot, e.g. condos
value_by_locid = resid_comm |> 
  group_by(LOC_ID) |> 
  summarize(TOTAL_VAL=as.numeric(sum(TOTAL_VAL)),
            PROP_ID=list(PROP_ID),
            ADDR_NUM=first(ADDR_NUM),
            FULL_STR=first(FULL_STR),
            USE_CODE=list(USE_CODE),
            .groups='drop')

sq_meter_per_acre = 4046.86
lots_with_value = value_by_locid |> 
  left_join(lots) |> 
  mutate(SHAPE = SHAPE |> st_cast('MULTIPOLYGON'), # mapview needs this...
         Acres = SHAPE_Area / sq_meter_per_acre,
         Value_per_acre = TOTAL_VAL/Acres) |>
  st_as_sf()

# mapview(lots_with_value, zcol='Value_per_acre')

resid_comm_with_value = lots_with_value |> 
  mutate(Full_addr = str_to_title(if_else(is.na(ADDR_NUM), 
                                          FULL_STR, 
                                          paste(ADDR_NUM, FULL_STR)))) |> 
  st_transform('+proj=longlat +datum=WGS84')

# What is the average value per acre?
#sum(resid_comm_with_value$TOTAL_VAL)/sum(resid_comm_with_value$Acres)
# 424366.1

noho = read_sf(here::here('Shapefiles/Noho_outline/Noho_outline.gpkg')) |> 
  st_transform(4326)

# Mouse-over labels
format_dollar = scales::label_currency(scale=1/1000, suffix='K')
labels = unclass(str_glue_data(resid_comm_with_value,
  '{Full_addr}<br>',
  '{format_dollar(round(Value_per_acre, -3))}/acre<br>'
)) |>
  lapply(htmltools::HTML)

# Popup labels include all PROP_IDs
format_prop_id = function(ids) {
  str_glue(
    '<a href="https://northamptonma.s3.amazonaws.com/recordcardsfy25/{ids}.PDF" target="_blank">{ids}</a>'
  )
}

# Lookup table for use codes
use_code_path = "~/Dev/FPRA/Cambridge Open Data/State Codes/StateClassification.tsv"
use_code_lookup = read_tsv(use_code_path, skip=6, col_types='c', comment='#') |> 
  deframe()

format_prop_id_and_use_code = function(ids, codes) {
  if (length(ids)==1 && length(codes)==1)
    paste(use_code_lookup[[codes]], format_prop_id(ids), sep='<br>')
  else {
    paste(format_prop_id(ids), 
          lapply(codes, \(code) use_code_lookup[[code]]), 
          sep=' ', collapse='<br>')
  }
}

popups = unclass(str_glue_data(resid_comm_with_value,
  '{Full_addr}<br>',
  'Assessed at {format_dollar(round(TOTAL_VAL, -3))}<br>',
  '{round(Acres, 2)} acres<br>',
  '{format_dollar(round(Value_per_acre, -3))}/acre<br><br>',
  '{pmap(list(PROP_ID, USE_CODE), format_prop_id_and_use_code)}'
)) |>
  lapply(htmltools::HTML)

# Color palette
# These are approximate 10% quantiles plus 5% and 95% and 99%
breaks = c(0, 100000, 200000, 300000, 580000, 750000, 970000, 1250000, 
           1700000, 2250000, 3250000, 4250000, 11000000, 44000000)
pal = colorBin(colorRampPalette(brewer.pal(11, "RdYlBu"))(length(breaks)-1), 
               bins=breaks,
               domain=resid_comm_with_value$Value_per_acre, 
               reverse=TRUE)

map = leaflet(width='95%', height='700px', resid_comm_with_value) |> 
  addPolygons(stroke=TRUE, weight=1, color=~pal(Value_per_acre), opacity=1,
              fillColor = ~pal(Value_per_acre), fillOpacity = 0.8,
              label=labels, popup=popups,
              group = 'Value per acre') |> 
  addPolygons(data=noho, fill=FALSE,
              color='darkslategrey', weight=4) |> 
  addProviderTiles('Stadia.StamenTonerLite', group='Street') |> 
  addProviderTiles('Esri.WorldImagery', group='Satellite') |>  
  addLegend(pal=pal, values=~Value_per_acre, 
            title='Value per acre ($1000\'s)',
            labFormat=labelFormat('$', 'K', transform=function(x) x/1000)) |> 
  addLayersControl(baseGroups=c('Street', 'Satellite'),
    overlayGroups=c('Value per acre'),
    options=layersControlOptions(collapsed=FALSE))
map

# Explore cumulative value by value_per_acre
resid_comm_with_value = resid_comm_with_value |> 
  arrange(Value_per_acre) |> 
  mutate(cum_acres = cumsum(Acres),
         cum_value = cumsum(TOTAL_VAL))

# Explore the top half the total property value
half_value = sum(resid_comm_with_value$TOTAL_VAL)/2

# Which high-value properties make the top half of value?
half_value_properties = which(resid_comm_with_value$cum_value >= half_value)

# Cumulative acres at the half-value point
half_acres = resid_comm_with_value$cum_acres[half_value_properties |> head(1)]

# Value_per_acre at the half-value point
half_value_per_acre = resid_comm_with_value$Value_per_acre[half_value_properties |> head(1)]

# Total acres in the top half of value
half_value_acres = sum(resid_comm_with_value$Acres[half_value_properties])

# Number of properties in the top half of value
half_value_count = length(half_value_properties)

if (FALSE) {
ggplot(resid_comm_with_value, aes(cum_acres, cum_value)) +
  geom_line() +
  geom_hline(yintercept=half_value) +
  geom_vline(xintercept=half_acres) +
  labs(title='Cumulative value of Northampton properties',
       x='Cumulative acres', y='Cumulative value ($trillion)') +
  scale_x_continuous(labels=scales::label_comma()) +
  scale_y_continuous(labels=scales::label_dollar(scale=1e-9, suffix=' T')) +
  theme_minimal()

ggplot(resid_comm_with_value, aes(Acres, Value_per_acre)) + 
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

ggplot(resid_comm_with_value |> filter(Value_per_acre<=10000000), aes(Value_per_acre)) +
  geom_histogram(binwidth=200000, aes(fill=after_stat(pal(x)))) +
  scale_x_continuous(labels=scales::label_dollar(scale=1e-6, suffix=' M'),
                     breaks = 1:10*1e6, minor_breaks=NULL) +
  geom_vline(xintercept=median(resid_comm_with_value$Value_per_acre),
             linetype=2, color='grey') +
  geom_vline(xintercept=half_value_per_acre,
             linetype=2, color='grey') +
  scale_fill_identity() +
  labs(x='Value per acre ($million)', y='Number of properties',
       title='Value per acre of Northampton properties') +
  theme_minimal()
}

