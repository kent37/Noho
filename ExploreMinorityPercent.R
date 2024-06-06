library(tidycensus)
library(mapview)

v20 <- load_variables(2020, "pl", cache = TRUE)

block_data = get_decennial(
  geography='block',
  variables=c('P2_001N', 'P2_005N'),
  state='MA',
  county='Hampshire',
  geometry=TRUE,
  output='wide',
  keep_geo_vars=TRUE
)

noho = read_sf(here::here('Shapefiles/Noho_outline/Noho_outline.gpkg'))


block_data = block_data |> 
  st_transform(st_crs(noho)) |> 
  mutate(bg=str_sub(NAME, 13))

noho_data = block_data[noho,] |> 
  mutate(
    NonWhite=POP20-P2_005N,
    Pct_NonWhite=NonWhite/POP20
  )

# Checking against MassGIS - looks good
noho_bg = noho_data |>
  summarize(.by=bg, across(geometry, st_union),
            POP20=sum(POP20),
            White=sum(P2_005N),
            NonWhite=sum(NonWhite),
            Pct_NonWhite=NonWhite/POP20)

mapview(noho_data, zcol='NonWhite')
