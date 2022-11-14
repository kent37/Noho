library(tidyverse)
library(sf)
library(leaflet)

path = '/Users/kent/Dev/Noho/Shapefiles/lots_20190816/lots_20190816.shp'

# From https://northamptonma.gov/932/Make-Maps-flooding-etc
lots = st_read(path) %>% 
  st_transform(4326) %>% 
  mutate(Owner2 = if_else(is.na(Owner2), '', Owner2))

# Major owners - many NAs in this data
owners = lots %>% 
  st_drop_geometry() %>% 
  count(Owner1, sort=TRUE)

lots %>% 
#  filter(Owner1=='GLOWATSKY EDWIN C & CARL E') %>% 
  leaflet() %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addPolygons(label=~paste(Owner1, Owner2), fillOpacity=0.1, weight=1, color='black')

# From https://northamptonma.gov/2040/Parcel-Ownership
parcels = readxl::read_xlsx(
  here::here('data/Parcel Ownership-TaxYear2021_spreadsheet_202011031029599965.xlsx')) %>% 
    mutate(Owner_2 = if_else(is.na(Owner_2), '', Owner_2))

# Consolidate names that appear to be different spellings of the same name
# List of <good name> = <names to replace>
duplicates = list(
  'SEVEN BRAVO TWO LLC' = 'SEVEN BRAVO  TWO LLC',
  'SUNWOOD DEVELOPMENT CORP' = 'SUNWOOD DEVELOPMENT DORP',
  'ALLARD\'S FARMS INC.' = c(
    'ALLARD FARMS INC',
    'ALLARDS FARMS INC',
    'ALLARD\'S FARMS INC',
    'ALLARDS FARM INC'
  ),
  'DUPREY NICHOLAS D & BETTY L' = c(
    'DUPREY NICHOLAS',
    'DUPREY NICHOLAS & BETTY L',
    'DUPREY NICHOLAS D & BETTY LOU',
    'DUPREY NICHOLAS DEAN & BETTY L'
  ),
  'ROCKWELL PLACE LLC' = c(
    'ROCKWELL PLACE',
    'ROCKWELL \\[PLACE'
  )
)

# Brute force...
for (good_name in names(duplicates))
  for (bad_name in duplicates[[good_name]]) {
    pattern = paste0('^', bad_name, '$')
    #print(pattern)
    parcels = mutate(parcels, 
      Owner_1 = str_replace(Owner_1, pattern, good_name))
  }

owners1 = parcels %>% 
  count(Owner_1, sort=TRUE)

# Don't care about these for now
ignore_owners = c("SMITH COLLEGE", "NORTHAMPTON CITY OF", "CITY OF NORTHAMPTON", 
"MASSACHUSETTS ELECTRIC COMPANY", "NORTHAMPTON HOUSING AUTHORITY", 
"MASSACHUSETTS COMMONWEALTH OF", 
"ROMAN CATHOLIC BISHOP OF")

top_owners = owners1 %>% 
  filter(n>=10) %>% 
  filter(!Owner_1 %in% ignore_owners) %>% 
  pull(Owner_1)

# Get parcels corresponding to top owners
# TODO
# Identify condos and get the corresponding parcel
top_owner_parcels = parcels %>% 
  filter(Owner_1 %in% top_owners) %>% 
  left_join(lots %>% select(Parcel_ID=PARCEL_ID, geometry)) %>% 
  st_as_sf()

top_owner_parcels %>% 
  leaflet() %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addPolygons(label=~paste(Owner_1, Owner_2), fillOpacity=0.1, weight=1, color='black')
