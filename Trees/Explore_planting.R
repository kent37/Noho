# Explore Noho tree planting data
library(tidyverse)
library(googlesheets4)
library(gt)
library(lubridate)
library(mapview)
library(sf)
library(tidygeocoder)

planted_raw = read_sheet(
  'https://docs.google.com/spreadsheets/d/1rIJKxHEv54ULyM4BhTkrGKDITdbv34X5bEDFp6gRx_4/edit#gid=1562980960',
  sheet=1,
  col_types='c'
)

planted = planted_raw |> 
  select(`#`:Notes) |> 
  rename(Num=`#`) |> 
  mutate(Num=parse_number(Num),
         Year = year(mdy(`Date Planted`)),
         Year = if_else(is.na(Year), parse_number(`Date Planted`), Year))

# Sort out the section of dead trees
planted$died = FALSE
planted$died[1929:2039] = TRUE

# Can we geocode the addresses?
addresses = planted |> 
  select(Num, Street) |> 
  unique() |> 
  mutate(Street=paste(Num, Street), 
         City='Northampton', State='MA', Country='USA') |> 
  select(-Num)

coded = geocode(addresses,
                street=Street, city=City, state=State, country=Country,
                method='geocodio',
                full_results=TRUE)

coded_sf = st_as_sf(coded, coords=c('long', 'lat'), crs=4326)

mapview(coded_sf, label='Street', zcol='accuracy')

# Look at percents by species, genus and family
# Living only for now until we understand the data better
int_pct = function(val) scales::percent(val, accuracy=1)

living = planted |> filter(!died)

living |> 
  count(Species, name='Count', sort=TRUE) |> 
  mutate(Percent=int_pct(Count/sum(Count))) |> 
  head(10) |> 
  gt() |> 
  tab_header(title='Top 10 species planted, 2015-2022')

living |> 
  count(Genus, name='Count', sort=TRUE) |> 
  mutate(Percent=int_pct(Count/sum(Count))) |> 
  head(10) |> 
  gt() |> 
  tab_header(title='Top 10 genera planted, 2015-2022')

living |> 
  count(Family, name='Count', sort=TRUE) |> 
  mutate(Percent=int_pct(Count/sum(Count))) |> 
  head(10) |> 
  gt() |> 
  tab_header(title='Top 10 families planted, 2015-2022')
