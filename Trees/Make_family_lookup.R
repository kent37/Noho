# Make a lookup table from Genus -> Family that includes all genera 
# in the Davey inventory and new plantings

library(tidyverse)
library(taxize)

# The inventory
source(here::here('Trees/Read_and_clean_inventory.R'))

# New planting
source(here::here('Trees/Read_and_clean_planting.R'))

# List of genera from both sources
genera = c(inventory$Genus, planted$Genus) |> 
  unique() |> 
  sort()

# Get families from NCBI. This needs some interaction!
ncbi_families = tax_name(genera, get='family', db='ncbi', messages=FALSE)
ncbi_families = ncbi_families |> 
  mutate(family=replace_na(family, 'Unknown')) |> 
  select(Genus=query, Family=family)

# Save it out
write_csv(ncbi_families, here::here('Trees/Family_lookup.csv'))
