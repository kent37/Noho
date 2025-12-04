# Read and clean 2024 planting data

planted_raw_2024 = readxl::read_xlsx(
  here::here('Trees/Hamp_Trees_Planted_2024.xlsx'), 
  skip=1) |> 
  select(-6) # Blank column

planted_2024 <- planted_raw_2024 |>
  rename(Num = Number) |> 
  mutate(
    Year = 2024,
    dead = FALSE,
    Num = parse_number(Num),
    # Extract genus (everything up to first space)
    Genus = str_extract(`Scientific Name`, "^[^ ]+"),
    
    # Figure out species and cultivar
    # Remove genus and optional "x " to get remaining text
    remaining = str_trim(str_remove(`Scientific Name`, "^[^ ]+ ?(x )?"))
  ) |>
  mutate(
    # Check if it's a single quoted phrase (e.g., 'Spring Snow' or "Prairifire")
    # or a single unquoted word
    is_single_unit = str_detect(remaining, "^['\"][^'\"]+['\"]$|^[^ ]+$"),
    
    # Extract species
    Species = case_when(
      # If no remaining text, Species is NA
      remaining == "" ~ NA_character_,
      # If single unit (word or quoted phrase), that's the species
      is_single_unit ~ str_remove_all(remaining, "^['\"]|['\"]$"),
      # If multiple words, first word (or first quoted phrase) is species
      # Check for quoted first word
      str_detect(remaining, "^['\"][^'\"]+['\"] ") ~ 
        str_extract(remaining, "^['\"][^'\"]+['\"]") |> str_remove_all("['\"]"),
      # Otherwise first unquoted word
      TRUE ~ str_extract(remaining, "^[^ ]+")
    ),
    
    # Extract cultivar  
    Cultivar = case_when(
      # If single unit or no remaining, no cultivar
      is_single_unit | remaining == "" ~ NA_character_,
      # If starts with quoted word followed by space, extract after it
      str_detect(remaining, "^['\"][^'\"]+['\"] ") ~
        str_trim(str_remove(remaining, "^['\"][^'\"]+['\"] ")),
      # Otherwise, everything after first word is cultivar
      TRUE ~ str_trim(str_remove(remaining, "^[^ ]+ ?"))
    )
  ) |>
  select(-remaining, -is_single_unit)

# planted_2024 |>
#   select(`Scientific Name`, Genus, Species, Cultivar) |>
#   distinct() |>
#   arrange(Genus, Species) |>
#   View()

planted_2024 = planted_2024 |> 
  mutate(Binomial = binomial_name(`Scientific Name`),
        # Fill in names to match older data
        Type = NA,
        'Date Planted' = NA,
        'Planting Project' = NA,
        'Water Bag' = NA,
        'Wilt Proof' = NA,
        'Trunk Wrap' = NA,
        'Pruning Date' = NA,
        Notes = NA)

# Note: 2024 data does not include ward, so we have to join it
# with planting_locations before combining with the older data
# Check for missing locations
missing_locations_2024 = planted_2024 |> 
  anti_join(planting_locations)
stopifnot(nrow(missing_locations_2024)==0)

planted_2024 = planted_2024 |> 
  inner_join(planting_locations) |> 
  select(-geom)
