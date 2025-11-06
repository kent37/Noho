library(tidyverse)
library(readxl)

# Read lines 5 to 34 from the spreadsheet
results_raw <- read_excel(
  here::here("Voting/Northampton UNOFFICIAL November 4, 2025 Election Results.xlsx"),
  skip = 4,
  n_max = 30
)

results = results_raw |> 
  filter(!is.na(WARDS), WARDS != "TOTAL", WARDS != "All") |> 
  select(-(1:2)) |> 
  mutate(`Jillian Marie Duclos` = parse_number(`Jillian Marie Duclos`))

write_csv(results, here::here('Voting/Results2025.csv'))

mayor = results |> 
  select(WARDS, 3, 4) |> 
  mutate(margin = `Gina Louise Sciarra` - `Jillian Marie Duclos`)

write_csv(mayor, here::here('Voting/MayorResults2025.csv'))
