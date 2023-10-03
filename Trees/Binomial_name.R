# Create a binomial name from the scientific name in the planting data
binomial_name = function(sci_name) {
  case_when(
    # Other
    str_starts(sci_name, 'Gleditsia triacanthos') ~ 'Gleditsia triacanthos',
    
    # Cultivar with ®
    str_ends(sci_name, '®') ~ str_remove(sci_name, ' [^ ]+®$'),
    
    # Hybrids where the hybrid is in (possibly smart!) quotes
    str_detect(sci_name, ' x [\'"“‘]') ~ sci_name,
    
    # Now it's safe to strip any trailing quoted string, it is the cultivar
    str_detect(sci_name, '[\'"“‘]') ~ str_remove(sci_name, ' *[\'"“‘].*$'),
    
    # Normal two-word name
    .default=sci_name
  ) |> 
    # Normalize quotes to all single
    str_replace_all('[\'"“”]', "'")
}

# Test for binomial name
local({
  library(testthat)
  test_cases = tribble(
    ~sci, ~binom,
    # Normal two-word name
    'Celtis occidentalis', 'Celtis occidentalis',
    'Quercus alba', 'Quercus alba', 
    
    # Cultivar with ®:
    'Tilia tomentosa Sterling®', 'Tilia tomentosa', 
    
    # Hybrids where the hybrid is in (possibly smart!) quotes:
    'Malus x “Prairifire”', "Malus x 'Prairifire'", 
    'Malus x "Prairifire"', "Malus x 'Prairifire'", 
    "Malus x 'royal raindrop'", "Malus x 'royal raindrop'", 
    
    # Other:
    "Gleditsia triacanthos var. inermis Street Keeper® ('Draves') (PP21698, CPBR4741)", 
      "Gleditsia triacanthos", 
    "Gleditsia triacanthos var. inermis 'Suncole'", "Gleditsia triacanthos", 
    "Gleditsia triacanthos f. inermis 'Skycole' Skyline®", "Gleditsia triacanthos",
    
     # Cultivar with single or double quotes and possible spaces:
    "Liquidambar styraciflua ‘Hapdell’ HAPPIDAZE", "Liquidambar styraciflua",
    "Ulmus minor 'Accolade'", "Ulmus minor", 
    'Ulmus americana "Valley Forge"', 'Ulmus americana', 
    
    # Hybrids, possibly with cultivar:
    'Platanus × acerifolia', 'Platanus × acerifolia', 
    "Platanus x acerifolia 'Bloodgood'", "Platanus x acerifolia", 
    'Platanus x acerifolia "Morton Circle"', 'Platanus x acerifolia', 
  )
  
  pmap(test_cases, function(sci, binom) expect_equal(binomial_name(sci), binom))
  invisible()
})
