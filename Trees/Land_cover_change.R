# Look at changes in NLCD land cover data
# Updated for 2021 data
source(here::here('Trees/NLCD_helpers.R'))

lc_2001 = read_layer(
  here::here('data/NLCD_Land_Cover_2001-2021/NLCD_2001_Land_Cover_L48_20210604_WHD7sUg1ZU0HbVS6Agh3.tiff')
)

lc_2021 = read_layer(
  here::here('data/NLCD_Land_Cover_2001-2021/NLCD_2021_Land_Cover_L48_20230630_WHD7sUg1ZU0HbVS6Agh3.tiff')
)

# Changes in land cover class i.e. from-to
lc_change = 
  tibble(`2001`=values(lc_2001, mat=FALSE),
         `2021`=values(lc_2021, mat=FALSE)) %>% 
  filter(!is.na(`2001`)) %>% 
  mutate(`2001`=class_lookup[as.character(`2001`)],
         `2021`=class_lookup[as.character(`2021`)]) %>% 
  count(`2001`, `2021`, sort=TRUE) %>% 
  mutate(Acres = acre_per_raster * n)

#devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)

data = lc_change %>% 
  filter(`2001` != `2021`) %>% 
  make_long(`2001`, `2021`, value=Acres)

ggplot(data, 
       aes(x=x, next_x=next_x, node=node, next_node=next_node, 
           value=value, label=node, fill=node)) +
  geom_sankey() +
  geom_sankey_label(show.legend=FALSE) +
  scale_fill_manual('Land cover class', values=class_color_map) +
  theme_sankey(base_size = 12) +
  labs(x = NULL, title='Change in Northampton land cover, 2001-2021',
       subtitle='Showing changes only',
       caption='Data: mlrc.gov | Analysis: Kent Johnson')

# Make a polygon layer with areas that were forested in 2001
# and not in 2021
forest_values = 41:43
lost_forest = (lc_2001==41|lc_2001==42|lc_2001==43) & lc_2021!=41 & lc_2021!=42 & lc_2021!=43

lost_forest[lost_forest==0] = NA
was_forest = raster::mask(lc_2021, lost_forest)
lost_forest_poly = 
  as.polygons(was_forest) %>% 
  st_as_sf() %>% 
  group_by(Layer_1) %>% 
  summarize() %>% 
  st_union(by_feature=TRUE) %>% 
  mutate(Class=class_lookup[as.character(Layer_1)]) %>% 
  select(-Layer_1)

lost_forest_poly = st_transform(lost_forest_poly, st_crs(noho)) # Mass state plane

st_write(st_sf(lost_forest_poly), here::here('Trees/Lost_forest_2001_2021.gpkg'), delete_layer=TRUE)

# Make a polygon layer with areas that were cultivated in 2001
# and not in 2021
lost_cultivated = (lc_2001==81|lc_2001==82) & lc_2021!=81 & lc_2021!=82

lost_cultivated[lost_cultivated==0] = NA
was_cultivated = raster::mask(lc_2021, lost_cultivated)
lost_cultivated_poly = 
  as.polygons(was_cultivated) %>% 
  st_as_sf() %>% 
  group_by(Layer_1) %>% 
  summarize() %>% 
  st_union(by_feature=TRUE) %>% 
  mutate(Class=class_lookup[as.character(Layer_1)]) %>% 
  select(-Layer_1)

lost_cultivated_poly = st_transform(lost_cultivated_poly, st_crs(noho)) # Mass state plane

st_write(st_sf(lost_cultivated_poly), 
         here::here('Trees/Lost_forest_2001_2021.gpkg'), 
         layer='Lost_cultivated', delete_layer=TRUE)

# Make a polygon layer with areas that were <30% tree coverage in 2015
coverage_2016 = read_layer(
  here::here('data/NLCD_Tree_and_Land/NLCD_2016_Tree_Canopy_L48_20210831_cE3J3qNGK7bbzFDvKwex.tiff')
)

# Exclude areas of water and farm
cov_lt30 = coverage_2016<30 & lc_2021 != 11 & lc_2021 != 81 & lc_2021 != 82
cov_lt30 = mask(coverage_2016, cov_lt30, maskvalues=FALSE)
cov_lt30_poly = as.polygons(cov_lt30, dissolve=TRUE, values=FALSE) |> 
  st_as_sf() |> 
  st_union() |> 
  st_cast('POLYGON')

st_write(st_transform(cov_lt30_poly, st_crs(noho)), 
         here::here('Trees/Lost_forest_2001_2021.gpkg'), 
         layer='Coverage <30', delete_layer=TRUE)
