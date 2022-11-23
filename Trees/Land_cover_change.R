# Look at changes in NLCD land cover data

source(here::here('Trees/NLCD_helpers.R'))

lc_2001 = read_layer(
  here::here('data/NLCD_Tree_and_Land/NLCD_2001_Land_Cover_L48_20210604_cE3J3qNGK7bbzFDvKwex.tiff')
)

lc_2019 = read_layer(
  here::here('data/NLCD_Tree_and_Land/NLCD_2019_Land_Cover_L48_20210604_cE3J3qNGK7bbzFDvKwex.tiff')
)

# Changes in land cover class i.e. from-to
lc_change = 
  tibble(`2001`=getValues(lc_2001),
         `2019`=getValues(lc_2019)) %>% 
  filter(!is.na(`2001`)) %>% 
  mutate(`2001`=class_lookup[as.character(`2001`)],
         `2019`=class_lookup[as.character(`2019`)]) %>% 
  count(`2001`, `2019`, sort=TRUE) %>% 
  mutate(Acres = acre_per_raster * n)

#devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)

data = lc_change %>% 
  filter(`2001` != `2019`) %>% 
  make_long(`2001`, `2019`, value=Acres)

ggplot(data, 
       aes(x=x, next_x=next_x, node=node, next_node=next_node, 
           value=value, label=node, fill=node)) +
  geom_sankey() +
  geom_sankey_label(show.legend=FALSE) +
  scale_fill_manual('Land cover class', values=class_color_map) +
  theme_sankey(base_size = 12) +
  labs(x = NULL, title='Change in Northampton land cover, 2001-2019',
       subtitle='Showing changes only',
       caption='Data: mlrc.gov | Analysis: Kent Johnson')
