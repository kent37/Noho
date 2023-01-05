source(here::here('Trees/NLCD_helpers.R'))
library(leaflet)

forest_loss = read_layer(
  here::here('data/NLCD_Tree_and_Land/NLCD_Forest_Loss_2001_2019.tiff')
) %>% 
  disaggregate(fact=2) %>% 
  projectRasterForLeaflet(method='ngb')

colors = function(vals) {
  cols = c('#000000FF', '#FFFFFF00')
  if_else(is.na(vals), '#00000000', cols[vals+1])
}

leaflet() %>% 
  addTiles() %>% 
  addRasterImage(forest_loss, project=FALSE, opacity=0.5, 
                 colors=colors, )
