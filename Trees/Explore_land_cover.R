# Explore NLCD tree and land cover data

source(here::here('Trees/NLCD_helpers.R'))

lc_2019 = raster::raster(
  here::here('data/NLCD_Tree_and_Land/NLCD_2019_Land_Cover_L48_20210604_cE3J3qNGK7bbzFDvKwex.tiff')
)
res_bump=4
lc_2019_x4 = raster::disaggregate(lc_2019, res_bump)

# Compare naive counting of regions with exactextractr
zoning = aggregated_zoning()

rural = zoning %>% filter(Class=='Rural')

naive = clip_and_count_layer_slow(lc_2019_x4, rural) %>% 
  mutate(Class = class_lookup[value],
         Fraction=n/sum(n),
         Acres=n * acre_per_raster / res_bump^2)

exact = clip_and_count_layer(lc_2019, rural) %>% 
  mutate(Class = class_lookup[as.character(value)],
         Fraction=n/sum(n),
         Acres=n * acre_per_raster)


# Compare the two methods
res_bump = 4

naive_all = map_dfr(zoning$Class, function(cls) {
  cat(cls, '\n')
  mask = zoning %>% 
    filter(Class==cls) %>% 
    as_Spatial()
  clip_and_count_layer_slow(lc_2019_x4, mask) %>% 
    mutate(Class=cls, n=n/res_bump^2)
})

exact_all = map_dfr(zoning$Class, function(cls) {
  cat(cls, '\n')
  mask = zoning %>% 
    filter(Class==cls)
  clip_and_count_layer(lc_2019, mask) %>% 
    mutate(Class=cls)
})

both = inner_join(naive_all, 
                  exact_all %>% mutate(value=as.character(value)),
                  by=c('value', 'Class'),
                  suffix=c('.naive', '.exact')) %>% 
  select(value, Class, everything()) %>% 
  mutate(
    value=class_lookup[value],
    diff=n.naive-n.exact, 
    diff_pct=scales::percent(diff/n.exact))
