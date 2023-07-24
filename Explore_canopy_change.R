# Look at changes in NLCD tree canopy data

source(here::here('Trees/NLCD_helpers.R'))

res_bump = 4

tc_2021_path = "/Users/kent/Downloads/nlcd_tcc_CONUS_all/nlcd_tcc_CONUS_2021_v2021-4/nlcd_tcc_conus_2021_v2021-4.tif"
tc_2021 = read_layer(tc_2021_path) |> 
  disagg(res_bump)

tc_2011_path = "/Users/kent/Downloads/nlcd_tcc_CONUS_all/nlcd_tcc_CONUS_2011_v2021-4/nlcd_tcc_conus_2011_v2021-4.tif"
tc_2011 = read_layer(tc_2011_path) |> 
  disagg(res_bump)

# 254 is a "non-processing" area
tc_2011[tc_2011==254] = NA

tc_change = tc_2021-tc_2011

mean(values(tc_2011), na.rm=TRUE) # 57.1%
mean(values(tc_2021), na.rm=TRUE) # 56.5%

zoning = aggregated_zoning()

canopy_change = zoning |> 
  st_transform(nlcd_crs) |> 
  deframe() |> 
  map_dbl(~clip_and_mean_layer(tc_2011, mask=.)) |> 
  enframe(name='Zone', value='2011') |> 
  inner_join(zoning |> 
    st_transform(nlcd_crs) |> 
    deframe() |> 
    map_dbl(~clip_and_mean_layer(tc_2021, mask=.)) |> 
    enframe(name='Zone', value='2021')) |> 
  mutate(Change=`2021`-`2011`)

plot_caption = 'Data: mlrc.gov | Analysis: Kent Johnson'
theme_set(theme_minimal(base_size=12) + theme(legend.position='bottom'))

ggplot(canopy_change) +
  geom_col(aes(Zone, Change/100, fill=Zone)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y='Percent change', 
       title='Change in tree canopy coverage by zoning category, 2011-2021',
       caption=plot_caption) +
  theme(legend.position='none')
