# Look at changes in NLCD tree canopy data

source(here::here('Trees/NLCD_helpers.R'))

res_bump = 4

tc_2021_path = here::here("data/nlcd_tcc_CONUS_all/nlcd_tcc_CONUS_2021_v2021-4/nlcd_tcc_conus_2021_v2021-4.tif")
tc_2021 = read_layer(tc_2021_path) |> 
  disagg(res_bump)

tc_2011_path = here::here("data/nlcd_tcc_CONUS_all/nlcd_tcc_CONUS_2011_v2021-4/nlcd_tcc_conus_2011_v2021-4.tif")
tc_2011 = read_layer(tc_2011_path) |> 
  disagg(res_bump)

# 254 is a "non-processing" area
tc_2011[tc_2011==254] = NA

tc_change = tc_2021-tc_2011
range(tc_change)
qplot(values(tc_change), binwidth=1)
tc_changed = tc_change
tc_changed[tc_changed==0] = NA
writeRaster(tc_changed, 
            here::here('data/NLCD_TCC_Noho/nlcd_tcc_noho_change_2011_2021.tif'),
            overwrite=TRUE)

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
