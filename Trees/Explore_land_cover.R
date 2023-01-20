# Explore NLCD tree and land cover data

source(here::here('Trees/NLCD_helpers.R'))

# Read land cover data
lc_files = list.files(here::here('data/NLCD_Tree_and_Land/'), 
                      pattern = '^NLCD_\\d{4}_Land_Cover.*\\.tiff$',
                      full.names=TRUE)
lc_counts = 
  map_dfr(set_names(lc_files, basename(lc_files)), 
          count_layer, .id='path')

lc_counts = lc_counts %>% 
  mutate(Name = name_lookup[value],
         Class = class_lookup[value],
         Year = as.integer(str_extract(path, '\\d{4}')),
         Fraction = n/raster_count,
         Acres = Fraction * noho_acres)

lc_class_counts = lc_counts %>% 
  group_by(Year, Class) %>% 
  summarize(Fraction=sum(Fraction), Acres=sum(Acres), 
            .groups='drop')

# What are the fractions for 2019?
lc_counts %>% 
  filter(Year==2019) %>% 
  select(Name, Fraction) %>% 
  arrange(-Fraction)

lc_class_counts %>% 
  filter(Year==2019) %>% 
  arrange(-Fraction)

# Check
lc_counts %>% filter(Year==2019) %>% pull(Fraction) %>% sum()

# Land cover types with > 1% coverage in some year
covers_to_include = lc_counts %>% 
  group_by(Name) %>% 
  summarize(max_frac = max(Fraction)) %>% 
  filter(max_frac >= 0.01) %>% 
  pull(Name)

theme_set(theme_minimal())
NLCD_year_axis = scale_x_continuous(breaks=seq(2001, 2019, 3),
                     minor_breaks=NULL)

# Land cover percent by year
ggplot(lc_counts %>% filter(Name %in% covers_to_include), 
       aes(Year, Fraction, color=Name)) + 
  geom_line(linewidth=2) +
  scale_color_manual('Land Cover Class', 
    values=color_map[names(color_map) %in% covers_to_include]) +
  scale_y_continuous('Percent Land Cover', 
                     labels=scales::label_percent()) +
  NLCD_year_axis +
  labs(title='Land Cover Change in Northampton, 2001-2019',
       caption='Data: mlrc.gov | Chart: Kent Johnson')

# Land cover class percent by year
ggplot(lc_class_counts, aes(Year, Fraction, color=Class)) + 
  geom_line(linewidth=2) +
  scale_color_manual('Land Cover Class',
    values=class_color_map) +
  scale_y_continuous('Percent Land Cover', 
                     labels=scales::label_percent()) +
  NLCD_year_axis +
  labs(title='Land Cover Change in Northampton, 2001-2019',
       caption='Data: mlrc.gov | Chart: Kent Johnson')

# Show change in land cover from a 2001 baseline
lc_change = lc_counts %>% 
#  filter(Name %in% covers_to_include) %>% 
  arrange(Year) %>% 
  group_by(Name) %>% 
  mutate(Change = Acres-Acres[1]) %>% 
  ungroup()

p = ggplot(lc_change, aes(Year, Change, color=Name)) +
  geom_line(linewidth=1) +
  scale_color_manual('Land Cover Class', 
#   values=color_map[names(color_map) %in% covers_to_include]) +
    values=color_map) +
  scale_y_continuous('Change in Land Cover (Acres)') +
  NLCD_year_axis +
  labs(title='Land Cover Change in Northampton, 2001-2019',
       subtitle='Change in acres from 2001 baseline',
       caption='Data: mlrc.gov | Chart: Kent Johnson')

ggplotly(p)

# Show change in land cover class from a 2001 baseline
lc_class_change = lc_class_counts %>% 
  arrange(Year) %>% 
  group_by(Class) %>% 
  mutate(Change = Acres-Acres[1]) %>% 
  ungroup()

p = ggplot(lc_class_change, aes(Year, Change, color=Class)) +
  geom_line(linewidth=1) +
  scale_color_manual('Land Cover Class',
    values=class_color_map) +
  scale_y_continuous('Change in Land Cover (Acres)') +
  NLCD_year_axis +
  labs(title='Land Cover Change in Northampton, 2001-2019',
       subtitle='Change in acres from 2001 baseline',
       caption='Data: mlrc.gov | Chart: Kent Johnson')

ggplotly(p)
