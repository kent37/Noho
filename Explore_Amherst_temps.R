library(tidyverse)
library(lubridate)

# Temperature data from https://www.ncdc.noaa.gov/cdo-web/
path = 'data/Amherst_temp_2012-2022.csv'
temps = read_csv(path)

temps = temps %>% 
  filter(!is.na(TMAX)) %>% 
  mutate(year=year(DATE), yday=yday(DATE))

theme_set(theme_minimal())

ggplot(temps, aes(yday, TMAX, color=year==2022, group=year)) + 
  geom_line()

# Figure out the first of each month for axis labels
firsts = 