# Play with LiDAR

library(tidyverse)
library(lidR)

las = readLAS('~/Downloads/USGS_LPC_MA_ME_MA_QL1_UTM18_L1_2015_QL1_18TXM694687_NW_LAS_2018.laz',
              filter = "-keep_first -drop_z_below 5 -drop_z_above 50")
las_check(las)
plot(las)
