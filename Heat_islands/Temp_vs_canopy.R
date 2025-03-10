# Compare surface temperature with canopy percent
# Thank you claude.ai!

# Load required libraries
library(terra)
library(sf)
library(tidyverse)

# Read the rasters and polygon
temperature <- rast(here::here('8 View Ave/Noho_temperature_7_2_2024_greyscale.tif'))  # EPSG:4326
canopy <- rast(here::here('data/NLCD_TCC_Noho/nlcd_tcc_noho_2021.tif'))  # EPSG:5070
noho <- st_read(here::here('Shapefiles/Noho_outline/Noho_outline.gpkg'),
  quiet=TRUE)  # EPSG:26986

# Reproject everything to EPSG:26986 (NAD83 / Massachusetts Mainland)
temperature <- project(temperature, "EPSG:26986")
canopy <- project(canopy, "EPSG:26986")

# Clip rasters to the polygon extent
temp_clip <- crop(temperature, noho)
temp_clip <- mask(temp_clip, noho)
canopy_clip <- crop(canopy, noho)
canopy_clip <- mask(canopy_clip, noho)

# Ensure rasters have the same extent and resolution
canopy_clip <- resample(canopy_clip, temp_clip)

# Convert raster values to degrees F
to_F = function(v) {
  v/255*(130-70)+70
}

# Convert rasters to data frames for ggplot
temp_df <- as.data.frame(temp_clip, xy = TRUE) |> 
  mutate(`vis-gray` = to_F(`vis-gray`))
canopy_df <- as.data.frame(canopy_clip, xy = TRUE)

# Combine the data frames
combined_df <- merge(temp_df, canopy_df, by = c("x", "y"))
names(combined_df) <- c("x", "y", "temperature", "canopy")

# Correlation analysis
correlation <- cor.test(combined_df$temperature, combined_df$canopy)
print(correlation)

# Linear regression
lm_model <- lm(canopy ~ temperature, data = combined_df)
model_summary <- summary(lm_model)

# Extract R-squared value
r_squared <- model_summary$r.squared

# Print R-squared to console
cat("R-squared:", round(r_squared, 3), "\n")

# Print full model summary
print(model_summary)

# Create the comparison plot
p3 = combined_df |> 
#  filter(temperature > min(temperature) + 2) |> 
ggplot(aes(x = temperature, y = canopy)) +
  geom_point(shape='.', alpha = 0.1) +
#  geom_smooth(color = "red", method='lm') +
  scale_y_continuous(limits=c(0, NA)) +
  labs(x = "Temperature °F", 
       y = "Canopy Cover %",
       title = "Comparison of Temperature and Canopy Cover in Northampton",
       subtitle = paste0("July 2, 2024 temperature vs 2021 tree canopy"),
       caption='Temperature: Landsat 9 | Canopy: www.mrlc.gov | Analysis: Kent S Johnson') +
  theme_minimal()

# Save the plot
# ggsave(here::here("Heat_islands/temperature_canopy_comparison.png"), 
#        width = 10, height = 8, dpi = 300)

# Create a multi-panel visualization
library(patchwork)

# Temperature map
p1 <- ggplot() +
  geom_raster(data = temp_df, aes(x = x, y = y, fill = `vis-gray`)) +
  scale_fill_viridis_c(name = "Temp (°F)") +
  theme_void() +
  labs(title = "Surface Temperature")

# Canopy map
p2 <- ggplot() +
  geom_raster(data = canopy_df, aes(x = x, y = y, fill = nlcd_tcc_noho_2021)) +
  scale_fill_viridis_c(name = "Canopy %", option = "viridis") +
  theme_void() +
  labs(title = "Tree Canopy Cover")

# Scatterplot
p3 <- combined_df |> 
  ggplot(aes(x = temperature, y = canopy)) +
  geom_point(shape='.', alpha = 0.1) +
  labs(x = "Temperature °F", y = "Canopy Cover %") +
  theme_minimal()

# Combine plots
(p1 + p2) / p3

# If you want to save the clipped rasters
# writeRaster(temp_clip, here::here("Heat_islands/temperature_clipped.tif"))
# writeRaster(canopy_clip, here::here("Heat_islands/canopy_clipped.tif"))

# Bivariate plot / map
library(pals)
library(classInt)
library(legendry)

cols <- stevens.pinkgreen; nbins <- 3

# categorize temp & canopy into 3 percentile bins
brkst <- classIntervals(combined_df$temperature, n=nbins, style='quantile')

brksc <- classIntervals(combined_df$canopy, n=nbins, style='quantile')

classt <- findCols(brkst)
classc <- findCols(brksc)
# convert x,y classes into a joint class x+3(y-1)
combined_df$class2 <- classc + nbins*(classt-1)

design = '
123
456
789
'
combined_df |> 
  ggplot(aes(x = temperature, y = canopy, color=factor(class2))) +
  geom_point(size=1, alpha = 0.1) +
  scale_color_manual(values=cols()) +
  labs(x = "Temperature °F", y = "Canopy Cover %") +
  theme_minimal()

ggplot() +
  geom_raster(data = combined_df, aes(x = x, y = y, fill = factor(class2))) +
  scale_fill_manual(values=cols()) +
  guides(fill = guide_legend_base(design = design)) +
  theme_void() +
  labs(title = "Tree Canopy Cover")
