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
combined_df |> 
#  filter(canopy>0) |> 
ggplot(aes(x = temperature, y = canopy)) +
  #geom_hex() +
  geom_point(shape='.', alpha = 0.1) +
  geom_smooth(color = "red") +
  scale_y_continuous(limits=c(0, NA)) +
  labs(x = "Temperature °F", 
       y = "Canopy Cover %",
       title = "Comparison of Temperature and Canopy Cover in Northampton",
       subtitle = paste0("July 2, 2024 temperature vs 2021 tree canopy\n",
                        "Correlation = ", 
                        round(correlation$estimate, 3)),
       caption='Temperature: Landsat 9 | Canopy: www.mrlc.gov | Analysis: Kent S Johnson') +
  theme_minimal()

# Save the plot
ggsave("temperature_canopy_comparison.png", width = 10, height = 8, dpi = 300)

# If you want to save the clipped rasters
writeRaster(temp_clip, "temperature_clipped.tif")
writeRaster(canopy_clip, "canopy_clipped.tif")
