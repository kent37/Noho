# Compare surface temperature with canopy percent
# Thank you claude.ai!

# Load required libraries
library(tidyverse)
library(classInt)
library(ggforce)
library(pals)
library(patchwork)
library(sf)
library(terra)

# Read the rasters and polygon
temperature <- 
  rast(here::here('8 View Ave/Noho_temperature_7_2_2024_greyscale.tif'))  # EPSG:4326
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

# Create a multi-panel visualization
# Temperature map
p1 <- ggplot() +
  geom_raster(data = temp_df, aes(x = x, y = y, fill = `vis-gray`)) +
  scale_fill_gradientn(colors=c("#f3f3f3", "#eac5dd", "#e6a3d0")) +
  coord_equal() +
  labs(title = "Surface Temperature", fill='°F') +
  theme_void() +
  theme(plot.title=element_text(face='bold'))

# Canopy map
p2 <- ggplot() +
  geom_raster(data = canopy_df, aes(x = x, y = y, fill = nlcd_tcc_noho_2021)) +
  scale_fill_gradientn(colors=c("#f3f3f3", "#c2f1ce", "#8be2af")) +
  coord_equal() +
  labs(title = "Tree Canopy Cover", fill='Canopy %') +
  theme_void() +
  theme(plot.title=element_text(face='bold'))

# Combine plots
patchwork1 = (p1 + p2) + theme(plot.margin = margin(0, 0, 0, 0, "pt")) +
  plot_annotation(
     title = "Temperature and Canopy Cover in Northampton",
     subtitle = paste0("July 2, 2024 temperature vs 2021 tree canopy"),
     caption='Temperature: Landsat 9 | Canopy: www.mrlc.gov | Analysis: Kent S Johnson',
     theme=theme(plot.title=element_text(face='bold')))
  
# Bivariate plot / map
cols <- stevens.pinkgreen; nbins <- 3

# categorize temp & canopy into 3 bins using
# fixed breaks that isolate the main cluster of low temp / high canopy
brkst <- classIntervals(combined_df$temperature, n=nbins, style='fixed',
                        fixedBreaks = c(74, 85, 98, 130))
brksc <- classIntervals(combined_df$canopy, n=nbins, style='fixed',
                        fixedBreaks = c(0, 40, 75, 96))

classt <- findCols(brkst)
classc <- findCols(brksc)

# convert x,y classes into a joint class x+3(y-1)
combined_df$class2 <- classc + nbins*(classt-1)

# Biplot map
p4 = ggplot() +
  geom_raster(data = combined_df, aes(x = x, y = y, 
                                      fill = paste(classt, classc))) +
  scale_fill_manual(values=cols(), guide=NULL) +
  coord_equal() +
  labs(x=NULL, y=NULL, 
       title = "Areas with high canopy have low temperatures, and vice versa",
       subtitle = paste0("July 2, 2024 temperature vs 2021 tree canopy"),
       caption='Temperature: Landsat 9 | Canopy: www.mrlc.gov | Analysis: Kent S Johnson') +
  theme_void() +
  theme(plot.title=element_text(face='bold'),
        axis.title=element_text(face='bold'))

# The legend is an inset plot
legend_data = expand.grid(
  Canopy=c('Low', 'Med', 'High'),
  Temp=c('Low', 'Med', 'High')
) |> bind_cols(color=cols())
  
legend = ggplot(legend_data, aes(Temp, Canopy, fill=color)) +
  geom_tile() +
  scale_fill_identity() +
    coord_equal() +
  labs(x='Temperature →', y='Canopy →') +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        axis.text=element_blank())

patchwork2 = p4 + inset_element(legend, left = 0.7, bottom = 0.7, right = 0.95, top = 0.95)

# Scatterplot using the same color scales - not great
# p5 = combined_df |> 
#   ggplot(aes(x = temperature, y = canopy, color=factor(class2))) +
#   geom_point(size=.2, alpha = 0.1) +
#   scale_x_continuous(limits = c(75, 115)) +
#   scale_color_manual(values=cols(), guide='none') +
#   labs(x = "Temperature °F", y = "Canopy Cover %") +
#   theme_minimal() +
#   theme(axis.title=element_text(face='bold'))

# Try coloring continuously
# Function to blend colors based on two variables
create_blended_colors <- function(temp, canopy) {
  # Normalize values to 0-1 range
  temp_norm <- scales::rescale(temp, to = c(0, 1)) # Note: avoid terra::rescale
  canopy_norm <- scales::rescale(canopy, to = c(0, 1))
  
  # Get colors from each palette based on normalized values
  temp_colors <- colorRampPalette(c("#f3f3f3", "#eac5dd", "#e6a3d0"))(100)
  canopy_colors <- colorRampPalette(c("#f3f3f3", "#c2f1ce", "#8be2af"))(100)
  
  # Get indices for each palette
  temp_idx <- pmax(1, pmin(100, round(temp_norm * 99) + 1))
  canopy_idx <- pmax(1, pmin(100, round(canopy_norm * 99) + 1))
  
  # Extract colors as RGB values
  temp_col <- col2rgb(temp_colors[temp_idx])
  canopy_col <- col2rgb(canopy_colors[canopy_idx])
  
 # Blend the colors by multiplication (creates darkening effect)
  # Multiply each RGB component
  blended_r <- temp_col[1,] * canopy_col[1,] / 255^2
  blended_g <- temp_col[2,] * canopy_col[2,] / 255^2
  blended_b <- temp_col[3,] * canopy_col[3,] / 255^2

  # Convert back to hex colors
  rgb(blended_r, blended_g, blended_b)
}

# Create blended colors for each point
combined_df$point_color <- 
  create_blended_colors(combined_df$temperature, combined_df$canopy)

ellipses = tribble(
  ~x0, ~y0, ~a, ~b, ~angle, ~color,
   80,  85,  5, 10, 0, "#5bb27f",
  105,  15,  5, 10, 0, '#b673a0'
)

arrows = tribble(
  ~x, ~y, ~xend, ~yend, ~color,
  95, 95,    84,    91, "#5bb27f",
  114,32,   110,    17, '#b673a0'
)

p6 = ggplot() +
  geom_point(data=combined_df,
             aes(x = temperature, y = canopy, color=point_color),
             shape='.', alpha = 1) +
  geom_ellipse(data=ellipses, 
               aes(x0=x0, y0=y0, a=a, b=b, angle=angle, color=color)) +
  # Annotate the cool trees
  geom_curve(data=arrows[1,],
    aes(x = x, y = y, xend = xend, yend = yend, color=color),
    arrow = arrow(length = unit(0.3, "cm")),
    curvature = 0.3
  ) +
  annotate("text", x = 96, y = 95, color='grey30',
           label = "Many locations have\nhigh tree canopy and\nlower temperatures", 
           fontface='bold', hjust = 0, vjust=1.) +
  # Annotate the hot spots
  geom_curve(data=arrows[2,],
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.3, "cm")),
    curvature = -0.3,
    color='#e6a3d0'
  ) +
  annotate("text", x = 114, y = 41, color='grey30',
           label = "The hottest locations\nhave low tree canopy", 
           fontface='bold', hjust = 1, vjust=1) +
  scale_x_continuous(limits = c(75, 115)) +
  scale_y_continuous(labels=c('', 25, 50, 75, 100), minor_breaks=NULL) +
  scale_color_identity(guide='none') +
  labs(x = "Temperature °F", y = "Tree canopy %",
       title='Heat islands are in areas of low tree canopy',
       subtitle='Each point represents a location in Northampton',
       caption='Temperature: Landsat 9 | Canopy: www.mrlc.gov | Analysis: Kent S Johnson') +
  theme_minimal() +
  theme(plot.title=element_text(face='bold'),
        axis.title=element_text(face='bold'))


patchwork3 = p6 + 
  inset_element(legend + theme(axis.title=element_text(size=rel(0.7))), 
                left = 0, bottom = 0.07, right = 0.2, top = 0.27,
                align_to='full')
