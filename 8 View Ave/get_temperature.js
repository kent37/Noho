// This script uses Google Earth Engine to access and download 
// Landsat temperature data.
// Use it at https://code.earthengine.google.com/

// Read Landsat images that intersect Northampton
var dataset = ee.ImageCollection('LANDSAT/LC09/C02/T1_L2')
    .filterDate('2024-07-01', '2024-07-04')
    .filterBounds(ee.Geometry.Point(-72.63042, 42.32882));  // Intersecting ROI;

// Applies scaling factors
function applyScaleFactors(image) {
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  
  // Convert to Kelvin, then Fahrenheit
  var thermalBands = image.select('ST_B.*')
    .multiply(0.00341802)
    .add(149.0)
    .subtract(273.15)
    .multiply(9/5)
    .add(32);
  return image.addBands(opticalBands, null, true)
              .addBands(thermalBands, null, true);
}

dataset = dataset.map(applyScaleFactors);

// Reduce ImageCollection to a single Image
var data_mean = dataset.mean();

// Clip to Noho boundary +.
var noho_bbox = ee.Geometry.BBox(-72.74243,  42.28324, -72.58300,  42.37614);
var data_clip = data_mean.clip(noho_bbox);
print(data_clip)

// Visualization parameters
var viz_params = {
  bands: ['ST_B10'],
  min: 70,
  max: 130,
  palette: ['#a50026','#d73027','#f46d43','#fdae61','#fee08b','#ffffbf','#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837'].reverse()
};

var visualization = data_clip.visualize(viz_params);

// Create a task that you can launch from the Tasks tab.
// This will save the image to Google Drive
Export.image.toDrive({
  image: visualization,
  description: 'Noho_temperature_7_2_2024',
  scale: 30
});

// Visualize
Map.setCenter(-72.6480632,42.3207333,13);

Map.addLayer(data_clip, viz_params, 'Temperature');
