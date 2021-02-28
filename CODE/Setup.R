# install these libraries first
library("raster")
library("rgdal")
library("goftest")

#setting variables to be the names of the file locations
tlstif = "../RASTER/tls.tif"
lidartif = "../RASTER/lidar.tif"
minimumtif = "../RASTER/minimum.tif"
maximumtif = "../RASTER/maximum.tif"
averagetif = "../RASTER/average.tif"
tintif = "../RASTER/tin.tif"
hightidetif = "../RASTER/hightidenew.tif"
metashapertif = "../RASTER/metashaper.tif"

# making rasters out of the file locations and immediately
# turning them into matrices
lidar = as.matrix(raster(lidartif))
tls = as.matrix(raster(tlstif))

minimum = as.matrix(raster(minimumtif))
maximum = as.matrix(raster(maximumtif))
average = as.matrix(raster(averagetif))
tin = as.matrix(raster(tintif))
hightide = as.matrix(raster(hightidetif))
metashaper = as.matrix(raster(metashapertif))

#reading in the shape files
upper = readOGR("../SHAPEFILES", "upper")
lower = readOGR("../SHAPEFILES", "lower")
clearing = readOGR("../SHAPEFILES", "clearing")
light = readOGR("../SHAPEFILES", "light")
shadow = readOGR("../SHAPEFILES", "shadow")
water = readOGR("../SHAPEFILES", "water")

#making a blank raster with the same extent of the ones we are using
shape.raster = raster(nrow = 370, ncol = 623)
extent(shape.raster) = extent(raster(tlstif))
# making masks based on the rasterized shape files after turning
# them into matrices
upper.mask = !is.na(as.matrix(rasterize(upper, shape.raster)))
lower.mask = !is.na(as.matrix(rasterize(lower, shape.raster)))
clearing.mask = !is.na(as.matrix(rasterize(clearing, shape.raster)))
light.mask = !is.na(as.matrix(rasterize(light, shape.raster)))
shadow.mask = !is.na(as.matrix(rasterize(shadow, shape.raster)))
water.mask = !is.na(as.matrix(rasterize(water, shape.raster)))
