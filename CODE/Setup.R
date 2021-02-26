#Function that profiles dem lines based on tls
profile.tls = function(dem.tls, dem.lidar, dem.uas, region = "[UNDEFINED]") {
  line.one = as.vector(dem.tls)
  line.two = as.vector(dem.lidar)
  line.three = as.vector(dem.uas)
  placement = order(line.one, na.last=NA)
  indices = 1:length(placement)
  line.one = line.one[placement]
  line.two = line.two[placement]
  line.three = line.three[placement]
  plot(indices, line.one, type="l",col="black",
       main = paste("Profiles of Relative DEMs for areas in", region),
       xlab = "Index of TLS Raster Cells by Height",
       ylab = "Height Values (in meters)",
       ylim = c(0, 3), lwd = 3)
  
  size = length(placement)
  filter = seq(1, size, floor(size / 800))
  lines(indices[filter], line.two[filter],
        type="p",col="red", pch = 19, cex = 0.5)
  lines(indices[filter], line.three[filter],
        type="p",col="blue", pch = 19, cex = 0.5)
  legend(x = "topleft",
         legend = c("TLS Profile", "LiDAR Profile", "UAS Profile"),
         col = c("black", "red", "blue"),
         lty = c(1, NA, NA), pch = c(NA, 19, 19),
         lwd = c(3, NA, NA))
}
#Function that profiles dem lines based on drone
profile.uas = function(dem.tls, dem.lidar, dem.uas, region = "[UNDEFINED]") {
  line.one = as.vector(dem.uas)
  line.two = as.vector(dem.lidar)
  line.three = as.vector(dem.tls)
  placement = order(line.one, na.last=NA)
  indices = 1:length(placement)
  line.one = line.one[placement]
  line.two = line.two[placement]
  line.three = line.three[placement]
  plot(indices, line.one, type="l",col="blue",
       main = paste("Profiles of Relative DEMs for areas in", region),
       xlab = "Index of UAS Raster Cells by Height",
       ylab = "Height Values (in meters)",
       ylim = c(0, 3), lwd = 3)
  
  size = length(placement)
  filter = seq(1, size, floor(size / 800))
  lines(indices[filter], line.two[filter],
        type="p",col="red", pch = 19, cex = 0.5)
  lines(indices[filter], line.three[filter],
        type="p",col="black", pch = 19, cex = 0.5)
  legend(x = "topleft",
         legend = c("UAS Profile", "LiDAR Profile", "TLS Profile"),
         col = c("blue", "red", "black"),
         lty = c(1, NA, NA), pch = c(NA, 19, 19),
         lwd = c(3, NA, NA))
}

#Function that maps residuals for comparisons based on tls height index
error.tls = function(dem.tls, dem.uas, region = "[UNDEFINED]") {
  resid = as.vector(dem.tls) - as.vector(dem.uas)
  placement = order(as.vector(dem.tls), na.last = NA)
  resid = resid[placement]
  size = length(placement)
  indices = 1:size
  filter = seq(1, size, floor(size / 800))
  plot(indices[filter], resid[filter], type="p",col="black",
       main = paste("Differences in TLS vs UAS over", region),
       xlab = "Index of TLS Raster Cells by Height",
       ylab = "Height Differences (in meters)",
       ylim = c(-2, 2))
}
#Function that maps residuals for comparisons based on tls height index
error.uas = function(dem.tls, dem.uas, region = "[UNDEFINED]") {
  resid = as.vector(dem.tls) - as.vector(dem.uas)
  placement = order(as.vector(dem.uas), na.last = NA)
  resid = resid[placement]
  size = length(placement)
  indices = 1:size
  filter = seq(1, size, floor(size / 800))
  plot(indices[filter], resid[filter], type="p",col="black",
       main = paste("Differences in TLS vs UAS over", region),
       xlab = "Index of UAS Raster Cells by Height",
       ylab = "Height Differences (in meters)",
       ylim = c(-2, 2))
}

# install these libraries first
library("raster")
library("rgdal")
library("goftest")

#setting variables to be the names of the file locations
tlstif =
  "C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/RASTER/tls.tif"
lidartif =
  "C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/RASTER/lidar.tif"
minimumtif =
  "C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/RASTER/minimum.tif"
maximumtif =
  "C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/RASTER/maximum.tif"
averagetif =
  "C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/RASTER/average.tif"
tintif =
  "C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/RASTER/tin.tif"
hightidetif =
  "C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/RASTER/hightidenew.tif"

# making rasters out of the file locations and immediately
# turning them into matrices
lidar.raw = as.matrix(raster(lidartif))
tls.raw = as.matrix(raster(tlstif))

minimum = as.matrix(raster(minimumtif))
maximum = as.matrix(raster(maximumtif))
average = as.matrix(raster(averagetif))
tin = as.matrix(raster(tintif))
hightide = as.matrix(raster(hightidetif))

#reading in the shape files
upper =
  readOGR("C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/SHAPEFILES",
          "upper")
lower =
  readOGR("C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/SHAPEFILES",
          "lower")
clearing =
  readOGR("C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/SHAPEFILES",
          "clearing")
light =
  readOGR("C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/SHAPEFILES",
          "light")
shadow =
  readOGR("C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/SHAPEFILES",
          "shadow")
water =
  readOGR("C:/Users/Hcraw/OneDrive/Documents/ArcGIS/Capstone/SHAPEFILES",
          "water")
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
