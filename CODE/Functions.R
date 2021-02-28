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

#Function that takes DEMs and a mask and gives output
get.DEM = function (TLS, LiDAR, UAS, mask){
  for (i in 1:length(TLS)) {
    if (!mask[i]) {
      TLS[i] = NA
      LiDAR[i] = NA
      UAS[i] = NA
    }
  }
  ret = list(TLS, LiDAR, UAS)
  names(ret) = c("TLS", "LiDAR", "UAS")
  return(ret)
}

#Function that gives comparisons based on DEMs
compare.DEM = function(dem.base, dem.comp, methods = "[UNDEFINED]", region = "[UNDEFINED]") {
  dem.comp = dem.base - dem.comp
  dem.bias = mean(as.vector(dem.comp), na.rm=T)
  dem.var = var(as.vector(dem.comp), na.rm=T)
  dem.min = min(as.vector(dem.comp), na.rm=T)
  dem.max = max(as.vector(dem.comp), na.rm=T)
  dem.mse = dem.bias ^ 2 + dem.var
  
  cat("\nComparison of", methods, "over", region)
  cat("\nBias:", dem.bias)
  cat("\nVariance:", dem.var)
  cat("\nMinimum:", dem.min)
  cat("\nMaximum:", dem.max)
  cat("\nMean Square Error:", dem.mse)
}
