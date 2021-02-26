###############################################################################
######################### PICK REGION AND UAS DEM TYPE ########################
uas = minimum
mask = clearing.mask * lower.mask
###############################################################################
###############################################################################

# THis isolates the important points by turning any point outside 
# of the shape into an NA so it wont be evaluated.
# PUT THE MASK YOU WANT TO USE IN THE IF STATEMENT
tls = tls.raw
lidar = lidar.raw
for (i in 1:length(tls)) {
  if (!mask[i]) {
    tls[i] = NA
    lidar[i] = NA
    uas[i] = NA
  }
}

#Make Line Profile
par(mfrow = c(2, 2))
profile.tls(tls, lidar, uas)
profile.uas(tls, lidar, uas)

error.tls(tls, uas)
error.uas(tls, uas)

# Examples of how to check differences attributes
tls.comp = tls - uas
tls.bias = mean(as.vector(tls.comp), na.rm=T)
tls.var = var(as.vector(tls.comp), na.rm=T)
tls.min = min(as.vector(tls.comp), na.rm=T)
tls.max = max(as.vector(tls.comp), na.rm=T)
tls.mse = tls.bias ^ 2 + tls.var

lidar.comp = lidar - uas
lidar.bias = mean(as.vector(lidar.comp), na.rm=T)
lidar.var = var(as.vector(lidar.comp), na.rm=T)
lidar.min = min(as.vector(lidar.comp), na.rm=T)
lidar.max = max(as.vector(lidar.comp), na.rm=T)
lidar.mse = lidar.bias ^ 2 + lidar.var

#KS TEST
uas.two = minimum
mask.two = shadow.mask * clearing.mask
tls.two = tls.raw
lidar.two = lidar.raw
for (i in 1:length(tls.two)) {
  if (!mask.two[i]) {
    tls.two = NA
    lidar.two[i] = NA
    uas.two[i] = NA
  }
}
ks.test(as.vector(tls- uas), as.vector(tls.two - uas.two),
        alternative = "two.sided")