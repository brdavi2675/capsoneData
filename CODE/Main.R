###############################################################################
######################### PICK REGION AND UAS DEM TYPE ########################
uas = minimum
mask = water.mask
###############################################################################
###############################################################################

# filters DEMs by mask
DEMs = get.DEM(tls, lidar, uas, mask)

#Make Line Profile
par(mfrow = c(2, 2))
profile.tls(DEMs$TLS, DEMs$LiDAR, DEMs$UAS)
profile.uas(DEMs$TLS, DEMs$LiDAR, DEMs$UAS)

# Make error profiles
error.tls(DEMs$TLS, DEMs$UAS)
error.uas(DEMs$TLS, DEMs$UAS)

# Examples of how to check differences attributes
compare.DEM(DEMs$TLS, DEMs$UAS, "TLS and UAS")
compare.DEM(DEMs$LiDAR, DEMs$UAS, "LiDAR and UAS")

#KS TEST
mask.two = clearing.mask
DEMs.two = get.DEM(tls, lidar, uas, mask.two)
ks.test(as.vector(DEMs$TLS - DEMs$UAS),
        as.vector(DEMs.two$TLS - DEMs.two$UAS),
        alternative = "two.sided")

#Check attributes in second area
compare.DEM(DEMs.two$TLS, DEMs.two$UAS, "TLS and UAS")
compare.DEM(DEMs.two$LiDAR, DEMs.two$UAS, "LiDAR and UAS")
