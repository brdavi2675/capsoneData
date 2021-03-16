# filters DEMs by mask
DEMs = get.DEM(tls, lidar, minimum, shadow.mask)

#Make Line Profile
par(mfrow = c(2, 2))
profile.tls(DEMs$TLS, DEMs$LiDAR, DEMs$UAS, "Shadow")
profile.uas(DEMs$TLS, DEMs$LiDAR, DEMs$UAS, "Shadow")

# Make error profiles
error.tls(DEMs$TLS, DEMs$UAS, "Shadow")
error.uas(DEMs$TLS, DEMs$UAS, "Shadow")



# Examples of how to check differences attributes
compare.DEM(DEMs$TLS, DEMs$UAS, "TLS and UAS for low tide")
compare.DEM(DEMs$LiDAR, DEMs$UAS, "LiDAR and UAS for low tide")

#KS TEST
DEMs.two = get.DEM(tls, lidar, hightide, water.mask)
#ks.test(as.vector(DEMs$TLS - DEMs$UAS),
#        as.vector(DEMs.two$TLS - DEMs.two$UAS),
#        alternative = "two.sided")

#Check attributes in second area
compare.DEM(DEMs.two$TLS, DEMs.two$UAS, "TLS and UAS for high tide")
compare.DEM(DEMs.two$LiDAR, DEMs.two$UAS, "LiDAR and UAS for high tide")
