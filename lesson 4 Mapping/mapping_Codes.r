library(maps)
library(mapdata)
library(marmap)
library(raster)
library(lattice)
library(rgdal)

#Early Forays: Basic mapping of the coastline with some bathymetry data
US_Coast<-getNOAA.bathy(lon1 = -129, lon2 = -116.5, lat1 = 51, lat2 = 29, resolution = 4)
Individuals<-read.csv("Ssimplex_Metadata.csv", header=TRUE, row.names=1)
#tiff("MapFigure_AllInd.tiff", width=4.2, height=7.6, units='in', res=300)
US_Coast<-getNOAA.bathy(lon1 = -129, lon2 = -116.5, lat1 = 51, lat2 = 29, resolution = 4)
map('world', xlim=c(-128,-117), ylim=c(30,50), fill=TRUE, col="grey", mar=c(9,8,0,0), asp=NA)
box();axis(1);axis(2)
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))
plot(US_Coast, add=TRUE, image=TRUE, land=TRUE, bpal = list(c(0, max(US_Coast), greys), c(min(US_Coast), 0, blues)))
title(xlab="Longitude", ylab="Latitude")
map('world', add=TRUE, xlim=c(-128,-117), ylim=c(30,50), fill=TRUE, col="grey")
map('state',add=TRUE, xlim=c(-128,-117), ylim=c(30,50), fill=TRUE, col="grey")
points(Individuals$Longitude[1:6],Individuals$Latitude[1:6], pch = 21, col ="black", bg ="white", cex =1.2)
points(Individuals$Longitude[7:13],Individuals$Latitude[7:13], pch = 24, col ="black", bg ="red", cex =1.0)
points(Individuals$Longitude[14:18],Individuals$Latitude[14:18], pch = 24, col ="black", bg ="darkgoldenrod1", cex =1.0)
points(Individuals$Longitude[25:29],Individuals$Latitude[25:29], pch = 24, col ="black", bg ="green3", cex = 1.0)
#if you used tiff run this: dev.off()  
#it closes the tiff file

#***************************************
# Internet Connection
# Exploring the getNOAA.bathy function
#***************************************


#Primnoa collections in SE Alaska
#what if we just want shading or want the land to look fancier
SE_AK<-getNOAA.bathy(lon1=-140, lon2=-131, lat1=54, lat2=59, resolution=1)
Figure1Ave<-read.table("MapAveLatLong.txt",sep="\t",header=TRUE)
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))
plot(SE_AK, asp=NA)
plot(SE_AK, image=TRUE, land=TRUE, deepest.isobath=0, shallowest.isobath=0, step=0, asp=NA, bpal=list(c(min(SE_AK),0,blues),c(0,max(SE_AK),greys)))
plot(SE_AK, image=TRUE, land=TRUE, deepest.isobath=-3000, shallowest.isobath=0, step=100, asp=NA, bpal=list(c(min(SE_AK),0,blues),c(0,max(SE_AK),greys)))
plot(SE_AK, image=TRUE, land=TRUE, deepest.isobath=-3000, shallowest.isobath=0, step=300, asp=NA, bpal=list(c(min(SE_AK),0,blues),c(0,max(SE_AK),greys)))
#NOW add our populations...
points(Figure1Ave$Longitude[1],Figure1Ave$Latitude[1], pch=23, col="black", bg="red", cex=2)
points(Figure1Ave$Longitude[2],Figure1Ave$Latitude[2], pch=23, col="black", bg="blue", cex=2)
points(Figure1Ave$Longitude[3],Figure1Ave$Latitude[3], pch=23, col="black", bg="green", cex=2)
points(Figure1Ave$Longitude[4],Figure1Ave$Latitude[4], pch=23, col="black", bg="yellow", cex=2)


#looking at level of detail in seamounts
#resolution of four, great for coastwide, less good when we need to look at a detailed feature
wholeRegion<-getNOAA.bathy(lon1 = -132, lon2 = -129,lat1 = 49.70, lat2 = 51.55, resolution=4)
plot(wholeRegion, image=TRUE, bpal=topo.colors(500), asp=NA)
plot(wholeRegion,image=TRUE, bpal=topo.colors(100), xlim=c(-131.05,-130.75), ylim=c(50.65, 50.85))
rm(wholeRegion)
#lets increase the resolution
wholeRegion<-getNOAA.bathy(lon1 = -132, lon2 = -129,lat1 = 49.70, lat2 = 51.55, resolution=1)
plot(wholeRegion, image=TRUE, bpal=topo.colors(500), asp=NA)
#zoom in to Dellwood Seamount
plot(wholeRegion,image=TRUE, bpal=topo.colors(100), xlim=c(-131.05,-130.75), ylim=c(50.65, 50.85))
#zoom in to dive site
plot(wholeRegion, image=TRUE, bpal=topo.colors(100), xlim=c(-130.925,-130.890), ylim=c(50.70, 50.74))

#But, we have multibeam batymetry data from Nautilus that we can look at for more detail! NOTE, be sure you've got the right file
Dellwood30<-raster("NA097_20180707_DellwoodSeamount_30m_srf.tif")
#This file is a GeoTiff, a tiff image with embeded information, in this case bathymetry, there are also backscatter files
Test<-as.bathy(Dellwood30)
plot.bathy(Test)
#note the odd numbers, this is because in raster files the lat/long is recoded to a universal code...for our figure we can change that 
#reprojecting the raster
Dellwood30
DellwoodLatLong<-projectRaster(Dellwood30,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
Test<-as.bathy(DellwoodLatLong)
plot.bathy(Test)
plot.bathy(Test, image=TRUE, bpal = topo.colors(100))
#Now we can add it to the whole region for comparison and see what changes
plot(wholeRegion, image=TRUE, bpal=topo.colors(500), asp=NA)
plot(Test, image=TRUE, add=TRUE, bpal = topo.colors(100))
plot(wholeRegion,image=TRUE, bpal=topo.colors(100), xlim=c(-131.05,-130.75), ylim=c(50.65, 50.85))
plot(Test, image=TRUE, add=TRUE, bpal = topo.colors(100))
#Zoomed into dive site, before and after adding new bathy data
plot(wholeRegion, image=TRUE, bpal=topo.colors(100), xlim=c(-130.925,-130.890), ylim=c(50.70, 50.74))
plot(Test, image=TRUE, bpal=topo.colors(100), xlim=c(-130.925,-130.890), ylim=c(50.70, 50.74))
#Next step is learning/figuring out how to add the dive track...

#Other fun stuff: Least Cost Pathway Analysis
data(hawaii, hawaii.sites)
sites<-hawaii.sites[-c(1,4),]
rownames(sites)<- 1:4
#now we compute a transistion matrix between sites, given conditions
trans1<-trans.mat(hawaii)
trans2<-trans.mat(hawaii, min.depth = -200)
#now compute least cost distance
out1<-lc.dist(trans1, sites, res = "path")
out2<-lc.dist(trans2, sites, res = "path")
#we can plot, the orange is constrained only by land, black by 200m, this can be used for isolation by distance in the deep.
plot(hawaii,xlim = c(-161, -154), ylim = c(18, 23),
     deep = c(-5000, -200, 0), shallow = c(-200, 0, 0),
     col = c("grey", "blue", "black"), step = c(1000, 200, 1),
     lty = c(1, 1, 1), lwd = c(0.6, 0.6, 1.2),
     draw = c(FALSE, FALSE, FALSE))
points(sites, pch = 21, col = "blue", bg = col2alpha("blue", .9),
       cex = 1.2)
text(sites[,1], sites[,2], lab = rownames(sites),
     pos = c(3, 4, 1, 2), col = "blue")
lapply(out1, lines, col = "orange", lwd = 5, lty = 1) -> dummy
lapply(out2, lines, col = "black", lwd = 1, lty = 1) -> dummy

#last fun thing because it might crash my system...wireframe for 3D images
wireframe(unclass(Test),shade=TRUE)

