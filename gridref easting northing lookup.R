#############################################################################
#
#   CREATE GRIDREF, EASTING, NORTHING REFERENCE FILES FOR GB AND IRELAND
#
#############################################################################

# Samantha Franks
# 26 Nov 2014

library(sp)
library(rgdal)

##########################################
#
#         Great Britain
#
##########################################

setwd("c:/Users/samf/Documents/GIS/British Isles/NationalGrids/")

####============= 1 km grid =============####

# load grid
GB1kmgrid <- readOGR("GB", "GB001kmgrid")

# get eastings and northings for grid square
# coords[3,] gives the SW corner of the square
# alternatively, use coordinates() gives centrepoint of the square
# easting=coordinates[,1]-500, northing=coordinates[,2]-500
xy <- apply(coordinates(GB1kmgrid), 2, function(x) (x-500))

GB1kmgrid.xy <- GB1kmgrid
GB1kmgrid.xy@data <- data.frame(GB1kmgrid@data, xy)
colnames(GB1kmgrid.xy@data) <- c("gridref","land","easting","northing")

# write shapefile
# write text file
writeOGR(GB1kmgrid.xy, dsn="GB", layer="GB001kmgrid_eastnorth", driver="ESRI Shapefile")
write.table(GB1kmgrid.xy@data, file="gridref lookups/GB001kmgid_eastnorth.txt", row.names=FALSE, quote=FALSE, sep="\t")


####============= 10 km grid =============####

# load grid
GB10kmgrid <- readOGR("GB", "GB010kmgrid")

if ("FID_1" %in% names(GB10kmgrid)) {
  GB10kmgrid <- subset(GB10kmgrid, select=c("TENKMREF","LAND"))
}

# get eastings and northings for grid square
# coords[3,] gives the SW corner of the square
# alternatively, use coordinates() gives centrepoint of the square
# easting=coordinates[,1]-500, northing=coordinates[,2]-500
xy <- apply(coordinates(GB10kmgrid), 2, function(x) (x-5000))

GB10kmgrid.xy <- GB10kmgrid
GB10kmgrid.xy@data <- data.frame(GB10kmgrid@data, xy)
colnames(GB10kmgrid.xy@data) <- c("gridref","land","easting","northing")

# write shapefile
# write text file
writeOGR(GB10kmgrid.xy, dsn="GB", layer="GB010kmgrid_eastnorth", driver="ESRI Shapefile")
write.table(GB10kmgrid.xy@data, file="gridref lookups/GB010kmgid_eastnorth.txt", row.names=FALSE, quote=FALSE, sep="\t")


####============= 100 km grid =============####

# load grid
GB100kmgrid <- readOGR("GB", "GB100kmgrid")

if ("FID_1" %in% names(GB100kmgrid)) {
  GB100kmgrid <- subset(GB100kmgrid, select=c("HUNDREF","LAND"))
}

# get eastings and northings for grid square
# coords[3,] gives the SW corner of the square
# alternatively, use coordinates() gives centrepoint of the square
# easting=coordinates[,1]-500, northing=coordinates[,2]-500
xy <- apply(coordinates(GB100kmgrid), 2, function(x) (x-50000))

GB100kmgrid.xy <- GB100kmgrid
GB100kmgrid.xy@data <- data.frame(GB100kmgrid@data, xy)
colnames(GB100kmgrid.xy@data) <- c("gridref","land","easting","northing")

# write shapefile
# write text file
writeOGR(GB100kmgrid.xy, dsn="GB", layer="GB100kmgrid_eastnorth", driver="ESRI Shapefile")
write.table(GB100kmgrid.xy@data, file="gridref lookups/GB100kmgid_eastnorth.txt", row.names=FALSE, quote=FALSE, sep="\t")


####============= centre points of 1km grid squares =============####

# load the GB 1km land grid
GB1kmlandgrid <- readOGR("GB", "GB001kmclip2land_corrected")

# centre points of grid squares
# get centre points of all GB 1km grid squares, transform to vertical rather than horizontal matrix using t()
# convert to shapefile and write to GB National Grid folder in GIS directory
centrept1km <- t(sapply(slot(GB1kmlandgrid, "polygons"), function(x) slot(x, "labpt")))

single <- subset(GB1kmgrid, ONEKMREF=="TM0979")

GB1kmgridcentres <- data.frame(GB1kmlandgrid$ONEKMREF, centrept1km)
colnames(GB1kmgridcentres) <- c("gridref","easting","northing")
coordinates(GB1kmgridcentres) <- c("easting","northing")
proj4string(GB1kmgridcentres) <- GB1kmlandgrid@proj4string
# writeOGR(GB1kmgridcentres, dsn=(paste(parentwd, "/GIS/GB/NationalGrids/GB", sep="")), layer="GB001kmgrid_centrepoints", driver="ESRI Shapefile")


# ##########################################
# #
# #         Ireland - this grid has decimal points for SW corners, so code below needs fixing before use!!!!!!!!!!!!
# #
# ##########################################
# 
# setwd("c:/Users/samf/Documents/GIS/British Isles/NationalGrids/")
# 
# ####============= 1 km grid =============####
# 
# # load grid
# IR1kmgrid <- readOGR("Ireland", "Ireland001kmgrid")
# 
# if ("FID_1" %in% names(IR1kmgrid)) {
#   IR1kmgrid <- subset(IR1kmgrid, select=c("ONEKMREF","LAND"))
# }
# 
# # get eastings and northings for grid square
# # coords[3,] gives the SW corner of the square
# # alternatively, use coordinates() gives centrepoint of the square
# # easting=coordinates[,1]-500, northing=coordinates[,2]-500
# xy <- apply(coordinates(IR1kmgrid), 2, function(x) (x-500))
# 
# IR1kmgrid.xy <- IR1kmgrid
# IR1kmgrid.xy@data <- data.frame(IR1kmgrid@data, xy)
# colnames(IR1kmgrid.xy@data) <- c("gridref","land","easting","northing")
# 
# # write shapefile
# # write text file
# writeOGR(IR1kmgrid.xy, dsn="Ireland", layer="Ireland001kmgrid_eastnorth", driver="ESRI Shapefile")
# write.table(IR1kmgrid.xy@data, file="gridref lookups/Ireland001kmgid_eastnorth.txt", row.names=FALSE, quote=FALSE, sep="\t")
# 
# 
# ####============= 10 km grid =============####
# 
# # load grid
# IR10kmgrid <- readOGR("Ireland", "Ireland010kmgrid")
# 
# if ("FID_1" %in% names(IR10kmgrid)) {
#   IR10kmgrid <- subset(IR10kmgrid, select=c("TENKMREF","LAND"))
# }
# 
# # get eastings and northings for grid square
# # coords[3,] gives the SW corner of the square
# # alternatively, use coordinates() gives centrepoint of the square
# # easting=coordinates[,1]-500, northing=coordinates[,2]-500
# xy <- apply(coordinates(IR10kmgrid), 2, function(x) (x-5000))
# 
# IR10kmgrid.xy <- IR10kmgrid
# IR10kmgrid.xy@data <- data.frame(IR10kmgrid@data, xy)
# colnames(IR10kmgrid.xy@data) <- c("gridref","land","easting","northing")
# 
# # write shapefile
# # write text file
# writeOGR(IR10kmgrid.xy, dsn="Ireland", layer="Ireland010kmgrid_eastnorth", driver="ESRI Shapefile")
# write.table(IR10kmgrid.xy@data, file="gridref lookups/Ireland010kmgid_eastnorth.txt", row.names=FALSE, quote=FALSE, sep="\t")
# 
# 
# ####============= 100 km grid =============####
# 
# # load grid
# IR100kmgrid <- readOGR("Ireland", "Ireland100kmgrid")
# 
# if ("FID_1" %in% names(IR100kmgrid)) {
#   IR100kmgrid <- subset(IR100kmgrid, select=c("HUNDREF","LAND"))
# }
# 
# # get eastings and northings for grid square
# # coords[3,] gives the SW corner of the square
# # alternatively, use coordinates() gives centrepoint of the square
# # easting=coordinates[,1]-500, northing=coordinates[,2]-500
# xy <- apply(coordinates(IR100kmgrid), 2, function(x) (x-50000))
# 
# IR100kmgrid.xy <- IR100kmgrid
# IR100kmgrid.xy@data <- data.frame(IR100kmgrid@data, xy)
# colnames(IR100kmgrid.xy@data) <- c("gridref","land","easting","northing")
# 
# # write shapefile
# # write text file
# writeOGR(IR100kmgrid.xy, dsn="Ireland", layer="Ireland100kmgrid_eastnorth", driver="ESRI Shapefile")
# write.table(IR100kmgrid.xy@data, file="gridref lookups/Ireland100kmgid_eastnorth.txt", row.names=FALSE, quote=FALSE, sep="\t")
# 
