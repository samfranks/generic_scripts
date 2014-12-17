
# 17 Nov 2014

# land cover (LCM2000 and LCM2007) and elevation data extraction and summarization by Samantha Franks
# UKCP09 climate data extraction and summarization by Kate Plummer

library(raster)
library(sp)
library(rgeos)
library(rgdal)
library(reshape)

####====   SET WORKING DIRECTORIES   ====####

Mac <- FALSE

if(.Platform$OS =='windows') cluster <- FALSE
if (Mac) cluster <- FALSE
if(.Platform$OS=='unix' & !Mac) cluster <- TRUE

# if (cluster) {
#   Args <- commandArgs()[3] # pulls arguments from cluster job script, where argument string is SSVyyyyYYYY: SS=2 letter species code, V=visit (E=early, L=late, M=maximum), yy=minimum year (2 digits), YY=final year (2 digits)
#   
#   #   species <- substr(Args,1,2)  # Curlew=CU, Carrion Crow=C., Lesser black-backed Gull=LB
#   #   Visit <- substr(Args,3,3) # E=early, L=late, M=maximum
#   #   min.year <- as.numeric(substr(Args,4,7)) # minimum year (4 digits)
#   #   max.year <- as.numeric(substr(Args,8,11)) # maximum, final year (4 digits)
#   
# }

# if (!cluster) {
#   
# }


#### SET DIRECTORY PATHS
# # Wales HPC cluster
# if (cluster) parentwd <- c("/home/samantha.franks/")

if (cluster) parentwd <- c("/users1/samf") # BTO cluster
if (!cluster) {
  if (!Mac) parentwd <- c("C:/Users/samf/Documents")
  if (Mac) parentwd <- c("/Volumes/SAM250GB/BTO PC Documents")
}

if (!cluster) {
  generic.scriptswd <- paste(parentwd, "/Git/generic scripts", sep="")
  landcoverwd <- paste(parentwd, "/GIS/land cover", sep="")
  climatewd <- paste(parentwd, "/GIS/climate/UKCP09", sep="")
  elevationwd <- paste(parentwd, "/GIS/elevation/CGIAR-STRM elevation data/GB_Ireland", sep="")
  soilwd <- paste(parentwd, "/GIS/land cover/soil/octop_insp_directory", sep="")
  workspacewd <- paste(parentwd, "/curlew_change/workspaces", sep="")
  gridwd <- paste(parentwd, "/GIS/British Isles/NationalGrids/", sep="")
}

if (cluster) {
  elevationwd <- paste(parentwd, "/curlew_change/data/elevation", sep="")
}

#set paths to UNIXarchive for itedata
if (!cluster) unix.archive <- "\\\\btodomain/FILES/UNIXArchive/itedata/"
if (cluster) unix.archive <- "/archive/itedata/"


#=============================================#
####              ELEVATION               ####
#=============================================#

# CGIAR STRM tiles required for GB + Ireland:
# strm_34_02 - far west of Ireland
# strm_35_02 - central Ireland
# strm_36_02 - central England
# strm_37_02 - East Anglia
# strm_35_01 - Western Isles
# strm 36_01 - Scotland
# strm 36_03 - very southern edge of the South Coast Cornwall? 


### extract elevation raster values for 1km GB grid

# load raster and 1km GB shapefile
setwd(elevationwd)
r <- raster("GB_Ireland_CGIAR-STRM_elevation_raster.tif")

if (!cluster) {
  GB1kmlandgrid <- readOGR(paste(parentwd, "/GIS/British Isles/NationalGrids/GB", sep=""), "GB001kmclip2land_corrected")
}

if (cluster) {
  GB1kmlandgrid <- readOGR(elevationwd, "GB001kmclip2land_corrected")
}

# GB10kmgridtext <- read.table(paste(parentwd, "GIS/British Isles/NationalGrids/gridref lookups/GB010kmgid_eastnorth.txt", sep="/"), sep="\t", header=TRUE)
# GB10kmland <- subset(GB10kmgridtext, land=="1")
# 
# test10km <- GB10kmland[300:310,]

# convert projection of GB grid to same as elevation raster
GB1kmlandgrid.proj2 <- spTransform(GB1kmlandgrid, CRS(proj4string(r)))

if (cluster) {
  save.image(paste(parentwd, "workspaces", sep="/"), "elevation extraction 1")
}

# testgrid <- subset(GB1kmlandgrid.proj2, grepl(paste(test10km$gridref, collapse="|"), GB1kmlandgrid.proj2$ONEKMREF))

# extract raster values, averaged over 1km grid square
#######!!!!!!!!! extraction of raster data over GB 1km grid clipped to land has run for nearly 3 days (Dec 5 22:30 to Dec 8 18:00) on BTO PC and is not done! This is not practical, need to find another way to extract data
# extracting a 10km square (at the 1km square level) only took ~ 2 minutes
# may need to run elevation raster extraction as a loop that cycles through all the 10km squares in GB (only ~3000 of them)
######!!!!!!!!! maybe test a 100km square and see how long this takes?
print(Sys.time())
elevation <- extract(r, GB1kmlandgrid.proj2, function(x) mean(x, na.rm=TRUE))
# elevation <- extract(r, testgrid, function(x) mean(x, na.rm=TRUE))
print(Sys.time())

if (cluster) {
  save.image(paste(parentwd, "workspaces", sep="/"), "elevation extraction 2")
}

# add elevation data to 1km grid dataset
elevation2 <- data.frame(GB1kmlandgrid.proj2, elevation)
# elevation2 <- data.frame(testgrid, elevation)
colnames(elevation2) <- c("gridref","land","elevation.m")

# check against Dario's extracted elevations
setwd(landcoverwd)
darioelv <- read.table("ite2000_elevation_county_1km_uk_dariom.csv", sep=",", header=TRUE)

if (cluster) {
  save.image(paste(parentwd, "workspaces", sep="/"), "elevation extraction 3")
}

# # merge 1km grid ref elevation data with eastings/northings
# GB1kmgrid <- read.table(paste(gridwd, "gridref lookups/GB001kmgid_eastnorth.txt", sep="/"), header=TRUE)
# 
# # merge UKCP09 with 1km grid
# elevation.ref <- merge(elevation2, subset(GB1kmgrid, select=c("gridref","easting","northing")), by="gridref")

# write elevation data
setwd(elevationwd)
write.table(elevation2, file="GB001kmgrid_mean_elevation.txt", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")