#===============================================================================#
####      EXTRACT ORGANIC TOPSOILS OF EUROPE - OCTOP DATA TO GB 1km GRID     ####
#===============================================================================#

# Samantha Franks, Nov 2014

# soil data (organic carbon content in topsoil) used is found here: http://eusoils.jrc.ec.europa.eu/esdb_archive/octop/octop_data.html
# Jones, R.J.A., Hiederer, R., Rusco, E., Loveland, P.J. and Montanarella, L. (2004). The map of organic carbon in topsoils in Europe, Version 1.2, September 2003: Explanation of Special Publication Ispra 2004 No.72 (S.P.I.04.72). European Soil Bureau Research Report No.17, EUR 21209 EN, 26pp. and 1 map in ISO B1 format. Office for Official Publications of the European Communities, Luxembourg.

setwd(soilwd)
# load the soil map raster: http://eusoils.jrc.ec.europa.eu/wyz_856/_07_oct/octop_insp_directory.zip
soilmap <- raster("octop_insp")
# set the projection of the soilmap (correct CRS can be seen when raster is loaded in ArcGIS)
proj4string(soilmap) <- CRS("+init=epsg:3035")

# load the GB 1km land grid
GB1kmlandgrid <- readOGR(paste(parentwd, "/GIS/GB/NationalGrids/GB", sep=""), "GB001kmclip2land_corrected")

# get centre points of all GB 1km grid squares, transform to vertical rather than horizontal matrix using t()
# convert to shapefile and write to GB National Grid folder in GIS directory
centrept1km <- t(sapply(slot(GB1kmlandgrid, "polygons"), function(x) slot(x, "labpt")))
GB1kmgridcentres <- data.frame(GB1kmlandgrid$ONEKMREF, centrept1km)
colnames(GB1kmgridcentres) <- c("gridref","easting","northing")
coordinates(GB1kmgridcentres) <- c("easting","northing")
proj4string(GB1kmgridcentres) <- GB1kmlandgrid@proj4string
# writeOGR(GB1kmgridcentres, dsn=(paste(parentwd, "/GIS/GB/NationalGrids/GB", sep="")), layer="GB001kmgrid_centrepoints", driver="ESRI Shapefile")

# transform GB1kmgridcentres to same projection as soilmap (epsg3035 LAEA)
GB1kmgridcentres.3035 <- spTransform(GB1kmgridcentres, CRS("+init=epsg:3035"))

# extract soilmap values
soilGB1km <- extract(soilmap, GB1kmgridcentres.3035)

# merge with grid square data
soilGB1km.final <- data.frame(orgC.percent=soilGB1km, gridref=GB1kmgridcentres.3035$gridref)

# output soil data with grid refs to csv file
setwd(paste(parentwd, "/GIS/land cover/soil/", sep=""))
write.table(soilGB1km.final, file="percent_organic_carbon_topsoil_1kmGB.csv", row.names=FALSE, col.names=TRUE, sep=",")