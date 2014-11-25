####################################################################
#
#   LAND COVER, ELEVATION & CLIMATE SUMMARY TO BRITISH NATIONAL GRID
#
####################################################################

# 17 Nov 2014

# land cover (LCM2000 and LCM2007) and elevation data extraction and summarization by Samantha Franks
# UKCP09 climate data extraction and summarization by Kate Plummer

library(raster)
library(sp)
library(rgeos)
library(rgdal)

####====   SET WORKING DIRECTORIES   ====####

Mac <- FALSE

if(.Platform$OS =='windows') cluster <- FALSE
if (Mac) cluster <- FALSE
if(.Platform$OS=='unix' & !Mac) cluster <- TRUE

if (cluster) {
  Args <- commandArgs()[3] # pulls arguments from cluster job script, where argument string is SSVyyyyYYYY: SS=2 letter species code, V=visit (E=early, L=late, M=maximum), yy=minimum year (2 digits), YY=final year (2 digits)
  
#   species <- substr(Args,1,2)  # Curlew=CU, Carrion Crow=C., Lesser black-backed Gull=LB
#   Visit <- substr(Args,3,3) # E=early, L=late, M=maximum
#   min.year <- as.numeric(substr(Args,4,7)) # minimum year (4 digits)
#   max.year <- as.numeric(substr(Args,8,11)) # maximum, final year (4 digits)
  
}

if (!cluster) {

}


#### SET DIRECTORY PATHS
# # Wales HPC cluster
# if (cluster) parentwd <- c("/home/samantha.franks/")

if (cluster) parentwd <- c("/users1/samf") # BTO cluster
if (!cluster) {
  if (!Mac) parentwd <- c("C:/Users/samf/Documents")
  if (Mac) parentwd <- c("/Volumes/SAM250GB/BTO PC Documents")
}

generic.scriptswd <- paste(parentwd, "/Git/generic scripts", sep="")
landcoverwd <- paste(parentwd, "/GIS/land cover", sep="")
climatewd <- paste(parentwd, "/GIS/climate", sep="")
elevationwd <- paste(parentwd, "/GIS/elevation", sep="")
soilwd <- paste(parentwd, "/GIS/land cover/soil/octop_insp_directory", sep="")
# outputwd <- paste(parentwd, "/curlew_change/output", sep="")
# workspacewd <- paste(parentwd, "/curlew_change/workspaces", sep="")

#set paths to UNIXarchive for itedata
if (!cluster) unix.archive <- "\\\\btodomain/FILES/UNIXArchive/itedata/"
if (cluster) unix.archive <- "/archive/itedata/"

#########################################################################################################

#=========================================================================#
####             LAND COVER - using LCM2000 and LCM2007 (GB)           ####
#=========================================================================#

# land cover data only for Great Britain
# land cover datasets on UNIXArchive/itedata are summarized for LCM2000 and LCM2007 subclass habitat categories at the 1km scale
# code summarizes LCM2000 and LCM2007 data to the same set of aggregate classes so they are comparable
# LCM2007 habitat proportions per 1km square do not sum to 1: missing proportion is coded to ocean (see Simon G's)

### read in lcm2000 and lcm2007 data for GB (ignore Northern Ireland for now)
setwd(unix.archive)
ite2000gb <- read.table("itemap2000/cs2000map_gb.txt", sep="", header=FALSE)
# ite2000.ni <- read.table("itemap2000/cs2000map_ni.txt", sep="", header=FALSE)

ite2007gb <- read.table("itemap2007/lcm2007gb1km.csv", sep=",", header=TRUE)

### give names to ite2000 columns
namesite2000 <- c(names(ite2007gb), paste("type", 24:27, sep=""))
names(ite2000gb) <- namesite2000

### convert ite2000 eastings/northings to correct numbers
ite2000gb$easting <- ite2000gb$easting * 1000
ite2000gb$northing <- ite2000gb$northing * 1000

### identify missing 1km squares that are in one dataset but not the other
# mostly coastal areas (e.g. bits of mostly ocean off the Western Isles)
# subset datasets to include 1km squares common to both datasets
x <- which(!ite2000gb$gridref %in% ite2007gb$gridref)
y <- which(!ite2007gb$gridref %in% ite2000gb$gridref)

notin07 <- ite2000gb[x,]
notin07 <- droplevels(notin07)
notin00 <- ite2007gb[y,]
notin00 <- droplevels(notin00)

ite2000gb <- subset(ite2000gb, ite2000gb$gridref %in% ite2007gb$gridref)
ite2000gb <- droplevels(ite2000gb)
ite2007gb <- subset(ite2007gb, ite2007gb$gridref %in% ite2000gb$gridref)
ite2007gb <- droplevels(ite2007gb)

### aggregate habitats
habdefs07 <- read.csv(paste(landcoverwd, "/itedata/ite2007_definitions.csv", sep=""), header=TRUE)
habs <- levels(habdefs07$aggregate.definition)

habdefs00 <- read.csv(paste(landcoverwd, "/itedata/ite2000_definitions.csv", sep=""), header=TRUE)

# create aggregate habitat placeholders
broadleaf.woodland<-coniferous.woodland<-arable<-improved.grassland<-seminatural.grassland<-mountain.heath.bog<-saltwater<-freshwater<-coastal<-urban.suburban <- numeric()

# sum ite2000 aggregate habitats
attach(ite2000gb)
broadleaf.woodland <- type12
coniferous.woodland <- type13
arable <- type21 + type22 + type23
improved.grassland <- type14
seminatural.grassland <- type15 + type16 + type17 + type18 + type19 + type20
mountain.heath.bog <- type08 + type09 + type10 + type11 + type26
saltwater <- type01
freshwater <- type02
coastal <- type03 + type04 + type05 + type06 + type07
urban.suburban <- type24 + type25
detach(ite2000gb)

ite2000gb.2 <- data.frame(subset(ite2000gb, select=c("gridref","easting","northing")), broadleaf.woodland, coniferous.woodland, arable, improved.grassland, seminatural.grassland, mountain.heath.bog, saltwater, freshwater, coastal, urban.suburban)

# sum ite2007 aggregate habitats
attach(ite2007gb)
broadleaf.woodland <- type01
coniferous.woodland <- type02
arable <- type03
improved.grassland <- type04
seminatural.grassland <- type05 + type06 + type07 + type08 + type09
mountain.heath.bog <- type10 + type11 + type12 + type13 + type14
saltwater <- type15
freshwater <- type16
coastal <- type17 + type18 + type19 + type20 + type21
urban.suburban <- type22 + type23
detach(ite2007gb)

ite2007gb.2 <- data.frame(subset(ite2007gb, select=c("gridref","easting","northing")), broadleaf.woodland, coniferous.woodland, arable, improved.grassland, seminatural.grassland, mountain.heath.bog, saltwater, freshwater, coastal, urban.suburban)

### both ite2000 and ite2007 include squares entirely coastal/at sea (do not include any true land at all)
# identify and remove these squares based on the GB 1km land grid shapefile
GB1kmlandgrid <- readOGR(paste(parentwd, "/GIS/GB/NationalGrids/GB", sep=""), "GB001kmclip2land_corrected")
ite2000gb.3 <- subset(ite2000gb.2, ite2000gb.2$gridref %in% GB1kmlandgrid$ONEKMREF)
ite2007gb.3 <- subset(ite2007gb.2, ite2007gb.2$gridref %in% GB1kmlandgrid$ONEKMREF)

### check that habitats sum to ~100, correct amount of saltwater habitat
# ite2000 is ok for saltwater habitat amounts - continue using ite2000gb.3
# ite2007 often does not include saltwater habitat
habsum <- round(with(ite2000gb.3, broadleaf.woodland + coniferous.woodland + arable + improved.grassland + seminatural.grassland + mountain.heath.bog + saltwater + freshwater + coastal + urban.suburban))
ite2000gb.3 <- data.frame(ite2000gb.3, habsum)

habsum <- round(with(ite2007gb.3, broadleaf.woodland + coniferous.woodland + arable + improved.grassland + seminatural.grassland + mountain.heath.bog + saltwater + freshwater + coastal + urban.suburban))
ite2007gb.3 <- data.frame(ite2007gb.3, habsum)

# round down to 100 for squares where habsum > 100
ite2007gb.3$habsum2 <- ifelse(ite2007gb.3$habsum > 100, 100, ite2007gb.3$habsum)

### correct saltwater habitats for coastal squares only, ite2007 only, where habsum2 is less than 100
lessthan100 <- subset(ite2007gb.3, habsum2 < 100)

# calculate proportion of square that should be saltwater (based on sum of other habitats)
# some squares will be incorrectly identified as having saltwater because of rounding errors for other habitats
# use GIS layer (lessthan1km) of coastal squares (less than 1km) to identify coastal squares with saltwater habitat
# this layer will not necessarily identify coastal squares properly e.g. some "inland" squares will actually be in an estuary
# set saltwater habitat amount in non-coastal squares to 0
lessthan100$old.saltwater <- lessthan100$saltwater
lessthan100$new.saltwater <- lessthan100$old.saltwater + 100-lessthan100$habsum2

# export GIS map of squares with summed habitats less than 100 to check if any are truly inland squares
lessthan100GIS <- subset(GB1kmlandgrid, GB1kmlandgrid$ONEKMREF %in% lessthan100$gridref)

# identify squares with a small saltwater component which has likely been added due to rounding errors in the other habitats
rm.smallsaltwater <- lessthan100[-which(lessthan100$new.saltwater > 0 & lessthan100$new.saltwater < 3),]

# of the squares with a small saltwater component which have been removed, some may actually be truly coastal, just with a tiny bit of saltwater
# this is likely to be the case if they have coastal habitat > 0
# add these squares back onto rm.smallsaltwater
smallsaltwater.withcoastal <- lessthan100[lessthan100$new.saltwater > 0 & lessthan100$new.saltwater < 3 & lessthan100$coastal > 0,]
rm.smallsaltwater <- rbind(rm.smallsaltwater, smallsaltwater.withcoastal)

# create a GIS layer of the squares which are most likely to truly be missing a saltwater component to visually check they are accurate
# this isn't a foolproof method for adding saltwater habitat correctly to the squares likely to be missing it
# a visual check in GIS reveals that there are some squares (e.g. in the Cambrian mountains in Wales SJ1465) which have really been incorrectly identified as needing a saltwater habitat component because the sum of the other habitats is way off 100
# these squares will need to be identified by hand and removed as best as possible
rm.smallsaltwater.GIS <- subset(GB1kmlandgrid, GB1kmlandgrid$ONEKMREF %in% rm.smallsaltwater$gridref)
# writeOGR(rm.smallsaltwater.GIS, dsn=paste(landcoverwd, "/itedata", sep=""), layer="GB001kmgrid_lessthan100byalot_ite2007summedhabitats_v3", driver="ESRI Shapefile")

# squares identified in ArcGIS visually by hand as being truly inland and incorrectly assigned saltwater habitat
inland.nosaltwater <- read.csv(paste(landcoverwd, "/itedata/ite2007_inlandsquares_nosaltwater.csv", sep=""), header=FALSE)
inland.nosaltwater <- as.character(inland.nosaltwater$V1)

# remove truly inland squares from rm.smallsaltwater - left with a dataset that is as much as possible the squares where summed habitat was less than 100 and which are most likely to need saltwater habitat added
true.saltwatersquares <- rm.smallsaltwater[-which(rm.smallsaltwater$gridref %in% inland.nosaltwater),]

# for true saltwater squares, replace saltwater values with the new.saltwater values
# remove true saltwater squares from original dataset (with incorrect saltwater values), then add the true saltwater squares with the correct values back
true.saltwatersquares$saltwater <- true.saltwatersquares$new.saltwater
ite2007gb.4 <- ite2007gb.3[-which(ite2007gb.3$gridref %in% true.saltwatersquares$gridref), ]
ite2007gb.4 <- rbind(ite2007gb.4, true.saltwatersquares[,-which(names(true.saltwatersquares) %in% c("old.saltwater","new.saltwater"))])

# check habitat sums for habsum3 (includes new saltwater habitat) - now min(habsum3) is 93, which is close enough to 100 to be happy!
habsum3 <- round(with(ite2007gb.4, broadleaf.woodland + coniferous.woodland + arable + improved.grassland + seminatural.grassland + mountain.heath.bog + saltwater + freshwater + coastal + urban.suburban))
ite2007gb.5 <- data.frame(ite2007gb.4, habsum3)

# create final ite datasets
ite2000.final <- ite2000gb.3[, -grep("habsum", names(ite2000gb.3))]
ite2000.final <- ite2000.final[order(-ite2000.final$northing, ite2000.final$easting),]

ite2007.final <- ite2007gb.5[, -grep("habsum", names(ite2007gb.5))]
ite2007.final <- ite2007.final[order(-ite2007.final$northing, ite2007.final$easting),]

setwd(paste(landcoverwd, "/itedata", sep=""))
write.table(ite2000.final, file="ite2000_aggregatehabitats_landonly.csv", row.names=FALSE, col.names=TRUE, sep=",")
write.table(ite2007.final, file="ite2007_aggregatehabitats_landonly.csv", row.names=FALSE, col.names=TRUE, sep=",")

#===========================================================#
####      SOIL - ORGANIC TOPSOILS OF EUROPE - OCTOP      ####
#===========================================================#

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


#########################################################################################################

#=============================================#
####              CLIMATE               ####
#=============================================#

# original code written by Kate Plummer 25/09/2013
# UKCP09 WEATHER DATA EXTRACTION

#CODE TO MERGE UKCP09 .CSV FILES INTO ONE DATASET
#This code is for years 1981 - 2011

#READ IN WEATHER FILES:---------------------------------------------------------
d   <- dir(climatewd)
d   <- d[-match("readme.txt", d)] 
RAW <- vector("list", length=length(d))

for (i in 1:length(d)){
  RAW[[i]]<-read.csv(paste(climatewd, d[i], sep="/"),header=T)
}
names(RAW) <- as.character(strsplit(d, ".csv"))

summary(RAW)
summary(RAW[["AirFrost_1981-2000"]])

rm(d, i, wd)
#-------------------------------------------------------------------------------



#JOIN WEATHER DATA YEAR BLOCKS:-------------------------------------------------
#Create new list to store weather data:
weather_var       <- as.character(lapply(strsplit(names(RAW), "_"), "[", 1))
weather           <- weather_var[!duplicated(weather_var)]
RAW_joined        <- vector("list", length=length(weather))
names(RAW_joined) <- weather

#Loop multi-merge:
library(reshape)
for (i in 1:length(weather)){
  RAW_joined[[i]] <- merge_recurse(RAW[weather_var==weather[i]], by= c("Easting", "Northing")) 
}

summary(RAW_joined)
rm(RAW, i, weather_var)   
gc()                
#-------------------------------------------------------------------------------



#SUBSET TO ONLY YEARS AND MONTH BEING ANALYSED:---------------------------------
#Which years/months to keep:
#  load("Blackcaps/BLACA winter dataset.RData")  
#  keep <- paste(WINsubs$month, WINsubs$year, sep=".")
#  keep <- keep[!duplicated(keep)]
#  keep <- c("Easting",  "Northing", keep)

#Loop through selecting out these columns:
#  RAW_sub        <- vector("list", length=length(weather))
#  names(RAW_sub) <- weather
#  for (i in 1:length(weather)){
#      To_keep      <- grepl(paste(keep,collapse="|"), colnames(RAW_joined[[i]]))
#      RAW_sub[[i]] <- subset(RAW_joined[[i]], select = To_keep)
#      }

#summary(RAW_sub) 
#-------------------------------------------------------------------------------     



#RESHAPE DATA:------------------------------------------------------------------
#Convert to long and thin:
library(reshape2)
UKCP09       <- vector("list", length=length(weather))
names(UKCP09)<- weather
for (i in 1:length(weather)){
  UKCP09[[i]] <- melt(RAW_joined[[i]], id=c("Easting", "Northing"))
  names(UKCP09[[i]])[names(UKCP09[[i]])=="variable"] <- "Date"
  names(UKCP09[[i]])[names(UKCP09[[i]])=="value"]    <- weather[i]
  gc()
}

#Check:
summary(UKCP09)

SUM       <- vector("list", length=length(weather))
names(SUM)<- weather
for (i in 1:length(weather)){      
  SUM[[i]] <- summary(UKCP09[[i]])
}
SUM

rm(i, RAW_joined, SUM, weather)
gc()
#-------------------------------------------------------------------------------   



#MERGE ALL WEATHER DATA INTO ONE DATASET:---------------------------------------
#Needs to be done in two parts due to space requirements
UKCP09_1 <- merge_recurse(UKCP09[1:8], by= c("Easting", "Northing", "Date"))


UKCP09_2 <- UKCP09[9:16]
rm(UKCP09) 
gc()  
UKCP09_2 <- merge_recurse(UKCP09_2, by= c("Easting", "Northing", "Date")) 
gc() 


UKCP09   <- merge(UKCP09_1, UKCP09_2, by= c("Easting", "Northing", "Date")) 

rm(UKCP09_1, UKCP09_2)
gc()

summary(UKCP09)

#Add year and month:
UKCP09$Year  <- as.numeric(substr(UKCP09$Date, 5,8)) 
UKCP09$Month <- factor(substr(UKCP09$Date, 1,3), levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))      

UKCP09       <- UKCP09[, -match("Date", names(UKCP09))] #remove column by name from a dataframe
UKCP09       <- UKCP09[,c(1:2,19:20,3:18)]              #restructure

summary(UKCP09)
#-------------------------------------------------------------------------------   



#Missing data:
summary(droplevels(subset(UKCP09, is.na(AirFrost))))    #one square 81-00 (E 397500, N 1137500)
summary(droplevels(subset(UKCP09, is.na(CloudCover))))  #no data for 2007 onwards
summary(droplevels(subset(UKCP09, is.na(GroundFrost)))) #some missing 81-00 in E 397500, N 1137500  
summary(droplevels(subset(UKCP09, GroundFrost==-9999.000))) #42 records 2007-08
summary(droplevels(subset(UKCP09, is.na(Rainfall))))        #two next door squares missing 81-00
summary(droplevels(subset(UKCP09, is.na(SnowFall))))        #some squares missing for some months 81-00
summary(droplevels(subset(UKCP09, SnowFall==-9999.000)))    #some of the same squares but for 2001-11

#Replace -9999's in weather variables with NAs:
for (i in 5:ncol(UKCP09)){      
  UKCP09[,i][UKCP09[,i]<=-9998] <- NA
}

rm(i)

#########################################################################################################################################
#SAVE UKCP09 WEATHER FILE

ls()
gc()

save.image("GBW/UKCP09_1981-2011.RData")  
#25/09/2013 16:24:34

#########################################################################################################################################