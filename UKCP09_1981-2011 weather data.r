#UKCP09 WEATHER DATA EXTRACTION
#KEP 25/09/2013 13:51:03
##########################################################################################################################################

rm(list=ls(all=TRUE))
objects()
search()


getwd()



#CODE TO MERGE UKCP09 .CSV FILES INTO ONE DATASET
#This code is for years 1981 - 2011



#READ IN WEATHER FILES:---------------------------------------------------------
  wd  <- "C:/Users/katep/Documents/BTO Data/UKCP09_1981-2011"        
  d   <-dir("C:/Users/katep/Documents/BTO Data/UKCP09_1981-2011")
  d   <- d[-match("readme.txt", d)] 
  RAW <- vector("list", length=length(d))

for (i in 1:length(d)){
  RAW[[i]]<-read.csv(paste(wd, d[i], sep="/"),header=T)
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