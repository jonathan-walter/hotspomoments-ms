# Package ID: edi.420.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hypereutrophic lake spatial sensor data during summer bloom, Swan Lake, Iowa, USA 2018.
# Data set creator:  David Ortiz - Iowa State University 
# Data set creator:  Grace Wilkinson - Iowa State University 
# Contact:  David Ortiz -  Iowa State University  - daortiz@iastate.edu
# Contact:  Grace Wilkinson -  Iowa State University  - wilkinso@iastate.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/420/1/5419cbb0b2174c90aae9703acac7d4ff" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Lake",     
                 "Site",     
                 "DOY",     
                 "TimeFraction",     
                 "DOY_TimeFraction",     
                 "Year",     
                 "Latitude",     
                 "Longitude",     
                 "Chlorophyll",     
                 "Phycocyanin",     
                 "pH",     
                 "DissolvedOxygen_Saturation",     
                 "DO_mg",     
                 "Temperature",     
                 "Specific_Conductivity",     
                 "TotalDissolved_Soilds",     
                 "Macrophyte"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Lake)!="factor") dt1$Lake<- as.factor(dt1$Lake)
if (class(dt1$Site)!="factor") dt1$Site<- as.factor(dt1$Site)
if (class(dt1$DOY)=="factor") dt1$DOY <-as.numeric(levels(dt1$DOY))[as.integer(dt1$DOY) ]               
if (class(dt1$DOY)=="character") dt1$DOY <-as.numeric(dt1$DOY)
if (class(dt1$TimeFraction)=="factor") dt1$TimeFraction <-as.numeric(levels(dt1$TimeFraction))[as.integer(dt1$TimeFraction) ]               
if (class(dt1$TimeFraction)=="character") dt1$TimeFraction <-as.numeric(dt1$TimeFraction)
if (class(dt1$DOY_TimeFraction)=="factor") dt1$DOY_TimeFraction <-as.numeric(levels(dt1$DOY_TimeFraction))[as.integer(dt1$DOY_TimeFraction) ]               
if (class(dt1$DOY_TimeFraction)=="character") dt1$DOY_TimeFraction <-as.numeric(dt1$DOY_TimeFraction)
if (class(dt1$Year)=="factor") dt1$Year <-as.numeric(levels(dt1$Year))[as.integer(dt1$Year) ]               
if (class(dt1$Year)=="character") dt1$Year <-as.numeric(dt1$Year)
if (class(dt1$Latitude)=="factor") dt1$Latitude <-as.numeric(levels(dt1$Latitude))[as.integer(dt1$Latitude) ]               
if (class(dt1$Latitude)=="character") dt1$Latitude <-as.numeric(dt1$Latitude)
if (class(dt1$Longitude)=="factor") dt1$Longitude <-as.numeric(levels(dt1$Longitude))[as.integer(dt1$Longitude) ]               
if (class(dt1$Longitude)=="character") dt1$Longitude <-as.numeric(dt1$Longitude)
if (class(dt1$Chlorophyll)=="factor") dt1$Chlorophyll <-as.numeric(levels(dt1$Chlorophyll))[as.integer(dt1$Chlorophyll) ]               
if (class(dt1$Chlorophyll)=="character") dt1$Chlorophyll <-as.numeric(dt1$Chlorophyll)
if (class(dt1$Phycocyanin)=="factor") dt1$Phycocyanin <-as.numeric(levels(dt1$Phycocyanin))[as.integer(dt1$Phycocyanin) ]               
if (class(dt1$Phycocyanin)=="character") dt1$Phycocyanin <-as.numeric(dt1$Phycocyanin)
if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]               
if (class(dt1$pH)=="character") dt1$pH <-as.numeric(dt1$pH)
if (class(dt1$DissolvedOxygen_Saturation)=="factor") dt1$DissolvedOxygen_Saturation <-as.numeric(levels(dt1$DissolvedOxygen_Saturation))[as.integer(dt1$DissolvedOxygen_Saturation) ]               
if (class(dt1$DissolvedOxygen_Saturation)=="character") dt1$DissolvedOxygen_Saturation <-as.numeric(dt1$DissolvedOxygen_Saturation)
if (class(dt1$DO_mg)=="factor") dt1$DO_mg <-as.numeric(levels(dt1$DO_mg))[as.integer(dt1$DO_mg) ]               
if (class(dt1$DO_mg)=="character") dt1$DO_mg <-as.numeric(dt1$DO_mg)
if (class(dt1$Temperature)=="factor") dt1$Temperature <-as.numeric(levels(dt1$Temperature))[as.integer(dt1$Temperature) ]               
if (class(dt1$Temperature)=="character") dt1$Temperature <-as.numeric(dt1$Temperature)
if (class(dt1$Specific_Conductivity)=="factor") dt1$Specific_Conductivity <-as.numeric(levels(dt1$Specific_Conductivity))[as.integer(dt1$Specific_Conductivity) ]               
if (class(dt1$Specific_Conductivity)=="character") dt1$Specific_Conductivity <-as.numeric(dt1$Specific_Conductivity)
if (class(dt1$TotalDissolved_Soilds)=="factor") dt1$TotalDissolved_Soilds <-as.numeric(levels(dt1$TotalDissolved_Soilds))[as.integer(dt1$TotalDissolved_Soilds) ]               
if (class(dt1$TotalDissolved_Soilds)=="character") dt1$TotalDissolved_Soilds <-as.numeric(dt1$TotalDissolved_Soilds)
if (class(dt1$Macrophyte)!="factor") dt1$Macrophyte<- as.factor(dt1$Macrophyte)

# Convert Missing Values to NA for non-dates

dt1$DOY <- ifelse((trimws(as.character(dt1$DOY))==trimws("NA")),NA,dt1$DOY)               
suppressWarnings(dt1$DOY <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOY))==as.character(as.numeric("NA"))),NA,dt1$DOY))
dt1$TimeFraction <- ifelse((trimws(as.character(dt1$TimeFraction))==trimws("NA")),NA,dt1$TimeFraction)               
suppressWarnings(dt1$TimeFraction <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TimeFraction))==as.character(as.numeric("NA"))),NA,dt1$TimeFraction))
dt1$DOY_TimeFraction <- ifelse((trimws(as.character(dt1$DOY_TimeFraction))==trimws("NA")),NA,dt1$DOY_TimeFraction)               
suppressWarnings(dt1$DOY_TimeFraction <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOY_TimeFraction))==as.character(as.numeric("NA"))),NA,dt1$DOY_TimeFraction))
dt1$Year <- ifelse((trimws(as.character(dt1$Year))==trimws("NA")),NA,dt1$Year)               
suppressWarnings(dt1$Year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Year))==as.character(as.numeric("NA"))),NA,dt1$Year))
dt1$Latitude <- ifelse((trimws(as.character(dt1$Latitude))==trimws("NA")),NA,dt1$Latitude)               
suppressWarnings(dt1$Latitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Latitude))==as.character(as.numeric("NA"))),NA,dt1$Latitude))
dt1$Longitude <- ifelse((trimws(as.character(dt1$Longitude))==trimws("NA")),NA,dt1$Longitude)               
suppressWarnings(dt1$Longitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Longitude))==as.character(as.numeric("NA"))),NA,dt1$Longitude))
dt1$Chlorophyll <- ifelse((trimws(as.character(dt1$Chlorophyll))==trimws("NA")),NA,dt1$Chlorophyll)               
suppressWarnings(dt1$Chlorophyll <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Chlorophyll))==as.character(as.numeric("NA"))),NA,dt1$Chlorophyll))
dt1$Phycocyanin <- ifelse((trimws(as.character(dt1$Phycocyanin))==trimws("NA")),NA,dt1$Phycocyanin)               
suppressWarnings(dt1$Phycocyanin <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Phycocyanin))==as.character(as.numeric("NA"))),NA,dt1$Phycocyanin))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("NA")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("NA"))),NA,dt1$pH))
dt1$DissolvedOxygen_Saturation <- ifelse((trimws(as.character(dt1$DissolvedOxygen_Saturation))==trimws("NA")),NA,dt1$DissolvedOxygen_Saturation)               
suppressWarnings(dt1$DissolvedOxygen_Saturation <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissolvedOxygen_Saturation))==as.character(as.numeric("NA"))),NA,dt1$DissolvedOxygen_Saturation))
dt1$DO_mg <- ifelse((trimws(as.character(dt1$DO_mg))==trimws("NA")),NA,dt1$DO_mg)               
suppressWarnings(dt1$DO_mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DO_mg))==as.character(as.numeric("NA"))),NA,dt1$DO_mg))
dt1$Temperature <- ifelse((trimws(as.character(dt1$Temperature))==trimws("NA")),NA,dt1$Temperature)               
suppressWarnings(dt1$Temperature <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Temperature))==as.character(as.numeric("NA"))),NA,dt1$Temperature))
dt1$Specific_Conductivity <- ifelse((trimws(as.character(dt1$Specific_Conductivity))==trimws("NA")),NA,dt1$Specific_Conductivity)               
suppressWarnings(dt1$Specific_Conductivity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Specific_Conductivity))==as.character(as.numeric("NA"))),NA,dt1$Specific_Conductivity))
dt1$TotalDissolved_Soilds <- ifelse((trimws(as.character(dt1$TotalDissolved_Soilds))==trimws("NA")),NA,dt1$TotalDissolved_Soilds)               
suppressWarnings(dt1$TotalDissolved_Soilds <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotalDissolved_Soilds))==as.character(as.numeric("NA"))),NA,dt1$TotalDissolved_Soilds))
dt1$Macrophyte <- as.factor(ifelse((trimws(as.character(dt1$Macrophyte))==trimws("NA")),NA,as.character(dt1$Macrophyte)))


swan.raw<-dt1
rm(dt1)

# Here is the structure of the input data frame:
#str(dt1)                            
# attach(dt1)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(Lake)
# summary(Site)
# summary(DOY)
# summary(TimeFraction)
# summary(DOY_TimeFraction)
# summary(Year)
# summary(Latitude)
# summary(Longitude)
# summary(Chlorophyll)
# summary(Phycocyanin)
# summary(pH)
# summary(DissolvedOxygen_Saturation)
# summary(DO_mg)
# summary(Temperature)
# summary(Specific_Conductivity)
# summary(TotalDissolved_Soilds)
# summary(Macrophyte) 
# # Get more details on character variables
# 
# summary(as.factor(dt1$Lake)) 
# summary(as.factor(dt1$Site)) 
# summary(as.factor(dt1$Macrophyte))
# detach(dt1)               





