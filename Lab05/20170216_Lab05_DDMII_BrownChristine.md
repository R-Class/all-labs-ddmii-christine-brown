Lab 5 DDMII
================
Christine Brown
February 22, 2017

### Load Packages

``` r
setwd("C:/Users/brown/Documents/Grad School/Fall 16/DDMII/all-labs-ddmii-christine-brown/Lab05")
library( censusapi )
library( dplyr )
library( ggmap )
library( jsonlite )
library( maptools )
library( memisc )
library( RCurl )
library( sp )

censuskey <- "5a1a01fa6496e71097aa2d4a066bed72117c8069"
```

### Assignment

``` r
#Get Data
home.val <- read.csv("https://raw.githubusercontent.com/lecy/hedonic-prices/master/Data/Housing%20Price%20In-Class%20Exercise%20(Responses).csv", stringsAsFactors=FALSE)

#rename columns
names( home.val ) <- c("timestamp","price","X1","X2","sqft","your.name","lot.size","beds","bath",
                       "garage","year","elementary","middle","high","walk","tax","highway","restaurant",
                       "starbucks","park","mall","address","zip","tract" )

#remove commas from numbers and cast as numeric
home.val$price <- as.numeric( gsub( ",","", home.val$price ) )
home.val$sqft <- as.numeric( gsub( ",","", home.val$sqft ) )
home.val$lot.size <- as.numeric( gsub( ",","", home.val$lot.size ) )
home.val$tax <- as.numeric( gsub( ",","", home.val$tax ) )

#replace NA lot sizes with average lot size
home.val$lot.size[ is.na( home.val$lot.size ) ] <- mean( home.val$lot.size, na.rm=T )

#prepare for geocoding

##make addresses street addresses only
home.val$address <- gsub(", Syracuse, NY 13205", "", home.val$address)
home.val$address <- gsub(", Syracuse, NY 13206", "", home.val$address)
home.val$address <- gsub(", Syracuse, NY 13208", "", home.val$address)
home.val$address <- gsub(", Syracuse, NY 13210", "", home.val$address)
home.val$address <- gsub("245 Dale St # 47", "245 Dale St", home.val$address)
home.val$address <- gsub("154 Reed Ave # 56", "154 Reed Ave", home.val$address)
home.val$address <- gsub(", Syracuse, NY", "", home.val$address)
home.val$address <- gsub(" Syracuse, NY", "", home.val$address)
home.val$address <- gsub(",", " ", home.val$address)


##create a full address vector
home.val <- mutate(home.val, full_address=paste(address,", Syracuse, NY"))

#geocode
#coordinates <- suppressMessages(geocode( home.val$full_address, messaging=F ))
#write.csv(coordinates, file = "coordinates.csv", row.names=FALSE)
coordinates <- read.csv("coordinates.csv", stringsAsFactors=FALSE)

#add geocode information to dataframe
home.val2 <- cbind(home.val, coordinates)

#get census tract information
#download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2010/TRACT/2010/tl_2010_36067_tract10.zip", "onondaga census tracts.zip" )
#unzip( "onondaga census tracts.zip" )
#file.remove( "onondaga census tracts.zip" )
onondaga <- readShapePoly( fn="tl_2010_36067_tract10", proj4string=CRS("+proj=longlat +datum=WGS84") )

#join census tract inforamtion with homes
coordinates_SP <- SpatialPoints(coordinates, proj4string=CRS("+proj=longlat +datum=WGS84") )
poly.matched <- over(coordinates_SP, onondaga )
houses.data <- cbind( home.val2, poly.matched )

#get census information
dat.census <- getCensus(name="acs5", vintage=2015, key=censuskey,
                        vars=c( "B01001_001E", "B19013_001E", "B19301_001E", "B22003_002E","B01001B_001E",
                                "B23025_005E", "B17001_002E"),
                        region="tract:*", regionin="state:36+county:067")

#rename census variables
names(dat.census) <- c( "state", "county", "tract", "total_pop", "med_hh_inc",
                        "pc_inc", "hh_snap", "black", "unemployed", "below_poverty" )

#create pasted geoid to match with the houses.data
dat.census <- mutate( dat.census, GEOID=paste( state, county, tract, sep="" ) )

#load spatial data
#download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2010/TRACT/2010/tl_2010_36067_tract10.zip", "onondaga census tracts.zip" )
#unzip( "onondaga census tracts.zip" )
#file.remove( "onondaga census tracts.zip" )
syr <- readShapePoly( fn="tl_2010_36067_tract10", 
                      proj4string=CRS("+proj=longlat +datum=WGS84") )

#add census information to homes
houses.census <- merge(houses.data, dat.census, by.x = "GEOID10", by.y = "GEOID")

#get crimes
crimes <- read.csv( "https://raw.githubusercontent.com/lecy/hedonic-prices/master/Data/crime.lat.lon.csv", stringsAsFactors = F)

#aggregate crimes by census tract
crimes_SP <- SpatialPoints(crimes, proj4string=CRS("+proj=longlat +datum=WGS84") )
crimes.matched <- over(crimes_SP, syr)
crimes.matched <- as.data.frame( table( crimes.matched$GEOID ) )
names(crimes.matched) <- c("GEOID","CRIME_COUNT")

#add crime data to dataset
houses.census.crime <- merge(houses.census, crimes.matched, by.x = "GEOID10", by.y = "GEOID")

#save to csv
write.csv(houses.census.crime, file = "houses.census.crime.csv", row.names=FALSE)
```
