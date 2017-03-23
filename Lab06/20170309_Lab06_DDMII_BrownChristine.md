20170309-Lab06-DDMII
================
Christine Brown
March 9, 2017

Part 2: Using the Syracuse parcel file: (1) Create a buffer a quarter mile from industrial zones (LandUse). Create a plot to highlight your buffer zone. (2) Identify houses within the buffer zone and create a categorical variable in the dataset indicating proximity to industrial zones. (3) Create a buffer zone an eighth of a mile from schools. Create a plot go highlight these buffer zones. (4) Identify houses within the buffer zone and create a categorical variable in the dataset indicating proximity to schools.

``` r
#Load Packages
library( dplyr )
library( geojsonio )
library( maps )
library( maptools )
library( raster )
library( rgdal )
library( rgeos )
library( sp )
```

Part I

``` r
#Prepare Working Directory
setwd("C:/Users/brown/Documents/Grad School/Fall 16/DDMII/all-labs-ddmii-christine-brown/Lab06")

#Download Onondaga Data
##download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2010/TRACT/2010/tl_2010_36067_tract10.zip", "onondaga_census_tracts.zip" )
##unzip( "onondaga_census_tracts.zip" )
##file.remove( "onondaga_census_tracts.zip" )

#Load Onondaga Data in Environment
ono_tracts <- readShapePoly( fn="tl_2010_36067_tract10", proj4string=CRS("+proj=longlat +datum=WGS84") )
syr <- ono_tracts[as.numeric(as.character(ono_tracts$NAME10)) < 62, ]

#Download Road Data
##download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2015/PRISECROADS/tl_2015_36_prisecroads.zip", "roads.zip" )
##unzip( "roads.zip" )
##file.remove( "roads.zip" )

#Load Road Data in Environment
roads <- readShapeLines( fn="tl_2015_36_prisecroads", proj4string=CRS("+proj=longlat +datum=WGS84") )

#Subset Interstates
interstate <- roads[ roads$RTTYP == "I" , ]
noninterstate <- roads[ roads$RTTYP != "I" , ]

#Clip Roads
interstate_clipped <- gIntersection(syr, interstate, byid = TRUE, drop_lower_td = TRUE) #clip interstate with syr
noninterstate_clipped <- gIntersection(syr, noninterstate, byid = TRUE, drop_lower_td = TRUE) #clip noninterstate with syr

#Create Buffer
buff1 <- gBuffer( interstate_clipped, width=.005, byid=T )
buff1 <- gUnaryUnion( buff1, id = NULL )
buff1_clipped <- gIntersection(syr, buff1)
```

``` r
#Load Houses
houses <- read.csv("https://raw.githubusercontent.com/R-Class/all-labs-ddmii-christine-brown/master/Lab05/houses.census.crime.csv")
coordinates <- read.csv("https://raw.githubusercontent.com/R-Class/all-labs-ddmii-christine-brown/master/Lab05/coordinates.csv")
coordinates_SP <- SpatialPoints(coordinates, proj4string=CRS("+proj=longlat +datum=WGS84") )
highway.zone <- over(coordinates_SP, buff1_clipped )
highway.zone[is.na(highway.zone)] <- 0
highway.zone <- as.data.frame(highway.zone)
houses <- cbind(houses, highway.zone)

#Plot Tracts, Roads, Buffer, and Houses
par( mar=c(0,0,0,0) )
plot(syr, border="gray80")
plot(noninterstate_clipped, col = "mediumpurple3", lwd=1.25, add=T)
plot(interstate_clipped, col = "firebrick", lwd=1.75, add=T)
plot(buff1_clipped, col=rgb(150,50,50,20,maxColorValue=255),add=T, border=F)
points(coordinates_SP[houses$highway.zone==0], pch=20, cex=.6)
points(coordinates_SP[houses$highway.zone==1], col="firebrick", pch=20, cex=.6)

#Add Scale
map.scale( metric=F, ratio=F, relwidth = 0.1, cex=.5 )
```

![](20170309_Lab06_DDMII_BrownChristine_files/figure-markdown_github/unnamed-chunk-3-1.png) Part II

``` r
#load shapefiles
##download.file("https://raw.githubusercontent.com/lecy/geojson/master/syr_parcels.geojson", "syr_parcels.geojson" )
syr_parcels <- readOGR(dsn = "syr_parcels.geojson")
```

    ## OGR data source with driver: GeoJSON 
    ## Source: "syr_parcels.geojson", layer: "OGRGeoJSON"
    ## with 41502 features
    ## It has 64 fields

``` r
#identify industrial parcels
industrial <- syr_parcels[syr_parcels$LandUse == "Industrial",]

#create industrial buffer
buff2 <- gBuffer( industrial, width=.0035, byid=F )
buff2_clipped <- gIntersection(syr_parcels, buff2)
buff2_clipped_proj <- spTransform( buff2_clipped, CRS( "+proj=longlat +datum=WGS84" ) )

#find data in buffer zone and record in dataframe
industrial.zone <- over(coordinates_SP, buff2_clipped_proj )
industrial.zone[is.na(industrial.zone)] <- 0
industrial.zone <- as.data.frame(industrial.zone)
houses <- cbind(houses, industrial.zone)

#plot industrial zone map with data
plot(syr_parcels, border=F, col="gray90" )
plot(industrial, border=F, col="#e2a409", add=T)
plot(buff2_clipped, col=rgb( 226, 164, 9, 40, maxColorValue=255), border=F, add=T)
map.scale( metric=F, ratio=F, relwidth = 0.1, cex=.5 )
points(coordinates_SP[houses$industrial.zone==0], col="gray70", pch=20, cex=1)
points(coordinates_SP[houses$industrial.zone==1], col="gray50", pch=20, cex=1)
```

![](20170309_Lab06_DDMII_BrownChristine_files/figure-markdown_github/unnamed-chunk-4-1.png)
