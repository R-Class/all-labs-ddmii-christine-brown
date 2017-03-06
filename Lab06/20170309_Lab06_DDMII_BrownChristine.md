20170309-Lab06-DDMII
================
Christine Brown
March 9, 2017

``` r
#Load Packages
library( maptools )
library( raster )
library( rgeos )
library( sp )

#Prepare Working Directory
setwd("C:/Users/brown/Documents/Grad School/Fall 16/DDMII/all-labs-ddmii-christine-brown/Lab06")
#dir.create( "shapefiles" )
setwd( "./shapefiles" )

#Download Onondaga Data
##download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2010/TRACT/2010/tl_2010_36067_tract10.zip", "onondaga_census_tracts.zip" )
##unzip( "onondaga_census_tracts.zip" )
##file.remove( "onondaga_census_tracts.zip" )

#Load Onondaga Data in Environment
ono_tracts <- readShapePoly( fn="tl_2010_36067_tract10", proj4string=CRS("+proj=longlat +datum=WGS84") )

#Download Road Data
##download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2015/PRISECROADS/tl_2015_36_prisecroads.zip", "roads.zip" )
##unzip( "roads.zip" )
##file.remove( "roads.zip" )

#Load Road Data in Environment
roads <- readShapeLines( fn="tl_2015_36_prisecroads", proj4string=CRS("+proj=longlat +datum=WGS84") )

#Subset Interstates
interstate <- roads[ roads$RTTYP == "I" , ]

#Get Bounding Box
b <‐ bbox(ono_tracts)

#Create Clipping Function
gClip <‐ function(shp, bb){
if(class(bb) == "matrix") b_poly <‐ as(extent(as.vector(t(bb))), "SpatialPolygons")
else b_poly <‐ as(extent(bb), "SpatialPolygons")
gIntersection(shp, b_poly, byid = T)
}

#Clip Elements to be Plotted
ono_tracts_clipped <‐ gClip(ono_tracts, b)
roads_clipped <‐ gClip(roads, b)
interstate_clipped <‐ gClip(interstate, b)

#Plot Data
par( mar=c(0,0,0,0) )
plot( ono_tracts,  border="gray80" )
plot(roads_clipped, col="steelblue", lwd=1.25, add=T)
plot( interstate_clipped, col="red", lwd=1.75, add=T )

#Create Buffer
buff1 <- gBuffer( interstate, width=.005, byid=T )
buff1 <- gUnaryUnion( buff1, id = NULL )
buff1_clipped <- gClip(buff1, b)
plot(buff1_clipped, border="indianred", lwd=.5, add=T)
```

![](20170309_Lab06_DDMII_BrownChristine_files/figure-markdown_github/unnamed-chunk-1-1.png)
