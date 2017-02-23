Lab 5 DDMII
================
Christine Brown
February 22, 2017

``` r
setwd("C:/Users/brown/Documents/Grad School/Fall 16/DDMII/all-labs-ddmii-christine-brown/Lab05")
library( censusapi )
library( dplyr )
library( ggmap )
library( jsonlite )
library( memisc )
library( RCurl )

censuskey <- "5a1a01fa6496e71097aa2d4a066bed72117c8069"
```

Create a "research database" of home values in Syracuse 1. Start with the dataset of home prices and assets collected from Zillow 2. Add a census tracts FIPS ID to each home (spatial join to census tracts) 3. Add census data to each home 4. Aggregate crimes by census tract (spatial join) and add to the dataset

Get Data

``` r
home.val <- read.csv("https://raw.githubusercontent.com/lecy/hedonic-prices/master/Data/Housing%20Price%20In-Class%20Exercise%20(Responses).csv", stringsAsFactors=FALSE)
#rename columns
names( home.val ) <- c("timestamp","price","X1","X2","sqft","your.name","lot.size","beds","bath",
                       "garage","year","elementary","middle","high","walk","tax","highway","restaurant",
                       "starbucks","park","mall","address","zip","tract" )
# remove commas from numbers and cast as numeric
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
home.val$address <- gsub(", Syracuse, NY", "", home.val$address)
home.val$address <- gsub(" Syracuse, NY", "", home.val$address)
home.val$address <- gsub(",", " ", home.val$address)
```

``` r
##create a full address vector
home.val <- mutate(home.val, full_address=paste(address,", Syracuse, NY"))
```

``` r
#geocode
coordinates <- suppressMessages(geocode( home.val$full_address, messaging=F ))
```

``` r
#add geocode information to dataframe
home.val <- cbind(home.val, coordinates)
```
