## Louise Searle
## January 12 2015

## Lesson 6 Exercise: Find out which city in the Netherlands is the greenest.

# Find the greenest city
# In January
# In August
# On average over the year
# Make at least one map to visualize the results
# What about provinces (see ?aggregate())

## Load packages and source functions.
library(raster)
library(rgdal)
library(downloader)

## Load data.

# Create data directory.
dir.create('data/', showWarnings=F)

# Get data.
download('https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip', 'data/MODIS.zip', mode='auto', quiet=T)
boundNL <- getData('GADM', country='NLD', level=3)

# Unzip data.
unzip('data/MODIS.zip', exdir='data/MODIS/')

# Assign data.
modisFiles <- list.files('data/MODIS/', pattern = glob2rx('*.grd'), full.names = TRUE)
modisBrick <- brick(modisFiles)

## Spatial Analysis.

# Reproject into RD.
boundUTM <- spTransform(boundNL, CRS(proj4string(modisBrick)))

# Create cities data frame.
cities <- data.frame(Name = boundUTM$NAME_2)

# Extract mean NDVI per city and store in dataframe.
cities$January <- extract(modisBrick[[1]], boundUTM, fun=mean, na.rm=T, match.ID=F)
cities$August <- extract(modisBrick[[8]], boundUTM, fun=mean, na.rm=T, match.ID=F)

## Visualise Results.
 
