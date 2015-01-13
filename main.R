## Louise Searle
## January 12 2015

## Lesson 6 Exercise: Find out which city in the Netherlands is the greenest.
rm(list=ls())
## Load packages and source functions.
library(raster)
library(rgdal)
library(downloader)
library(maptools)

## Load data.

# Create data directory.
dir.create('data/', showWarnings=F)

# Get data.
download('https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip', 'data/MODIS.zip', mode='auto', quiet=T)
boundariesNL <- getData('GADM', country='NLD', level=3, path='data/')
# provincesNL <- getData('GADM', country='NLD', level=2, path='data/')

# Unzip data.
unzip('data/MODIS.zip', exdir='data/MODIS/')

# Assign data.
modisFiles <- list.files('data/MODIS/', pattern = glob2rx('*.grd'), full.names = TRUE)
modisBrick <- brick(modisFiles)

## Spatial Analysis.

# Extract mean NDVI per city for time periods.
citiesNDVI <- suppressWarnings(extract(modisBrick, boundariesNL, fun=mean, na.rm=T, match.ID=F, sp=T))
citiesNDVI$Annual <- rowMeans(citiesNDVI@data[15:26], na.rm=T)

# citiesNDVI$Annual <- suppressWarnings(extract(mean(modisBrick), citiesNL, fun=mean, na.rm=T, match.ID=F, sp=T))

# Extract mean NDVI per province for time periods.
provincesAgg <- aggregate(citiesNDVI@data[15:27], list(citiesNDVI$NAME_1), mean)
provincesUnion <- unionSpatialPolygons(citiesNDVI, citiesNDVI$NAME_1)
provincesNDVI <- SpatialPolygonsDataFrame(provincesUnion, provincesAgg, match.ID=F)

# provincesNDVI <- suppressWarnings(extract(modisBrick, boundariesNL, fun=mean, na.rm=T, match.ID=F, sp=T))
# provincesNDVI$Annual <- rowMeans(provincesNDVI@data[13:24], na.rm=T)

# Greenest city calculation.
cityJan <- subset(citiesNDVI, citiesNDVI$January == max(citiesNDVI$January), na.rm=T)
cityAug <- subset(citiesNDVI, citiesNDVI$August == max(citiesNDVI$August), na.rm=T)
cityAnnual <- subset(citiesNDVI, citiesNDVI$Annual == max(citiesNDVI$Annual), na.rm=T)

# Greenest province calculation.
provinceJan <- subset(provincesNDVI, provincesNDVI$January == max(provincesNDVI$January), na.rm=T)
provinceAug <- subset(provincesNDVI, provincesNDVI$August == max(provincesNDVI$August), na.rm=T)
provinceAnnual <- subset(provincesNDVI, provincesNDVI$Annual == max(provincesNDVI$Annual), na.rm=T)

## Visualise Results.

# Plot monthly city mean NDVI for August, January and Annually.
spplot(citiesNDVI, zcol='January', col.regions=colorRampPalette(c('peachpuff4', 'lightgoldenrod', 'darkgreen'))(20), main='Mean NDVI January per City')
spplot(citiesNDVI, zcol='August', col.regions=colorRampPalette(c('peachpuff4', 'lightgoldenrod', 'darkgreen'))(20), main='Mean NDVI August per City')
spplot(citiesNDVI, zcol='Annual', col.regions=colorRampPalette(c('peachpuff4', 'lightgoldenrod', 'darkgreen'))(20), main='Mean NDVI Annual per City')

# Plot monthly province mean NDVI for August, January and Annually.
spplot(provincesNDVI, zcol='January', col.regions=colorRampPalette(c('peachpuff4', 'lightgoldenrod', 'darkgreen'))(20), main='Mean NDVI January per Province')
spplot(provincesNDVI, zcol='August', col.regions=colorRampPalette(c('peachpuff4', 'lightgoldenrod', 'darkgreen'))(20), main='Mean NDVI August per Province')
spplot(provincesNDVI, zcol='Annual', col.regions=colorRampPalette(c('peachpuff4', 'lightgoldenrod', 'darkgreen'))(20), main='Mean NDVI Annual per Province')

# Max NDVI city for August, January and Annually.
opar <- par(mfrow=c(1,3))

plot(citiesNDVI, col='honeydew2', axes=T, lwd=0.2)
box()
plot(cityJan, add=T, col='greenyellow')
mtext(side=3, "Greenest City: January", line=1, adj=0)
text(cityJan, labels=as.character(cityJan$NAME_2), font=2, pos=2, offset=0.8, col='darkgreen')

plot(citiesNDVI, col='honeydew2', axes=T, lwd=0.2)
box()
plot(cityAug, add=T, col='violetred1')
mtext(side=3, "Greenest City: August", line=1, adj=0)
text(cityAug, labels=as.character(cityAug$NAME_2), font=2, pos=2, offset=0.8, col='violetred3')

plot(citiesNDVI, col='honeydew2', axes=T, lwd=0.2)
box()
plot(cityAnnual, add=T, col='skyblue1')
mtext(side=3, "Greenest City: Annual", line=1, adj=0)
text(cityAnnual, labels=as.character(cityAnnual$NAME_2), font=2, pos=2, offset=0.8, col='steelblue4')

# Max NDVI province for August, January and Annually.
opar <- par(mfrow=c(1,3))

plot(provincesNDVI, col='honeydew2', axes=T, lwd=0.2)
box()
plot(provinceJan, add=T, col='greenyellow')
mtext(side=3, "Greenest Province: January", line=1, adj=0)
text(provinceJan, labels=as.character(provinceJan$Group.1), font=2, pos=2, offset=1.2, col='darkgreen')

plot(provincesNDVI, col='honeydew2', axes=T, lwd=0.2)
box()
plot(provinceAug, add=T, col='violetred1')
mtext(side=3, "Greenest Province: August", line=1, adj=0)
text(provinceAug, labels=as.character(provinceAug$Group.1), font=2, pos=2, offset=1.2, col='violetred3')

plot(provincesNDVI, col='honeydew2', axes=T, lwd=0.2)
box()
plot(provinceAnnual, add=T, col='skyblue1')
mtext(side=3, "Greenest Province: Annual", line=1, adj=0)
text(provinceAnnual, labels=as.character(provinceAnnual$Group.1), font=2, pos=2, offset=1.2, col='steelblue4')
