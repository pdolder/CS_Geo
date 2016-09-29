
## This can be generalised, but basically takes the Kmeans knots and assigns a
## habitat based on joining the EMOD data to produce a
#### dataframe of habitats associated with the kmean centers for a covariate

library(VAST)
library(rgdal)

# Load the kmeans
load(file.path('..','..','downloads','2016-09-27_RUN2','Kmeans-100.RData'))

# Create dataframe of kmeans and convert back to LLs
DF <- data.frame(X = Kmeans$centers[,'E_km'], Y = Kmeans$centers[,'N_km'])
attr(DF, 'projection') = 'UTM'
attr(DF, "zone") = 30 
LLs <- PBSmapping::convUL(DF)

# Distances between sample points
hist(dist(coordinates(LLs)))

## http://rstudio-pubs-static.s3.amazonaws.com/7993_6b081819ba184047802a508a7f3187cb.html

# Read in frame
HabMap <- readOGR(file.path('201208_EUSeaMap_Atlantic_Habitats'),'201208_EUSeaMap_Atlantic_Habitats', verbose = F)

## Join on the spatial points

LLs <- SpatialPoints(LLs)
proj4string(LLs) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

join <- over(LLs, HabMap)

LLs <- SpatialPointsDataFrame(LLs, join)
head(LLs)

table(LLs$Level2_des)

KmeanHab <- data.frame(Habitat = LLs$Level2_des)

spplot(HabMap, col.regions = rainbow(length(HabMap)), 
sp.layout = list('sp.points', LLs, pch = 16, col = 'black'))


spplot(HabMap)

plot(HabMap, xlim = c(-12, -2), ylim = c(48, 52))
points(x = coordinates(LLs)[,'X'], y = coordinates(LLs)[,'Y'], col = 'red', pch = '*')
#save(KmeanHab, file = 'KmeansHab.RData')
