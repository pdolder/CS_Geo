library(VAST)

## Read in the habitats

load('Habitat_Classifications.RData')

class(file.df.sub)

DF <-  data.frame(X = file.df.sub$long, Y = file.df.sub$lat)
attr(DF, "projection") = "LL"

DF <- PBSmapping::convUL(DF) 

load(file.path('..','..','downloads','2016-09-27_RUN2','Kmeans-100.RData'))
