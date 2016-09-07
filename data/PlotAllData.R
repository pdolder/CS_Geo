
rm(list = ls())

library(ggmap);

### Quick plot of Haul Positions from different surveys

  load(file.path('..','data','CelticSurveyFormatted.RData')) ## EVHOE and IE-IGFS
  load(file.path('..','data','CelticSurvey2Formatted.RData')) ## Various Cefas surveys

  # Combine the survey data
  DF$SpeciesName <- toupper(DF$SpeciesName)

  DF2 <- DF

  ac <- as.character
  DF <- data.frame(Survey = c(DF2$Ship,        ac(FSS$fldSeriesName)),
		   Year   = c(DF2$Year,        ac(FSS$Year)),
		   Lat    = c(DF2$HaulLatMid,  FSS$HaulLatMid),
		   Lon    = c(DF2$HaulLonMid,  FSS$HaulLonMid),
		   TowDur = c(DF2$HaulDur,     FSS$fldTowDuration),
		   spp    = c(DF2$SpeciesName, ac(FSS$fldScientificName)),
		   Kg     = c(DF2$Kg,          FSS$Kg))

table(DF$Survey, DF$Year)		   

pdf(file = 'CelticSeaSurveysSummary.pdf', paper = 'a4r')

 print(ggplot(DF, aes(x = Year, y = Survey)) + geom_point() +
	 theme(axis.text.x = element_text(angle = -90)) +
	 ggtitle('Temporal coverage'))

 sort(unique(DF$spp))

  DF <- DF[(DF$spp %in% c('GADUS MORHUA')),]

  DF$SpeciesName <- factor(DF$spp) # drop empty factors
  DF$Ship        <- as.factor(DF$Survey)
  DF$Year        <- as.factor(DF$Year)


map <- get_map("Celtic Sea", zoom = 6)

print(ggmap(map) + geom_point(data = DF,aes(x = Lon, y = Lat, colour = Survey), size = 1, pch = 'x') +
      scale_colour_manual(values =c('darkred','darkgreen','darkblue','purple','orange','brown','black')) +
      ggtitle('Spatial Coverage'))


ggplot(DF, aes(Kg)) + geom_histogram() + facet_wrap(Survey ~ spp, scale = 'free')

print(ggplot(DF, aes(x = Survey,y = as.numeric(as.character(TowDur)))) + geom_boxplot() +
	ggtitle('Tow Duration'))

dev.off()
