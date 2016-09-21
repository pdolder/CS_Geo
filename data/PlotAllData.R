
rm(list = ls())

library(ggmap); library(dplyr) 

### Quick plot of Haul Positions from different surveys

  load(file.path('..','data','CelticSurveyFormatted.RData')) ## EVHOE and IE-IGFS
  load(file.path('..','data','CelticSurvey2Formatted.RData')) ## Various Cefas surveys

  # Combine the survey data
  DF$SpeciesName <- toupper(DF$SpeciesName)

  DF2 <- DF

  ac <- as.character
  DF <- data.frame(Survey = 	   c(DF2$Ship,        ac(FSS$fldSeriesName)),
		   Year   =	   c(DF2$Year,        ac(FSS$Year)),
		   Lat    = 	   c(DF2$HaulLatMid,  FSS$HaulLatMid),
		   Lon    = 	   c(DF2$HaulLonMid,  FSS$HaulLonMid),
		   TowDur =        c(DF2$HaulDur,     FSS$fldTowDuration),
		   AreaSwept_km2 = c(DF2$SweptArea,     FSS$SweptArea),
		   spp    = 	   c(DF2$SpeciesName, ac(FSS$fldScientificName)),
		   Kg     = 	   c(DF2$Kg,          FSS$Kg))

table(DF$Survey, DF$Year)		   


pdf(file = 'CelticSeaSurveysSummary.pdf', paper = 'a4r')

##############################
## Plot the haul level data ##
##############################

## Temporal coverage by survey

 print(ggplot(DF, aes(x = Year, y = Survey)) + geom_point() +
	 theme(axis.text.x = element_text(angle = -90)) +
	 ggtitle('Temporal coverage'))

 sort(unique(DF$spp))

# Plot the max and min bearings by year

bearings <- group_by(DF, Year) %>% summarise(maxLat = max(Lat), minLat = min(Lat),
					     maxLon = max(Lon), minLon = min(Lon))

print(ggplot(bearings, aes(x = Year)) + 
      geom_segment(aes(x = Year, xend = Year, y = minLat, yend = maxLat, group = Year), size = 2) +
      expand_limits(y = c(46,56)) + ggtitle('Range of Latitude') + theme(axis.text.x = element_text(angle = -90)))

print(ggplot(bearings, aes(x = Year)) + 
      geom_segment(aes(x = Year, xend = Year, y = minLon, yend = maxLon, group = Year), size = 2) +
      expand_limits(y = c(-14,0))+ ggtitle('Range of Longitude') + theme(axis.text.x = element_text(angle = -90)))

# Map the haul data

Hauls <- reshape2::dcast(DF, Survey + Year + Lat + Lon + TowDur + AreaSwept_km2 ~ spp, value.var = 'Kg')
Hauls <- Hauls[c('Survey','Year','Lat','Lon','TowDur', 'AreaSwept_km2')]

print(plot(table(Hauls$Year), main = 'No Stations per year', type = 'b'))

map <- get_map("Celtic Sea", zoom = 6)

# All hauls over years
print(ggmap(map) + geom_point(data = Hauls,aes(x = Lon, y = Lat, colour = Survey), size = 1, pch = 'x') +
      scale_colour_manual(values =c('darkred','darkgreen','darkblue','purple','orange','brown','black')) +
      ggtitle('Spatial Coverage'))

# All hauls, by year
print(ggmap(map) + geom_point(data = Hauls,aes(x = Lon, y = Lat, colour = Survey), size = 1, pch = 'x') +
      scale_colour_manual(values =c('darkred','darkgreen','darkblue','purple','orange','brown','black')) +
      facet_wrap(~ Year) + ggtitle('Spatial Coveragei by year'))

# Tow duration by survey
print(ggplot(DF, aes(x = Survey,y = as.numeric(as.character(TowDur)))) + geom_boxplot() +
	ggtitle('Tow Duration'))


plot(Hauls$AreaSwept_km2 ~ Hauls$Survey)


# Plot the CPUE per survey, per year

CPUE <- group_by(DF, Survey, spp, Year) %>% summarise(q10 = quantile(100 * (Kg/as.numeric(TowDur)), prob = 0.05), 
						      q50 = quantile(100 * (Kg/as.numeric(TowDur)), prob = 0.5), 
						      q90 = quantile(100 * (Kg/as.numeric(TowDur)), prob = 0.9)) 

s <- c('GADUS MORHUA','MELANOGRAMMUS AEGLEFINUS','MERLANGIUS MERLANGUS')

for (i in 1:length(s)) {
print(ggplot(filter(CPUE, spp %in% s[i]), aes(x = Year)) +
	 geom_line(aes(y = q50, colour = Survey, group = spp)) + 
	 geom_line(aes(y = q10, colour = Survey, group = spp), linetype = 2) +
	 geom_line(aes(y = q90, colour = Survey, group = spp), linetype = 2) +
	 facet_grid(Survey ~ spp) + theme(axis.text.x = element_text(angle = -90))) 
}

  #DF <- DF[(DF$spp %in% c('GADUS MORHUA')),]

  #DF$SpeciesName <- factor(DF$spp) # drop empty factors
  #DF$Ship        <- as.factor(DF$Survey)
  #DF$Year        <- as.factor(DF$Year)



dev.off()


