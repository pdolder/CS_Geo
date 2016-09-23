
rm(list = ls())

library(VAST)

load('Save.RData')

## Original data ####

# Read or simulate trawl data
  load(file.path('..','..','data','CelticSurveyFormattedSize.RData')) ## EVHOE and IE-IGFS
  load(file.path('..','..','data','CelticSurvey2FormattedSize.RData')) ## Various Cefas surveys

  # Combine the survey data
  DF$SpeciesName <- toupper(DF$SpeciesName)

  DF2 <- DF

  ac <- as.character
  DF <- data.frame(Survey        = c(DF2$Ship,        ac(FSS$fldSeriesName)),
		   Year          = c(DF2$Year,        ac(FSS$Year)),
		   Station       = c(DF2$StNo,        FSS$fldCruiseStationNumber),
		   Lat           = c(DF2$HaulLatMid,  FSS$HaulLatMid),
		   Lon           = c(DF2$HaulLonMid,  FSS$HaulLonMid),
		   AreaSwept_km2 = c(DF2$SweptArea,   FSS$SweptArea),
		   spp           = c(DF2$SpeciesName, ac(FSS$fldScientificName)),
		   Kg            = c(DF2$Kg,          FSS$Kg))

 table(DF$Survey, DF$Year)		   

 sort(unique(DF$spp)) 
 DF <- DF[(DF$spp %in% c('GADUS MORHUA_Juv',
			 'GADUS MORHUA_Adu',
			 'MELANOGRAMMUS AEGLEFINUS_Juv',
			 'MELANOGRAMMUS AEGLEFINUS_Adu',
		         'MERLANGIUS MERLANGUS_Juv',		 
			 'MERLANGIUS MERLANGUS_Adu')),]

  DF <- DF[(DF$Year %in% c(1990:2015)),]
  
  ## Add missing zeros
  DF <- reshape2::dcast(DF, Survey + Year + Station + Lat + Lon + AreaSwept_km2 ~ spp, value.var = 'Kg', fill = 0)
  DF <- reshape2::melt(DF, id = c('Survey','Year','Station','Lat','Lon','AreaSwept_km2'), value.name = 'Kg')
  colnames(DF)[7] <- 'spp'

  DF$SpeciesName <- factor(DF$spp) # drop empty factors
  DF$Ship        <- as.factor(DF$Survey)
  DF$Year        <- as.factor(DF$Year)

  # Remove some surveys
  # DF <- DF[DF$Survey %in% c('CEXP','THA2','WCGFS','Q1SWIBTS','Q1SWBEAM'),]

## Results

Opt     <- Save$Opt
Report  <- Save$Report
ParHat  <- Save$ParHat
TmbData <- Save$TmbData

strata.limits <- data.frame('STRATA'="All_areas")
BiasCorr = FALSE 

DateFile = getwd()

an <- as.numeric

Index <-  SpatialDeltaGLMM::PlotIndex_Fn( DirName=DateFile, TmbData=TmbData, Sdreport=Opt$SD, Year_Set=seq(min(an(as.character(DF[,'Year']))),max(an(as.character(DF[,'Year'])))), strata_names=strata.limits[,1], category_names=levels(DF[,'SpeciesName']), use_biascorr=BiasCorr, cex = 0.3)

Index <- Index$Table

Index$Hi <- Index$Estimate..metric.tonnes. + Index$SD..natural.
Index$Lo <- Index$Estimate..metric.tonnes. - Index$SD..natural.

library(ggplot2)
library(ggthemes)

theme_set(theme_tufte())

print(ggplot(Index, aes(x = Year, y = Estimate..metric.tonnes.)) + 
	geom_line() + geom_pointrange(aes(ymin = Hi,
					ymax = Lo)) + 
facet_wrap(~Category, scale = 'free_y', ncol = 2) + 	expand_limits(y = 0))


## ICES assessment SSB
CS <- read.csv(file.path('..','..','..','SFG_talk','CSAssessmentOutput2016.csv'))

library(dplyr)
CS <- filter(CS, StockDescription %in% unique(CS$StockDescription)[c(5,7,12)])

CS$SpeciesName <- toupper(CS$SpeciesName)
CS$Category <- paste(CS$SpeciesName,'Adu', sep = '_')

# scale the data
CS_scaled <- by(CS[,c('High_StockSize','StockSize','Low_StockSize')], CS[,'Category'], scale)

CS$scaled <- c(CS_scaled[['MELANOGRAMMUS AEGLEFINUS_Adu']][,'StockSize'], 
	       CS_scaled[['MERLANGIUS MERLANGUS_Adu']][,'StockSize'],
	       CS_scaled[['GADUS MORHUA_Adu']][,'StockSize'])

CS$scaledMult <- (CS$High_StockSize - CS$StockSize) / CS$StockSize

Index <- filter(Index, Category %in% unique(grep('Adu', Index$Category, value = T))) 

Index_scaled <- by(Index[,c('Estimate..metric.tonnes.','Hi','Lo')], Index[,'Category'], scale)

Index$scaled <- c(Index_scaled[['GADUS MORHUA_Adu']][,'Estimate..metric.tonnes.'],
		  Index_scaled[['MELANOGRAMMUS AEGLEFINUS_Adu']][,'Estimate..metric.tonnes.'], 
	          Index_scaled[['MERLANGIUS MERLANGUS_Adu']][,'Estimate..metric.tonnes.']
	         )

Index$scaledMult <- (Index$Hi - Index$Estimate..metric.tonnes.)/Index$Estimate..metric.tonnes.


## Plot indices

print(ggplot(Index, aes(x = Year, y = scaled)) + 
	geom_line() + geom_pointrange(aes(ymin = scaled - scaled * scaledMult,
					ymax = scaled + scaled * scaledMult)) + 
facet_wrap(~Category, scale = 'free_y', ncol = 1) + 	expand_limits(y = 0) +
geom_line(data = CS, aes(x = Year, y = scaled), col = 'blue') +
geom_ribbon(data = CS, aes(x = Year, ymin = scaled - scaled * scaledMult, 
			   ymax = scaled + scaled * scaledMult), alpha = 0.5, fill = 'blue') +
      ggtitle('Blue = Assessment, black = VAST estimates'))
ggsave('RealativeIndexVRelativeAssessSSB.png')


Combined <- Index
Combined$AssessPop <- CS$scaled[match(paste(Combined$Category, Combined$Year),
				      paste(CS$Category, CS$Year))]

print(ggplot(Combined, aes(x = AssessPop, y = scaled)) + geom_point() +
	facet_wrap(~Category) + geom_smooth(method = 'lm') +
	geom_abline(intercept = 0, slope = 1, col = 'red') + 
	ggtitle('x = Relative abundance estimates from assessment, 
		\n y = Relative abundance estimates from VAST.
		\n blue = LM, Red, slope = 1, intercept = 0'))
		ggsave('Fidelity with assessment.png')


