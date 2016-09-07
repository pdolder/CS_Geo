###############################################################
### Convert the at length data from the survey to a biomass ###
###############################################################

# packages
library(dplyr) 

load('CelticSurveyData.RData') # pre-downloaded data, HH is station, HL is catch

## Some initial cleaning
HH <- filter(HH, HaulVal == 'V') # only valid hauls

## Add species names
load('DatrasSpeciesCodes.RData')
HL$SpeciesName <- DatrasSpeciesCodes$scientific.name[match(HL$SpecCode, DatrasSpeciesCodes$code_number)]

## Add a and b parameters for length-weight
load('length.weight.RData')

HL$SpeciesA <- length.weight$a[match(HL$SpeciesName, length.weight$scientific.name)]
HL$SpeciesB <- length.weight$b[match(HL$SpeciesName, length.weight$scientific.name)]

# Weight at length class

# need as numeric
an <- as.numeric
HL$LngtClass  <- an(HL$LngtClass)
HL$HLNoAtLngt <- an(HL$HLNoAtLngt)
HL$SubFactor  <- an(HL$SubFactor)


# Deal with different length codes - standarise to cm
HL$LngtClass[HL$LngtCode == ". "] <- HL$LngtClass[HL$LngtCode == '. ']/10
HL$LngtClass[HL$LngtCode == 0] <- HL$LngtClass[HL$LngtCode == 0]/10

# Round down length classes & add 0.5
HL$LngtClass[HL$LngtCode != "5"] <- round(HL$LngtClass[HL$LngtCode != "5"])
HL$LngtClass[HL$LngtCode != "5"] <- HL$LngtClass[HL$LngtCode != "5"]+0.5

# boxplot(HL$LngtClass ~ HL$SpeciesName)

# Now raise the length data to weights
# a*L^b

HL$Wt <- (HL$SpeciesA * HL$LngtClass)^HL$SpeciesB * (HL$HLNoAtLngt * HL$SubFactor) * 100 # in kg

# COD <- filter(HL, SpeciesName == 'Gadus morhua')
# boxplot((COD$Wt/COD$HLNoAtLngt) ~ COD$LngtClass) 

## And aggregate across lengths

DF <- HL[!is.na(HL$Wt),]
DF <- DF %>% group_by(Survey, Quarter, Country, Ship, Gear, StNo, HaulNo,Year, SpeciesName) %>%
	summarise(Kg = sum(Wt)) %>% as.data.frame()

# Now merge in the station details: lat, lon etc..
# midpoint of haul locations - small enough distances to not worry about
# spherical distances
HH$HaulLatMid <- (an(HH$ShootLat) + an(HH$HaulLat)) / 2 
HH$HaulLonMid <- (an(HH$ShootLon) + an(HH$HaulLon )) / 2

# Fix blank spaces in variables...
DF$Survey <- gsub(' ','',DF$Survey)
DF$Gear   <- gsub(' ','',DF$Gear)
DF$Ship   <- gsub(' ','',DF$Ship)
DF$StNo   <- gsub(' ','',DF$StNo)

DF2 <- left_join(x = DF, y = HH)

# Standardise hauls per 30 mins - no, use as effort measure
#DF2$HaulDur <- an(DF2$HaulDur)
#DF2$Kg <- an(as.character(DF2$Kg))
#DF2$Kg <- (DF2$Kg / DF2$HaulDur) * 30

# Subset to variables of interest
DF <- DF2[c('Survey','Ship','StNo','HaulNo','Year','SpeciesName','HaulLatMid','HaulLonMid','HaulDur','Kg')]


# Remove marginal areas
DF <- filter(DF, HaulLonMid < -2 & HaulLonMid > -12)
DF <- filter(DF, HaulLatMid >  48 & HaulLatMid < 54)

# Save

save(DF, file = file.path('..','CelticSurveyFormatted.RData'))


