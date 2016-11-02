
library(VAST)

load('Save.RData')

TmbData <- Save$TmbData


Plot_factors(Report = Save$Report, ParHat = Save$ParHat, Data = Save$TmbData, SD = Save$Opt$SD)

