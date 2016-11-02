
library(VAST)

load('Save.RData')

TmbData <- Save$TmbData


Plot_factors(Report = Save$Report, ParHat = Save$ParHat, Data = Save$TmbData, SD = Save$Opt$SD)

## Error, TmbData not found
## Error in Recalculating L_pj from Cov_jj... Error in Psi[i,,j] : incorrect
## number of dimensions

Cov_List <- Summarize_Covariance(Report = Save$Report, ParHat = Save$ParHat, 
				 Data = Save$TmbData, SD = Save$Opt$SD, category_names = 1:dim(Save$Report$D_xct)[2])

SpatialDFA::Rotate_Fn(Cov_jj = Cov_List[['Cov_epsilon1']][,,'Estimate'], 
		      Psi = Save$Report$Epsilon1_sct)
