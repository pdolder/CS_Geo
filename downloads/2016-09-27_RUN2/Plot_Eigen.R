
library(VAST)
DateFile <- getwd()

load('Save.RData')

SppName <- c('cod_adu','cod_juv','had_adu','had_juv','whg_adu','whg_juv')

Cov_List = Summarize_Covariance( report=Save$Report, parhat=Save$ParHat, tmbdata=Save$TmbData, sd_report=Save$Opt$SD, plot_cor=TRUE, names_set=SppName, figname=paste0(DateFile,"Spatio-temporal_covariances"), plotTF=c("Omega1"=TRUE,"Epsilon1"=TRUE,"Omega2"=TRUE,"Epsilon2"=TRUE), mgp=c(2,0.5,0), tck=-0.02, oma=c(0,5,2,2) )

## Covariances

plot_cov(Cov_List$Cov_omega1)
plot_cov(Cov_List$Cov_epsilon1)

## Eigen decomposition

eigen <- plot_eigen(Cov_List$Cov_omega1)
plot_eigen(Cov_List$Cov_omega2, add = T)




