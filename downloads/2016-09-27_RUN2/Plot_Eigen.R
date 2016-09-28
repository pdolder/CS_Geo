
library(VAST)
DateFile <- getwd()

load('Save.RData')

SppName <- c('cod_adu','cod_juv','had_adu','had_juv','whg_adu','whg_juv')

Cov_List = Summarize_Covariance( report=Save$Report, parhat=Save$ParHat, tmbdata=Save$TmbData, sd_report=Save$Opt$SD, plot_cor=TRUE, names_set=SppName, figname=paste0(DateFile,"Spatio-temporal_covariances"), plotTF=c("Omega1"=TRUE,"Epsilon1"=TRUE,"Omega2"=TRUE,"Epsilon2"=TRUE), mgp=c(2,0.5,0), tck=-0.02, oma=c(0,5,2,2) )

#########################
## Eigen decomposition ##
#########################

par(mfrow = c(1,2))

## Spatial - catch rates
Eigen1 <- Cov_List$Cov_epsilon1[,1]
Eigen2 <- Cov_List$Cov_epsilon1[,2]

plot(x = c(-1,1), y = c(-1,1), type = 'n', xlab = 'PC1', ylab = 'PC2', main = 'Spatio-temporal Encounter')
abline(h = 0)
abline(v = 0)
arrows(0, 0, Eigen1, Eigen2, code = 2, length = 0.1, col = 'red')
text(x = Eigen1, y = Eigen2, labels = SppName, cex = 0.8)

## Spatio-temporal - catch rates
Eigen1 <- Cov_List$Cov_epsilon2[,1]
Eigen2 <- Cov_List$Cov_epsilon2[,2]

plot(x = c(-0.5,0.5), y = c(-0.5,0.5), type = 'n', xlab = 'PC1', ylab = 'PC2', main = 'Spatio-temporal Density')
abline(h = 0)
abline(v = 0)
# points(Eigen1, Eigen2)
arrows(0, 0, Eigen1, Eigen2, code = 2, length = 0.1, col = 'red')
text(x = Eigen1, y = Eigen2, labels = SppName, cex = 0.8)

