### Example of using INLA for GRF

library(INLA)
library(dplyr)
# Load data set

load(file.path('..','data','CelticSurveyFormatted.RData'))

head(DF)

## Keep just one species from one survey in 1 year
DF <- filter(DF, Survey == 'EVHOE', SpeciesName == 'Gadus morhua', Year == 2011)

## Need to create the inla mesh from the lats and lons

coords <- as.matrix(DF[,c('HaulLonMid','HaulLatMid')])

# These are the same as the example in the book Blangiardo & Cameletti
mesh0 <- inla.mesh.2d(loc = coords, max.edge = 0.1)
mesh1 <- inla.mesh.2d(loc = coords, max.edge = c(0.1, 0.1))
mesh2 <- inla.mesh.2d(loc = coords, max.edge = c(0.2, 0.2))

par(mfrow = c(1,2))
plot(mesh0, main = '') # plot of mesh
points(coords, pch = 21, bg = 1, col = 'white', cex = 1.8) # The data

## cutoff can avoid having too many small triangles around data points, e.g.
mesh1 <- inla.mesh.2d(loc = coords, max.edge = 0.1, cutoff = 0.2, plot.delay = 0.1)
plot(mesh1, main = '') # plot of mesh
points(coords, pch = 21, bg = 1, col = 'white', cex = 1.8) # The data

## Create a projector matrix, e.g.

A.est1 <- inla.spde.make.A(mesh = mesh1, loc = coords)

dim(A.est1) # shows 5228 vertices

## Model fitting ##

# create a matern spde object..

spde <- inla.spde2.matern(mesh = mesh1, alpha = 2)

# define the linear predictor through formula
formula <- y ~ -1 + intercept + f(spatial.field, model = spde) # note -1 is so as not to define an intercept
# spatial field is the random effect, defined as a sequence of integers from
# 1:number of mesh

# fit model with the inla function
output1 <- inla(formula, data = list(y = DF$Kg, intercept = rep(1, spde$n.spde),
				     spatial.field = 1:spde$n.spde),
		control.predictor = list(A = A.est1, compute = TRUE))

round(output1$summary.fixed,3)
round(output1$summary.hyperpar[1,],3)

## Extract the results
output1.field <- inla.spde2.result(inla = output1, name = 'spatial.field', spde = spde, do.transf = TRUE)



