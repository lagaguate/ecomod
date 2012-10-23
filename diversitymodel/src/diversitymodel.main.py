
import pymc 

import numpy as np
import pdb as pdb

import diversitymodel  # the pymc form of the model

# get data from an R-session
from rpy2.robjects import r
r('source( "~/.Rprofile" )')
r('source( file.path( project.directory("diversity")model, "src", "load.environment.diversitymodel.r" ) )')

# map --> vector conversion requires element-wise conversions with map(fn, list)

nStations =  map( int, r('max(sar$idnum)')) [0] # r2py converts to array .. conversion is a bit convoluted 
nYears = map( int, r('max(sar$yr) ')) [0]  
nSizes = map( int, r('length(nums )')) [0] 

Stations = map( int, r('sar$idnum') ) 
Years = map( int, r('sar$yr') )

SA = map( float, r(' pi * (p$lengthscale)^2 / 1000') )
SAsystem = 100
Robs = np.array( r('Robs') )
eps = 1e-4  # small non-zero number


# S = MCMC(diversitymodel)
# S.sample( iter=10000, burn=5000, thin=10 )
# Matplot.plot(S)



