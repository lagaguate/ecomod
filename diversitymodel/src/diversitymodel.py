"""
Diversity model is ...

"""

import pymc as pm 
import numpy


def main():
	# By default, run the model if called directly
	M = MCMC( [p, z, ... varlist ] )
	M.sample(10000, 1000, verbose=2)
	Matplot.plot(M)

if __name__ == '__main__':
	main()`


# Species-area curve parameterization 
Ztau = pm.Gamma('Ztau', alpha=10.e-3., beta=10.e-3.,  value=10.e-3. )
@pm.stochastic(dtype=float)
def Z(value=0.25):
	#[0.25]*nStations
	return 
p = pm.truncnorm_like( 0.25, , Ztau, 0., 1. ) 

Bmu = pm.Gamma('Bmu', alpha=10e-3, beta=10e-3,  value=10e-3  )  # one intercept 
Btau = pm.Gamma('Btau', alpha=10e-3, beta=10e-3,  value=10e-3  )
B = pm.Normal('B', Bmu, Btau )

Rtau = pm.Gamma('Rtau', alpha=10e-3, beta=10e-3,  value=10e-3  ) 
Rhat = np.array([np.empty(nSizes, dtype=object)]*nStations )
for i in range(nStations) :
	Rmu = pm.Lambda('Rmu', lambda B=B, SA=SA, Zi=Z[i] : np.log( np.maximum( eps, B*SA**Zi) ) ) 
	Rhat[i,:] = pm.Normal( 'Rhat', Rmu, Rtau, observed=True, value=np.log(np.maximum( eps, Robs[i,:])) )
# 6 GB for 5 years of data 4881 stations 


 
Rp = pm.Lognormal( 'Rp', Pmu , Ptau )


# observation model; Rest = value estimated for a fixed system size
# rarefaction curve parameter estimation
for i in range(1:nStations) :
		
			) {
    }
    
    # prediction to a constant system size (SAsystem)
    Rs[i] <- b0[i] * pow( SAsystem, Z[i] )
    
  }



for (i in 1:nStations) {
    Z[i] ~ dnorm( 0.25, 10e-6 )
    tauP[i] ~ dgamma( 10e-3, 10e-3 )
    sdP[i] <- 1/sqrt(tauP[i])
  }

  for (y in 1:nYears) {
    w[y] ~ dgamma( 10-3, 10e-3)
    tauO[y] ~ dgamma( 10e-3, 10e-3 )
    tauR[y] ~ dgamma( 10e-3, 10e-3 )
    sdO[y] <- 1/sqrt(tauO[y])
    sdR[y] <- 1/sqrt(tauR[y])
  }

  # observation model; Rest = value estimated for a fixed system size
  for (i in 1:nStations) {

    # rarefaction curve parameter estimation
    for ( ai in 1:nSizes ) {
      Rpred[i,ai] ~ dlnorm( log( max(eps, b0[i] * pow( SA[ai], Z[i]))), tauP[i] )
    }
    
    # prediction to a constant system size (SAsystem)
    Rs[i] <- b0[i] * pow( SAsystem, Z[i] )
    
  }


  # observation model; Rest = value estimated for a fixed system size
  for (y in 1:nStations) {
		Rmean[y] <- mean( Rs[Years[i]] )
    Rsd[y] <- sd( Rs[Years[i]] )
  }
  for (y in 1:nYears) {
		Rmean[y] <- mean( Rs[Years[i]] )
    Rsd[y] <- sd( Rs[Years[i]] )
    
    # mean estimates for the whole system by year; R= Rreal and RO = R observed
		RO[y] ~ dnorm( Rs[y], tauO[y]  )
  }
 
  # observation model fixed system size
  for (i in 1:nStations) {
		# mean estimates for the whole system by year; R= Rreal and RO = R observed
		RO[Years[i]] ~ dnorm( R[Years[i]], tauO[Years[i]]  )
  }
 

	# process model (timeseries)
	R[1] ~ dnorm( mean(RO),   )      
	for(y in 2:nYears) {
		R[y] ~ dlnorm( log( max(R[y-1]*( 1 + w[j]*(1-R[y-1,j])), eps)), pow( cvR, -2 ) ) ;
	}




