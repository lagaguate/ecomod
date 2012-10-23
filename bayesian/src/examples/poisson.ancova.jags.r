

u = read.csv( file="/home/jae/projects/nath/poissonregression/data.csv", sep=";", stringsAsFactors=F )
u$urban_dens = as.factor(u$urban_dens)
u$mortality = 100000*u$COUNT/(u$pop_pct01_15_plus*11)
# u$offset = log(u$pop_pct01_15_plus*11/100000)
u$offset = log(u$pop_pct01_15_plus)
u$qq = as.numeric(as.factor(paste(u$mat_quint_1286_by_URB, u$soc_quint_1286_by_URB)))




	require( R2jags)
	set.seed( seed=1, kind="L'Ecuyer-CMRG")


	causes = unique( u$ucgrp )
	
	for (cs in 1:length(causes) {

		i = which( u$ucgrp == causes[cs] ) # "All_causes"
		v = u[i,]

		# set up the data
		jags.data = list( 
			count = v$COUNT ,
			mat = v$mean_mat_score ,
			soc = v$mean_soc_score ,
			qq = v$qq,
			matse = v$stder_mat_mean_score,
			socse = v$stder_soc_mean_score,
			urban = as.numeric(as.character(v$urban)) ,
			offset = v$offset ,
			# inits= function(){ list( "b"=rnorm(jags.data$nparams), "lambda"=runif(jags.data$n)) }, # used by jags2/or jags.parallel?
			nparams = 6, # the no. of coeff being estimated in ancova
			n = nrow(v),
			n.iter=5000  # 
		)
		
		# sampling call to JAGS
		bm <- jags( data=jags.data,  
			parameters.to.save=c("b", "lambda", "bias"),
			n.iter=jags.data$n.iter,
			# model.file=ancova.basic.bugs
			model.file="ancova.observationerror.bugs"  # this one uses the T() truncator which R does not like to parse. It is saved as a file instead
			# model.file="ancova.heirarchical.bugs"
		)

		bm <- autojags(bm)  # auto update it until it converges
		
		debug = F
		if( debug) {
			# display the output
			print(bm)
			plot(bm)
			# traceplot
			traceplot(bm)
			# if the model does not converge, update it!
		  bm <- update(bm, n.iter=100)
			print( bm )
		  print(bm, intervals=c(0.025, 0.5, 0.975))
			plot(bm)
			# or to use some plots in coda
		  # use as.mcmmc to convert rjags object into mcmc.list
			bm.mcmc <- as.mcmc(bm)
			## now we can use the plotting methods from coda
			xyplot(bm.mcmc)
			densityplot(bm.mcmc)

		}
	
	}

  
  



ancova.basic.bugs = function() {
	# Ancova model for bugs/jags
	
	# Priors for parameters 
	#   b[1] = intercept / constant, b[2] = slope of material depriv index, b[3] = slope of social depriv index, etc .. see below
	#   are assumed to be coef=0 with high variance, resulting in a "skeptical"/uninformative prior
	for (j in 1:nparams) {
		b[j] ~ dnorm(0.0, 1.0e-4) 
	}

	# Likelihood of the data being modelled
	for (o in 1:n) {
		count[o] ~ dpois( lambda[o] ) # mortality rate is assumed to be a poisson process 
		lambda[o] <- exp( offset[o] + b[1] + b[2]*mat[o] + b[3]*soc[o] + b[4]*urban[o] + b[5]*urban[o]*mat[o] + b[6]*urban[o]*soc[o] ) # regression with offset in log space
	}
}



ancova.observationerror.bugs = function() {
	# Ancova model for bugs/jags
	# additional assumptions of observation error 
	
	# Priors for parameters 
	#   b[1] = intercept / constant, b[2] = slope of material depriv index, b[3] = slope of social depriv index, etc .. see below
	#   are assumed to be coef=0 with high variance, resulting in a "skeptical"/uninformative prior
	for (j in 1:nparams) {
		b[j] ~ dnorm(0.0, 0.0001) 
	}

	# Likelihood of the data being modelled
	for (o in 1:n) {
		countTrue[o] ~ dpois( lambda[o] ) # mortality rate is assumed to be a poisson process 
		count[o] ~ dnorm( bias * countTrue[o], count_tau );  # real counts are with error following a normal distribution and some bias
		lambda[o] <- exp( offset[o] + b[1] + b[2]*mat[o] + b[3]*soc[o] + b[4]*urban[o] + b[5]*urban[o]*mat[o] + b[6]*urban[o]*soc[o] ) # regression with offset in log space
		
	}

	count_tau ~ dgamma( 0.0001, 0.0001 )
	bias ~ dnorm(0, 0.0001)

}



### requires additional inputs: 
### 1. raw data from mat and soc scores
### 2. separate data streams for counts by quantile

ancova.heirarchical.bugs = function() {
	# Ancova model for bugs/jags
	# additional assumptions of data distributions associated with mat and soc scores, using raw scores
	
	# Priors for parameters 
	#   b[1] = intercept / constant, b[2] = slope of material depriv index, b[3] = slope of social depriv index, etc .. see below
	#   are assumed to be coef=0 with high variance, resulting in a "skeptical"/uninformative prior
	for (j in 1:nparams) {
		b[j] ~ dnorm(0.0, 0.0001) 
	}

	# Likelihood of the data being modelled
	for (o in 1:n) {
		countTrue[o] ~ dpois( lambda[o] ) # mortality rate is assumed to be a poisson process 
		count[o] ~ dnorm( bias * countTrue[qq[o]], count_tau )  # real counts are with error following a normal distribution and some bias
		lambda[o] <- exp( offset[o] + b[1] + b[2]*mat_mu[o] + b[3]*soc_mu[o] + b[4]*urban[o] + b[5]*urban[o]*mat_mu[o] + b[6]*urban[o]*soc_mu[o] ) # regression with offset in log space
	}

	count_tau ~ dgamma( 0.0001, 0.0001 )
	bias ~ dnorm(0, 0.0001)

}


   
