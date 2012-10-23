

# test simulations

# poisson and negative binomial distribution assumptions of taxonomic richness
# assuming that for a given taxon, relative intensity (lambda) of occurence ~ poisson 
# and that that differing taxa, the distribution of lambda ~ gamma
# there is an expectation that taxonomic richness ~ negative binomial

  env.init = c(
		file.path( project.directory("common"), "src", "functions.map.r" ),
		file.path( project.directory("common"), "src", "functions.spatial.r" ),
		file.path( project.directory("common"), "src", "functions.date.r" ),
		file.path( project.directory("common"), "src", "functions.filter.r" ),
    file.path( project.directory("common"), "src", "functions.parallel.r" ),
		file.path( project.directory("common"), "src", "functions.conversion.r" ),
		file.path( project.directory("common"), "src", "functions.utility.r" ),
		file.path( project.directory("common"), "src", "geodesy.r" ),
    file.path( project.directory("bathymetry"), "src", "functions.bathymetry.r" ),
    file.path( project.directory("temperature"), "src", "functions.temperature.r" ),
    file.path( project.directory("habitat"), "src", "functions.habitat.r" ),  # watch out: this accesses temperatures -- must be run before
    file.path( project.directory("taxonomy"), "src", "functions.taxonomy.r" ),
    file.path( project.directory("taxonomy"), "src", "functions.itis.r" ),
    file.path( project.directory("bio"), "src", "functions.bio.r" ),
		file.path( project.directory("diversity")model, "src", "functions.diversitysim.r" )
  )
  
  for (i in env.init) source (i)


  p = list()
	p$init.files = env.init

	p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$data.sources = c("groundfish", "snowcrab") 
   
	p$season = "allseasons"
  
  p$taxa = "maxresolved"
  p$taxa.secondary.filter = "allfish"


	p$Sx = 400  # size in X ; distance scale in km
	p$Sy = 200   # size in Y ; distance scale in km
 	p$dSx = 1 # size of each cell in X; km
 	p$dSy = 1 # size of each cell in X; km
	p$nT = 100  # total number of taxa expected in system
	p$nX = floor(p$Sx / p$dSx )
	p$nY = floor(p$Sy / p$dSy)
	p$nsims = 100

	# variogram constraints
	p$drange = 100
	p$nmax = 100
	p$boundaries = c(5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30, 32.5, 35, 37.5, 40, 42.5, 45, 47.5, 50, 55, 60, 65, 70, 75,  80, 90, 100 )  # distances to use for variogram c
	p$variorgamtype = "exponential"


# ---------------------

	do.simple.nospatialautocorrelation.simulations = F
	if (do.simple.nospatialautocorrelation.simulations) {
		# data distributional assumptions
		# NO spatial structure
			p$type = "gamma"
				# p$type = "beta"
				# p$type = "gaussian"
				# p$type = "uniform"

			p$simulation.method="simple"  ##  Pr(occupancy) as a binomial
				# p$simulation.method="abundance.based.poisson"  # relative abundance modelling and then derive occupancy  
				# p$simulation.method="abundance.based.negativebinomial"  # relative abundance modelling and then derive occupancy  

			p$lambda = sar.create.lambda( p) # create a lambda for each taxon
			p$theta=rep(1,p$nT)  # only required for neg binomial

			res = sar.create.simulation( p ) 
			ntaxa = sar.estimate.from.grid ( res=res, nsamples=50 )
			newdata = sar.models ( ntaxa, samax=500 )

			sar.plot( ntaxa, newdata$newdata )
			sar.plot( ntaxa, newdata$newdata, type="log")

	}


# ---------------------

	do.example.simulations.variogram.based = F
	if (do.example.simulations.variogram.based) {
		# primarily for reference: to visualise meaning of variogram parameters
	
		fn = "~/tmp/fields.pdf"

		require(geoR)
		pdf(file=fn)

		# nugget==tausq; sigmasq==covp1 (partial sill); phi==covp2 (range)
		cp = list(
			list( mod="exponential", covp=c(0.01, 0.2), nugget=0.99, meanv=0, n=4000, grid="reg", vylim = c(0, 1.25), zlim = c(-3,3) ),
			list( mod="exponential", covp=c(0.5, 0.2),  nugget=0.5,  meanv=0, n=4000, grid="reg", vylim = c(0, 1.25), zlim = c(-3,3) ),
			list( mod="exponential", covp=c(0.99, 0.2), nugget=0.01, meanv=0, n=4000, grid="reg", vylim = c(0, 1.25), zlim = c(-3,3) )
		) 
	  simulate.and.plot.random.field(cp)

		cp = list(
	  	list( mod="exponential", covp=c(0.01, 0.1), nugget=0.99, meanv=0, n=4000, grid="reg", vylim = c(0, 1.25), zlim = c(-3,3) ),
			list( mod="exponential", covp=c(0.5, 0.1),  nugget=0.5,  meanv=0, n=4000, grid="reg", vylim = c(0, 1.25), zlim = c(-3,3) ),
			list( mod="exponential", covp=c(0.99, 0.1), nugget=0.01, meanv=0, n=4000, grid="reg", vylim = c(0, 1.25), zlim = c(-3,3) )
		) 
	  simulate.and.plot.random.field(cp)

		cp = list(
			list( mod="spherical", covp=c(0.01, 0.2), nugget=0.99, meanv=0, n=4000, grid="reg", vylim = c(0, 1.25), zlim = c(-3,3) ),
			list( mod="spherical", covp=c(0.5, 0.2),  nugget=0.5,  meanv=0, n=4000, grid="reg", vylim = c(0, 1.25), zlim = c(-3,3) ),
			list( mod="spherical", covp=c(0.99, 0.2), nugget=0.01, meanv=0, n=4000, grid="reg", vylim = c(0, 1.25), zlim = c(-3,3) )
		) 
	  simulate.and.plot.random.field(cp)
		dev.off()

		cmd( "xpdf", fn, "&" ) 
	
	}


# ---------------------


	do.spatially.constrained.simulations = F
	if (do.spatially.constrained.simulations) {

		# estimate.variogram.params(p, DS="redo")  -- must make parallel .. takes days ...
		
		V = estimate.variogram.params(p)
		vars = c("Vmean", "Vvar", "Vrange", "Veffrange", "Vnugget", "Vpsill", "Vsse", "Nall", "Nnonzero")
		v = by( V[,vars], V$sp, FUN=colMeans, na.rm=T, simplify=T )
		v = as.data.frame(t(sapply(v, FUN=function(x){x} )))
		v = v[ which(is.finite(rowSums(v))) ,]
	
		try using body size as a surrogate to estimate parameters for missing data -- not enough data does not mean many species should be ignored


		# require(RandomFields)
		# RFparameters()
		
		require(geoR)


		# xseq = seq( 1,300/4,1)
		# yseq = seq( 1,200/4,1)

		# gr = expand.grid( plon=xseq, plat=yseq)
		
		npts = 4000
		vs = sample.int( nrow(v), 1)

		# DEBUG
		# param=c(mean, variance, nugget, scale, alpha))
		# f <- GaussRF(x=xseq, y=yseq, model="exponential", grid=TRUE, param=parms, n=2 ) 
		# range == "phi" 
		# sigma^2 == "partial sill"
		# f = grf( n=nrow(gr), grid=gr, nsim=1, cov.model="exp", cov.pars=c(v$Vpsill[vs], v$Vrange[vs] ), nugget=v$Vnugget[vs], mean=v$Vmean[vs] )
		# image(f, sim.number=1)
		# hist(f$data)
		# nugget==tausq; sigmasq==covp1 (partial sill); phi==covp2 (range)
		#	points.geodata(f, main="simulated locations and values")
		# plot(f, max.dist=1, main="true and empirical variograms") 

		cp = list(
			list( mod="exponential", covp=c(v$Vpsill[vs], v$Vrange[vs] ), nugget=v$Vnugget[vs], 
						meanv=v$Vmean[vs], n=npts, grid="reg", vylim=c(0, 1.25), zlim = c(-3,3) )
		)
	
		simulate.and.plot.random.field(cp)
		

	}



	
	

