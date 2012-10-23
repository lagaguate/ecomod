

sar.create.lambda = function( p ) {


	if (p$type == "gamma" ) {
		# intensity (relative frequency) of occurence ~ gamma distribution parameters
		if ( ! exists( "p1", p))  p$p1 = 2  # shape -- skewed distribution (0, 1)
		if ( ! exists( "p2", p))	p$p2 = 0.001 # scale
		lambda = rgamma( p$nT, shape=p$p1, scale=p$p2)  # represents relative proportion of stations where a taxon is observed
	}

	if ( p$type=="beta" ) {
		if ( ! exists( "p1", p))  p$p1 = 2  # 
		if ( ! exists( "p2", p))	p$p2 = 1000 # scale
		lambda = rbeta( p$nT, p$p1, p$p2 )
	}
	
	if ( p$type=="gaussian" ) {
		if ( ! exists( "p1", p))  p$p1 = 0.002  # 
		if ( ! exists( "p2", p))	p$p2 = 0.0002 # scale
		lambda = rnorm( p$nT, p$p1, p$p2 )
	}

	if ( p$type=="uniform" ) {
		if ( ! exists( "p1", p))  p$p1 = 0.0001  # 
		if ( ! exists( "p2", p))	p$p2 = 0.0100 # scale
		lambda = rnorm( p$nT, p$p1, p$p2 )
	}

	lambda[ which (lambda<=0) ] = 0.00001
	lambda[ which (lambda>1) ]  = 0.999
	
	hist(lambda, "fd")
	
	return (lambda)

}




sar.create.simulation = function(p ) {
	
	nlocations = p$nX * p$nY # assume a square of 200 X 200 cells
	res = array( 0, c( p$nX, p$nY, p$nT ) )
	
	if (p$simulation.method=="simple") {
		for ( i in 1:p$nT) {
			res[,,i] = rbinom( nlocations, 1, p$lambda[i] )
		}
	}

	if (p$simulation.method=="abundance.based.poisson") {
	  for ( i in 1:p$nT) {
			abu = array( rpois(nlocations, p$lambda[i]), c( p$nX, p$nY ) )
			abu[ which(abu>0) ] = 1 	
			res[,,i] = abu
		}
	}
	
	if (p$simulation.method=="abundance.based.negativebinomial") {
	  require(MASS) # rnegbin
		
		for ( i in 1:p$nT) {
			abu = array( rnegbin(nlocations, mu=p$lambda[i], theta=p$theta[i]), c( p$nX, p$nY ) )
			abu[ which(abu>0) ] = 1 	
			res[,,i] = abu
		}
	}


	nx = apply( res, c(1,2), sum )
	image (nx)

	return (res)
}




sar.estimate.from.grid = function( res, nsamples=10 ) {
	# simple counting mechanism using square blocks ... 
  nX = dim(res)[1]
  nY = dim(res)[2]
  nT = dim(res)[3]
	lmax = floor( max(nX,nY) * 0.2 )
	nt = sm = array ( 0, c( nsamples,  lmax)) 
	
	# estimate sar 
	for ( i in 1: nsamples ) {
		ptx = floor( runif( nsamples ) * nX )
		pty = floor( runif( nsamples ) * nY )
		for ( j in 0: (lmax-1) ) {
			xv = ptx[i] + (-j:j)
			yv = pty[i] + (-j:j)
			xv = xv [ which( xv >0 & xv<=nX )]
			yv = yv [ which( yv >0 & yv<=nY )]
			wn = res[xv, yv, ]
			wn = array( wn, c( length(xv), length(yv), nT))
			nt[i,(j+1)] = length( which( apply( wn, c(3), function(x) {any (x==1)} ) ))
			sm[i,(j+1)] = length( wn[,,1] )
		}
	}
	 
	colnames(nt) = 1:lmax
	nt2 = as.data.frame.table( as.matrix(nt))
	names( nt2) = c("id", "ls", "rich")
 
	colnames(sm) = 1:lmax
	nt3 = as.data.frame.table( as.matrix(sm))
	names( nt3) = c("id", "ls", "sa")

	ntaxa = merge( nt2, nt3, by=c("id", "ls"), sort=F )
	ntaxa = ntaxa [ which( ntaxa$sa >0) , ]

	return ( ntaxa )
}




sar.models = function( ntaxa, samax=NULL ) {

	if (!is.null(samax)) {
		ntaxa = ntaxa[ which( ntaxa$sa <= samax ) ,]
	}

	m0 = lm( log(rich+1) ~ log(sa), data=ntaxa ) 
	# Z = 0.43973
	
	m1 = glm( rich ~ log(sa), data=ntaxa, family=poisson ) 
		# Z = 0.26875
	
	m2 = glm( log(rich+1) ~ log(sa), data=ntaxa, family=gaussian ) 
		# Z = 0.4556

	m3 = glm( rich ~ log(sa), data=ntaxa[ which(ntaxa$rich>0),], family=Gamma(link="log") ) 
		# Z = 0.404

	require(mgcv)
	m4 = gam( rich ~ s(log(sa)), data=ntaxa[ which(ntaxa$rich>0),]) 

	m5 = nls( rich ~ sa^Z, data=ntaxa, start=list(Z=0.45) )

	newdata = data.frame( sa=seq(min(ntaxa$sa), max(ntaxa$sa), length.out=30 ))

	newdata$m0 = exp( predict( m0, newdata, type="response" ) ) - 1
	newdata$m1 = predict( m1, newdata, type="response" )
	newdata$m2 = exp( predict( m2, newdata, type="response" ) ) -1
	newdata$m3 = predict( m3, newdata, type="response" )
	newdata$m4 = predict( m4, newdata, type="response" )
	newdata$m5 = predict( m5, newdata, type="response" )
	
	return( list(newdata=newdata, lm=m0, glmpoisson=m1, glmgaussian=m2, glmgamma=m3, gam=m4, nls=m5) )

}




sar.plot = function( ntaxa, newdata, type="default" ) { 

	if (type == "default" ) {
		plot( rich ~ sa, ntaxa, pch=22, cex=0.25, col="gray")
		lines( m0 ~ sa, newdata, col="green"  )
		lines( m1 ~ sa, newdata, col="red"  )
		lines( m2 ~ sa, newdata, col="purple"  )
		lines( m3 ~ sa, newdata, col="blue"  )
		lines( m4 ~ sa, newdata, col="yellow"  )
		lines( m5 ~ sa, newdata, col="orange"  )
	} 

	if (type == "log" ) {
		plot( log(rich) ~ log(sa), ntaxa, cex=0.25, col="gray")
		lines( log(m0) ~ log(sa), newdata, col="green" )
		lines( log(m1) ~ log(sa), newdata, col="red" )
		lines( log(m2) ~ log(sa), newdata, col="purple" )
		lines( log(m3) ~ log(sa), newdata, col="blue" )
		lines( log(m4) ~ log(sa), newdata, col="yellow" )
		lines( log(m5) ~ log(sa), newdata, col="orange" )
	} 
}



estimate.variogram.params = function(p, DS=NULL) {
	
	fn = file.path( project.directory("diversity")model, "data", "variogram.rdata" )
	fn2 =  file.path( project.directory("diversity")model, "data", "variogram.raw.rdata" )
	
	if (is.null(DS)) {
		res = NULL
		if (file.exists(fn)) load(fn)
		return (res)
	}
	
	if (DS=="raw") {
		res = NULL
		if (file.exists(fn2)) load(fn2)
		return (res)
	}


	OO = bio.db( DS="subset", p=p) 

  sset = OO$set
 

	rm (OO); gc()

	sps = unique( scat$spec )
	yrs = unique( sset$yr )

	coords = c("plon", "plat") 
	va = "zn"




	require(geoR)

	res = data.frame()

	for (sp in sps) {
	for (yr in yrs ) {
	
		# yr = 2010
		# sp = 10
		
		iscat = which( scat$spec==sp )
		if (length( iscat) < 30 ) next()
		
		isset = which( sset$yr == yr & sset$id %in% unique( scat$id )  )
		if (length( isset) < 10 ) next()

		dat = merge( sset[isset, c("id", "plon", "plat")], scat[iscat,], by=c("id"), all.x=T, all.y=F, sort=F )

	
		# dat = dat[ which( is.finited(dat[,va])), ] 	 <<<--- two-stage model is better?	
		ina = which( !is.finite(dat[,va]))
		if ( (nrow(dat) - length(ina)) < 10) next()

		if (length( ina) > 0) dat[ ina, va] = 0		
		
		Vvar =  var( dat[,va], na.rm=T )
		Vmean	= mean( dat[,va], na.rm=T )
		
		out= NULL
		dat = dat[, c("plon", "plat", va )]
		dat[, c("plon", "plat")] = jitterDupCoords( dat[, c("plon", "plat")] , max=1, min=0.25  )

		g = as.geodata( dat )

		vario0 = variog(g, uvec=p$boundaries, max.dist=p$drange, pairs.min=3, estimator.type= "classical")
		# plot( vario0)

		x = try( variofit( vario0, cov.model=p$variorgamtype, ini.cov.pars = c(Vvar/2, p$drange/4), nugget=Vvar/2, weights="equal"), silent=T )
		  if (! ("try-error" %in% class(x) )) out = rbind( out, c(x$nugget, x$cov.pars, x$practicalRange, x$value) )
			# lines(x)
		x = try( variofit( vario0, cov.model=p$variorgamtype, ini.cov.pars = c(Vvar/2,  p$drange/4), nugget=Vvar/2, weights="cressie"), silent=T)
		  if (! ("try-error" %in% class(x) )) out = rbind( out, c(x$nugget, x$cov.pars, x$practicalRange, x$value) )
		
		x = try( variofit( vario0, cov.model=p$variorgamtype, ini.cov.pars = c(Vvar/2, p$drange/4), nugget=Vvar/2, weights="npairs"), silent=T )
		  if (! ("try-error" %in% class(x) )) out = rbind( out, c(x$nugget, x$cov.pars, x$practicalRange, x$value) )
	

		colnames(out) = c( "tausq", "sigmasq", "phi", "range", "sse" ) 
		  # correlation function C(h) = sigma^2 * rho(h) , where e.g. for "exp":: rho(h) = exp(-h/phi)
			# sigmasq==partial sill
			# phi = range
			# tausq = nugget
			# effective range = practicalRange = range at wich correlation drops to 0.05

		Vest = apply( out, 2, weighted.mean, w=(1/out[,"sse"]))  # to use as starting values
		
		cov.params = as.vector( Vest[c("sigmasq", "phi") ])
		nugget =  as.vector( Vest["tausq"] )

		x0 = try (likfit( g, cov.model=p$variorgamtype, ini.cov.pars=cov.params, nugget=nugget, lik.method = "ML"), silent=T )
			if (! ("try-error" %in% class(x0) )) out = rbind( out, c(x0$nugget, x0$cov.pars, x0$practicalRange, x0$AIC) ) 
		
		x1 = try (likfit( g, cov.model=p$variorgamtype, ini.cov.pars=cov.params, nugget=nugget, lik.method = "REML"), silent=T )
		  if (! ("try-error" %in% class(x1) )) out = rbind( out, c(x1$nugget, x1$cov.pars, x1$practicalRange, x1$AIC) )
		
		if ( ("try-error" %in% class(x0) ) & ("try-error" %in% class(x1) ) ) Vest = Vest  # nothing to do
		if ( !("try-error" %in% class(x0) ) & ("try-error" %in% class(x1) ) ) Vest = c(x0$nugget, x0$cov.pars, x0$practicalRange, x0$AIC)
		if ( ("try-error" %in% class(x0) ) & !("try-error" %in% class(x1) ) ) Vest = c(x1$nugget, x1$cov.pars, x1$practicalRange, x1$AIC)
		if ( !("try-error" %in% class(x0) ) & !("try-error" %in% class(x1) ) ) {
			if (x0$AIC > x1$AIC ) {
				Vest = c(x1$nugget, x1$cov.pars, x1$practicalRange, x1$AIC)
			} else {
				Vest = c(x0$nugget, x0$cov.pars, x0$practicalRange, x0$AIC)
			}
		}	

		names(Vest) = colnames(out)

		Vtot = Vest["tausq"] + Vest["sigmasq"]
		Vrange = Vest["phi"] 
		Vnugget = Vest["tausq"] / Vtot  # %; assume max variance attained by the end of the distance series
		Vpsill = Vest["sigmasq"] / Vtot   # %; " " "
		Veffrange = Vest["range"]
		
		res = rbind( res, data.frame(cbind( sp, yr, Vmean, Vvar, Vrange, Veffrange, Vnugget, Vpsill, Vsse=Vest["sse"], Nall=nrow(dat), Nnonzero=nrow(dat)-length(ina)  )) )

    # g = gstat(g, id=va, model=f)
		# out = predict(object=g, newdata=newdata, block=c(pres,pres) )
       
	}}

	save( res, file=fn2, compress=T )
	
	ibad = which( 
		res$Vrange > 0.95 * p$drange 
	)

	if (length(ibad)>0) {
		res$Vrange[ ibad] = NA
	  res$Vnugget[ ibad] = NA
		res$Vpsill[ ibad] = NA
		res$Vsse[ibad] = NA
	}
	save( res, file=fn, compress=T )
	return(fn)

}



variogram.estimate.geoR = function() {
	
	res = rep( NA, 11)

	iscat = which( scat$spec==sp )
		if (length( iscat) < 30 ) return(res)
		
		isset = which( sset$yr == yr & sset$id %in% unique( scat$id )  )
		if (length( isset) < 10 ) return(res)

		dat = merge( sset[isset, c("id", "plon", "plat")], scat[iscat,], by=c("id"), all.x=T, all.y=F, sort=F )
		# dat = dat[ which( is.finited(dat[,va])), ] 	 <<<--- two-stage model is better?	
		ina = which( !is.finite(dat[,va]))
		if ( (nrow(dat) - length(ina)) < 10) return(res)

		dat[ ina, va] = 0		
		
		Vvar =  var( dat[,va], na.rm=T )
		Vmean	= mean( dat[,va], na.rm=T )
		
		out= NULL
		dat = dat[, c("plon", "plat", va )]
		dat[, c("plon", "plat")] = jitterDupCoords( dat[, c("plon", "plat")] , max=1, min=0.25  )

		g = as.geodata( dat )

		vario0 = variog(g, uvec=p$boundaries, max.dist=p$drange, pairs.min=3, estimator.type= "classical")
		# plot( vario0)

		x = try( variofit( vario0, cov.model=p$variorgamtype, ini.cov.pars = c(Vvar/2, p$drange/4), nugget=Vvar/2, weights="equal"), silent=T )
		  if (! ("try-error" %in% class(x) )) out = rbind( out, c(x$nugget, x$cov.pars, x$practicalRange, x$value) )
			# lines(x)
		x = try( variofit( vario0, cov.model=p$variorgamtype, ini.cov.pars = c(Vvar/2,  p$drange/4), nugget=Vvar/2, weights="cressie"), silent=T)
		  if (! ("try-error" %in% class(x) )) out = rbind( out, c(x$nugget, x$cov.pars, x$practicalRange, x$value) )
		
		x = try( variofit( vario0, cov.model=p$variorgamtype, ini.cov.pars = c(Vvar/2, p$drange/4), nugget=Vvar/2, weights="npairs"), silent=T )
		  if (! ("try-error" %in% class(x) )) out = rbind( out, c(x$nugget, x$cov.pars, x$practicalRange, x$value) )
	

		colnames(out) = c( "tausq", "sigmasq", "phi", "range", "sse" ) 
		  # correlation function C(h) = sigma^2 * rho(h) , where e.g. for "exp":: rho(h) = exp(-h/phi)
			# sigmasq==partial sill
			# phi = range
			# tausq = nugget
			# effective range = practicalRange = range at wich correlation drops to 0.05

		Vest = apply( out, 2, weighted.mean, w=(1/out[,"sse"]))  # to use as starting values
		
		cov.params = as.vector( Vest[c("sigmasq", "phi") ])
		nugget =  as.vector( Vest["tausq"] )

		x0 = try (likfit( g, cov.model=p$variorgamtype, ini.cov.pars=cov.params, nugget=nugget, lik.method = "ML"), silent=T )
			if (! ("try-error" %in% class(x0) )) out = rbind( out, c(x0$nugget, x0$cov.pars, x0$practicalRange, x0$AIC) ) 
		
		x1 = try (likfit( g, cov.model=p$variorgamtype, ini.cov.pars=cov.params, nugget=nugget, lik.method = "REML"), silent=T )
		  if (! ("try-error" %in% class(x1) )) out = rbind( out, c(x1$nugget, x1$cov.pars, x1$practicalRange, x1$AIC) )
		
		if ( ("try-error" %in% class(x0) ) & ("try-error" %in% class(x1) ) ) Vest = Vest  # nothing to do
		if ( !("try-error" %in% class(x0) ) & ("try-error" %in% class(x1) ) ) Vest = c(x0$nugget, x0$cov.pars, x0$practicalRange, x0$AIC)
		if ( ("try-error" %in% class(x0) ) & !("try-error" %in% class(x1) ) ) Vest = c(x1$nugget, x1$cov.pars, x1$practicalRange, x1$AIC)
		if ( !("try-error" %in% class(x0) ) & !("try-error" %in% class(x1) ) ) {
			if (x0$AIC > x1$AIC ) {
				Vest = c(x1$nugget, x1$cov.pars, x1$practicalRange, x1$AIC)
			} else {
				Vest = c(x0$nugget, x0$cov.pars, x0$practicalRange, x0$AIC)
			}
		}	

		names(Vest) = colnames(out)

		Vtot = Vest["tausq"] + Vest["sigmasq"]
		Vrange = Vest["phi"] 
		Vnugget = Vest["tausq"] / Vtot  # %; assume max variance attained by the end of the distance series
		Vpsill = Vest["sigmasq"] / Vtot   # %; " " "
		Veffrange = Vest["range"]
		
		res = data.frame(cbind( sp, yr, Vmean, Vvar, Vrange, Veffrange, Vnugget, Vpsill, Vsse=Vest["sse"], Nall=nrow(dat), Nnonzero=nrow(dat)-length(ina)  )) 
    # g = gstat(g, id=va, model=f)
		# out = predict(object=g, newdata=newdata, block=c(pres,pres) )

		return(res)
}



simulate.and.plot.random.field = function(cp) { 

		nsims = length(cp)
			pl = layout( matrix( c(1:(nsims*2)), nrow=nsims, ncol=2, byrow=T ) )
			for ( i in 1:nsims ) {
				set.seed(1)
			  attach( cp[[i]]	 )
				s = grf(n=n, grid=grid, cov.model=mod, cov.pars=covp, nugget=nugget, mean=meanv)
				plot(s, ylim=vylim, xlab="", ylab="", cex=0.5 ) 
				text( 0, vylim[2]*.95, paste( mod, round(covp[1],2), round(covp[2],2), round(nugget,2), sep=": " ), cex=0.5, pos=4 )
				image(s, zlim=zlim, xlab="", ylab="" )
				detach()
			}

}


