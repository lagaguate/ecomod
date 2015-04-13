


  ### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db"
  

  p = list()
  p$libs = RLibrary ( c("chron", "fields", "mgcv", "sp", "parallel")) 

	p$init.files = loadfunctions( c(
    "spacetime", "utility", "parallel", 
    "bathymetry", "temperature", "substrate", "habitat", "taxonomy", "bio", "speciescomposition"  ) )
  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$data.sources = c("groundfish", "snowcrab") 
  
  p$taxa = "maxresolved"
  p$season = "allseasons"
  p$timescale = c( 0,1,2,5,10 ) # yr
  p$interpolation.distances =  25 # for interpolation of habitat vars

  # choose:
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep( "localhost", 2 )
  # p$clusters = rep( "localhost", 8 )
  # p$clusters = rep( "localhost", 24 )
  # p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("kaos.beowulf", 24 ) )
  # p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("kaos", 24 ) )
  p$clusters = rep("localhost", detectCores() )

 
  p$yearstomodel = 1970:2014
  p$varstomodel = c( "ca1", "ca2", "pca1", "pca2" )

  p$modtype = "complex"

  p$habitat.predict.time.julian = "Sept-1" # Sept 1
  
  p$spatial.knots = 100
  
  p$movingdatawindow = 0  # this signifies no moving window ... all in one model
  # p$movingdatawindow = c( -4:+4 )  # this is the range in years to supplement data to model 
  p$movingdatawindowyears = length (p$movingdatawindow)

  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is bam, then this .. see GAM options



  # ordination
  speciescomposition.db( DS="speciescomposition.ordination.redo", p=p )
  speciescomposition.db( DS="speciescomposition.redo", p=p )
	


# -------------------------------------------------------------------------------------
# Generic spatio-temporal interpolations and maping of data 
# using the interpolating functions and models defined in ~ecomod/habitat/src/
# -------------------------------------------------------------------------------------

  #required for interpolations and mapping 
  p$project.name = "speciescomposition"
  p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )


 
  if ( p$movingdatawindow == 0 ) { 
    ## NO windowing ... full model

    # create a spatial interpolation model for each variable of interest 
    # full model requires 30-40 GB ! 
    p = make.list( list(vars= p$varstomodel ), Y=p )  # no moving window 
    parallel.run( habitat.model, DS="redo", p=p ) 
    # habitat.model ( DS="redo", p=p ) 
 
    
    # predictive interpolation to full domain (iteratively expanding spatial extent)
    # ~ 5 GB /process required so on a 64 GB machine = 64/5 = 12 processes 

    p = make.list( list(vars= p$varstomodel ), Y=p )  # no moving window 
    parallel.run( habitat.interpolate, p=p, DS="redo" ) 
    # habitat.interpolate( p=p, DS="redo" ) 

  } else {
    ## Windowing approach
    p = make.list( list(vars= p$varstomodel, yrs=p$yearstomodel ), Y=p ) 
    parallel.run( habitat.model, DS="redo", p=p ) 
    # habitat.model ( DS="redo", p=p ) 
  
    # predictive interpolation to full domain (iteratively expanding spatial extent)
    # ~ 5 GB /process required so on a 64 GB machine = 64/5 = 12 processes 

    p = make.list( list(yrs=p$yearstomodel ), Y=p ) 
    parallel.run( habitat.interpolate, p=p, DS="redo" ) 
    # habitat.interpolate( p=p, DS="redo" ) 
 
  }


  # map everything
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p  ) 
  # habitat.map( p=p  ) 








# -------------------------------------------------------------------------------------


  testing = FALSE
  if ( testing)  {

      ca = speciescomposition.db(DS="ca", p=p)
      ca$variance

      pca =  speciescomposition.db(DS="pca", p=p)
      
      toplot = pca$cscores
      spec = as.numeric( as.character( rownames( toplot )))
      rownames(toplot) = taxonomy.recode( from="spec", tolookup=spec)

      plot.ordination (X=toplot)


       
# spatial autocorrelation
      require(gstat)
      drange = 150
      nmax = 200
      SC = speciescomposition.db( DS="speciescomposition.merged", p=p )
      sc = SC[ SC$yr==2011,]
      g = gstat(id="sc", formula=pca1~1, loc=~plon+plat, data=sc, maxdist=drange, nmax=nmax )
      g.e  = variogram(g, cutoff=drange, cressie=T)
      plot(g.v)


# spBayes attempt
      require( spBayes)
      o = spLM( pca1~1, coords=as.matrix(sc[,c("plon","plat")]), data=sc,
        knots=c(6,6,0),
        starting=list("phi"=0.6,"sigma.sq"=1, "tau.sq"=1),
        sp.tuning=list("phi"=0.01, "sigma.sq"=0.01, "tau.sq"=0.01),
        priors=list("phi.Unif"=c(0.3, 3), "sigma.sq.IG"=c(2, 1),
                        "tau.sq.IG"=c(2, 1)),
        cov.model="exponential",
        n.samples=1000, verbose=TRUE)

         print(summary(o$p.samples))
         plot(o$p.samples)
         
         ##Requires MBA package to
         ##make surfaces
         library(MBA)
         par(mfrow=c(1,2))
         obs.surf <-
           mba.surf(cbind(coords, w), no.X=100, no.Y=100, extend=TRUE)$xyz.est
         image(obs.surf, xaxs = "r", yaxs = "r", main="Observed response")
         points(coords)
         contour(obs.surf, add=T)
         
         w.hat <- rowMeans(o$sp.effects)
         w.surf <-
           mba.surf(cbind(coords, w.hat), no.X=100, no.Y=100, extend=TRUE)$xyz.est
         image(w.surf, xaxs = "r", yaxs = "r", main="Estimated random effects")
         contour(w.surf, add=T)
         points(coords, pch=1, cex=1)
         points(o$knot.coords, pch=19, cex=1)
         legend(1.5,2.5, legend=c("Obs.", "Knots"), pch=c(1,19), bg="white")
         
      





# GAM analysis


    Family: gaussian 
    Link function: identity 

    Formula:
    ca1 ~ s(plon, plat, k = 400) + s(z, k = 3) + s(t, k = 3) + s(yr) + 
        s(julian, k = 3)

    Parametric coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  0.04727    0.00344    13.8   <2e-16 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

    Approximate significance of smooth terms:
                    edf Ref.df      F p-value    
    s(plon,plat) 280.90 344.55   27.4  <2e-16 ***
    s(z)           1.00   1.00   89.9  <2e-16 ***
    s(t)           1.59   1.83  282.4  <2e-16 ***
    s(yr)          8.97   9.00 1611.3  <2e-16 ***
    s(julian)      1.99   2.00  382.1  <2e-16 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

    R-sq.(adj) =  0.795   Deviance explained = 79.8%
    GCV score = 0.21988  Scale est. = 0.21633   n = 18320
    > 


    Family: gaussian 
    Link function: identity 

    Formula:
    ca2 ~ s(plon, plat, k = 400) + s(z, k = 3) + s(t, k = 3) + s(yr) + 
        s(julian, k = 3)

    Parametric coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  0.09189    0.00309    29.8   <2e-16 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

    Approximate significance of smooth terms:
                    edf Ref.df      F p-value    
    s(plon,plat) 342.60 384.87   22.2  <2e-16 ***
    s(z)           1.80   1.95 3056.5  <2e-16 ***
    s(t)           1.00   1.00  239.2  <2e-16 ***
    s(yr)          8.86   8.99  466.4  <2e-16 ***
    s(julian)      2.00   2.00  506.5  <2e-16 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

    R-sq.(adj) =  0.824   Deviance explained = 82.8%
    GCV score = 0.17794  Scale est. = 0.17447   n = 18320

}


