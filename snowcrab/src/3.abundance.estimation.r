
  # --------------------------
  # 0. Initialise work space 
  p= list()

	p$init.files = loadfunctions( "snowcrab", functionname="initialise.local.environment.r") 
  p$libs = RLibrary( "mgcv", "chron", "lattice", "lattice", "grid", "fields", "parallel"  ) 


  # --------------------------
  # 1. Define some additional starting parameters for debugging
  #    choose various over-rides: these are initially defined in parameters.r
  
  p$regions = c("cfa4x", "cfanorth","cfasouth" )
  
  p$vars.to.model = c("R0.mass")
  # p$vars.to.model = c("R0.mass",  "R1.no")
  # p$vars.to.model = c("R0.mass", "R0.no", "R1.no", "totno.female.primiparous","totno.female.multiparous", "totno.female.berried", "fecundity","totno.female.imm", "totno.male.imm" )  
  # p$vars.to.model = c("R0.no", "R1.no", "totno.female.primiparous","totno.female.multiparous", "totno.female.berried", "fecundity","totno.female.imm", "totno.male.imm" )  
  p$years.to.model=2000:2014

  
  debug = F
  if (debug) {
    # identify areas to interpolate
      p$regions = c("cfanorth")
      p$regions = c("cfa4x", "cfanorth","cfasouth" )
      p$regions = c("cfa4x", "cfanorth","cfasouth", "cfaall" )
    
    # identify variables to interpolate and create abundance estimates
      
      p$vars.to.model = c("R0.no", "R1.no")
      p$vars.to.model = c("R0.mass",  "R1.no")
      p$vars.to.model = c("R1.no")
      p$vars.to.model = c("R0.mass")

      p$vars.to.model = c("male.large.mass", "male.small.mass", "female.large.mass", "female.small.mass" )
      
      p$vars.to.model = c("R0.mass", "R0.no", "R1.no", "totno.female.primiparous","totno.female.multiparous", "totno.female.berried", "fecundity","totno.female.imm", "totno.male.imm" ) 
      
      p$vars.to.model = c("R0.mass", "R0.no", "R1.no", "R2.no", "R3.no", "R4.no", "R5p.no", 
        "totno.female.berried", "totno.female.imm", "totno.female.mat", "totno.female.primiparous","totno.female.multiparous",
        "fecundity", "totno.male.com", "totno.male.mat", "totno.male.imm","dwarf.no", "totno.male.skip.moulter", "totno.male.com.CC5" 
      )

    # modify cluster requirements
      # p$do.parallel =F
      p$clusters = rep("localhost", detectCores() )
      # p$clusters = rep( "localhost", 2)  # only need 2 if 2 vars are being modelled
      # p$clusters = rep( "localhost", 8)  
      # p$clusters = rep( "localhost", 24)  
      # p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("kaos", 24 ) )
       
      # p$clusters = rep("tethys", 7 )
      # p$clusters = rep("kaos",23 )
      # p$clusters = rep("nyx",24 )
      # p$clusters = rep("tartarus",24 )
  }


  # -----------------
  # 2. Abundance estimation via GAM   

    # create some intermediary files to speed up analysis

  #  habitat.model.db( DS="large.male.auxillary.data.redo", p=p )


 #   if (abundance.estimation.via.GAM) {
      
      # Define controlling parameters 
      p$auxilliary.data = c( 
            "t", "tmean", "tmean.cl", "tamp", "wmin", 
            "z", "substrate.mean", "dZ", "ddZ", 
            "ca1", "ca2", 
            "nss.rsquared", "nss.shannon", 
            "smr", "Ea", "A", "qm", "mass",
            "Z", "Npred" ) 



      print( "Make sure variable list here matches those in ecomod/habitat/src/habitat.complete.r ") 
      print( "and in the model statement in ecomod/snowcrab/_Rfunctions/analysis/model.formula.r")

      p$model.type = "gam.full" # choose method for habitat model :
      p$habitat.threshold.quantile = 0.05 # quantile at which to consider zero-valued abundance
      p$optimizers = c( "bam", "perf","nlm",   "bfgs", "newton", "Nelder-Mead" )  # used by GAM
			p$prediction.weekno = 39 # predict for ~ Sept 1 
      p$threshold.distance = 15  # limit to extrapolation/interpolation in km
     
      p$use.annual.models = F  ## <<<<< new addition
      p$movingdatawindow = 0 # c( -1:+1 )  # this is the range in years to supplement data to model 
      p$movingdatawindowyears = length (p$movingdatawindow)



      # ---------------------
      # model habitat and intermediate predictions
      # ~ 1 MB / process  .. use all 24 CPUs
      # Parameterize habitat space models for various classes, 
      # see: habitat.model.db( DS="habitat.redo" ... )
      
      p$clusters = rep("localhost", detectCores() )
  


    moving.window=F
      if(moving.window) p = make.list( list(v=p$vars.to.model, yrs=p$years.to.model  ), Y=p )
      if(!moving.window)p = make.list( list(v=p$vars.to.model  ), Y=p )
  
      parallel.run( habitat.model.db, DS="habitat.redo", p=p )  
      # habitat.model.db( DS="habitat.redo", p=p, yr=p$years.to.model )   
      #  --- parallel mode is not completing ... FIXME

      # ---------------------
      testing.environmentals.only = FALSE
      if ( testing.environmentals.only ) {
        # habitat surface area estimation for R0.mass from 1970 to present --- for timeseries and direct prediction maps
        p$clusters = c( rep( "localhost", 20) )
        # p$clusters = rep( "localhost", 2)  
        p = make.list( list(v=c("R0.mass.environmentals.only", "R0.mass") ), Y=p )
          habitat.model.db (DS="habitat.redo", p=p ) 
        p = make.list( list(y=1970:p$current.assessment.year, v=c("R0.mass.environmentals.only", "R0.mass") ), Y=p )
          parallel.run( snowcrab.habitat.db, p=p ) 
        # or
        # snowcrab.habitat.db (p=p) -- working?    
      }      


      # ---------------------
      # model abundance and intermediate predictions 
      moving.window=F
      if(moving.window) p = make.list( list(v=p$vars.to.model, yrs=p$years.to.model  ), Y=p )
      if(!moving.window)p = make.list( list(v=p$vars.to.model  ), Y=p )
      parallel.run( habitat.model.db, DS="abundance.redo", p=p )
      # habitat.model.db( DS="abundance.redo", p=p) 
      
      
      # ---------------------
      # compute posterior simulated estimates using habitat and abundance predictions 
      # and then map, stored in R/gam/maps/

      p$vars.to.model= "R0.mass"
      p$nsims = 2000 # n=1000 ~ 1 , 15 GB/run for sims 
      p$ItemsToMap = c( "map.habitat", "map.abundance", "map.abundance.estimation" )

      # p$clusters = c( rep( "nyx.beowulf",3), rep("tartarus.beowulf",3), rep("kaos", 3 ) )
      
      p$clusters = c( rep( "localhost", 2) )
      p = make.list( list(y=p$years.to.model, v=p$vars.to.model ), Y=p )
      parallel.run( interpolation.db, DS="interpolation.redo", p=p )
			# interpolation.db ( DS="interpolation.redo", p=p )

      # collect all results into a single file and return: 
      K = interpolation.db( DS="interpolation.simulation", p=p  ) 
      table.view( K ) 
 
      abund.v = c("yr", "total", "lbound", "ubound" )

      Pmeta = K[ which( K$region=="cfanorth") ,]
      Pmeta = K[ which( K$region=="cfasouth") ,]
      Pmeta = K[ which( K$region=="cfa4x") ,]
      Pmeta = K[ which( K$region=="cfaall") ,]
     
      outdir = file.path( project.directory("snowcrab"), "R", "gam", "timeseries")
      rr = "cfanorth"
      rr = "cfasouth"
      rr = "cfa4x"
      rr = "cfaall"

      vv = "R0.mass"
      Pmeta = K[ which( as.character(K$vars)==vv & as.character(K$region)==rr ) ,abund.v ]
      figure.timeseries.errorbars( Pmeta, outdir=outdir, fname=paste(vv, rr, sep=".") )
      
      # TODO :: MUST determine potential habitat stats in historical data ( 1950 to present ) for timeseries plots
      # use "prediction.surface( p, DS="annual.redo", vclass=vclass )" as an example

      ### --------- prediction success:
      
      set = snowcrab.db( DS="set.logbook" )
      set = set[ set$yr %in% p$years.to.model ,]
      set$total.landings.scaled = scale( set$total.landings, center=T, scale=T )
      set = presence.absence( set, "R0.mass", p$habitat.threshold.quantile )  # determine presence absence(Y) and weighting(wt)
      set$weekno = floor(set$julian / 365 * 52) + 1
      set$dt.seasonal = set$tmean -  set$t 
      set$dt.annual = set$tmean - set$tmean.cl

      H = habitat.model.db( DS="habitat", p=p, v="R0.mass" )
      set$predicted.pa = predict( H, set, type="response" )
      A = habitat.model.db( DS="abundance", p=p, v="R0.mass" )
      set$predicted.abund = predict( A, set, type="response" )

      set$predicted.R0.mass = set$predicted.abund
      set$predicted.R0.mass[ which( set$predicted.pa < 0.5) ] = 0

      amin = min( set$predicted.R0.mass[ which( set$predicted.R0.mass >0 )])
      bmin = min( set$R0.mass[ which( set$R0.mass >0 )])

      plot( set$predicted.R0.mass , set$R0.mass )
      
      cor( set$predicted.R0.mass , set$R0.mass )^2  # 0.7870114 !!
      
      save ( set, file="/home/adam/tmp/set.test.rdata")
      
    }



    ---


    y = 2012
    v = "R0.mass"

        PS = habitat.db ( DS="complete", year=y, p=p )
				PS$weekno = p$prediction.weekno  # must be same as above
				PS$t = NA
      
        PST = temperature.interpolations( p=p, DS="spatial.interpolation", yr=y  )
				if (is.null(PST)) next ()
				
        PS$t = PST[, p$prediction.weekno ]
        PS$t[ which(PS$t < -2) ] = -2
			  PS$t[ which(PS$t > 30) ] = 30 

        iitna = which( ! is.finite( PS$t ) ) 
        if (length(iitna)>0) PS$t[iitna] = PS$tmean[iitna]

        PS$z = log(PS$z)
        PS$dt.seasonal = PS$tmean - PS$t 
        PS$dt.annual = PS$tmean - PS$tmean.cl
        PS$sa = 1

				# posterior simulations
				Hmodel = habitat.model.db( DS="habitat", v= v )
      
        PS$predicted.pa = predict( Hmodel, PS, type="response" )
      	
        levelplot( predicted.pa ~ plon + plat, data=PS, aspect="iso", title=paste("Habitat", y) )
       

        Amodel = habitat.model.db( DS="abundance", v= v )
      
        PS$abundance = predict( Amodel, PS, type="response" )
      	
        levelplot( log10(abundance) ~ plon + plat, data=PS, aspect="iso", title=paste("Abundance", y)  )
       

        present = which(PS$predicted.pa > p$habitat.threshold.quantile )
        PS$pa = 0
        PS$pa[present] = 1
        
        PS$combined = PS$abundance * PS$pa
        levelplot( log10(combined) ~ plon + plat, data=PS, aspect="iso", title=paste("Abundance", y)  )
  


        ----

        2013/2014

Family: binomial 
Link function: logit 

Formula:
Y ~ s(weekno, k = 4, bs = "ts") + s(tmean) + s(dt.annual, k = 4, 
    bs = "ts") + s(dt.seasonal, k = 4, bs = "ts") + s(tamp, k = 4, 
    bs = "ts") + s(wmin, k = 4, bs = "ts") + s(z) + s(dZ, k = 4, 
    bs = "ts") + s(substrate.mean, k = 4, bs = "ts") + s(ca1, 
    k = 4, bs = "ts") + s(ca2, k = 4, bs = "ts") + s(Npred, k = 4, 
    bs = "ts") + s(Z, k = 4, bs = "ts") + s(smr, k = 4, bs = "ts") + 
    s(mr, k = 4, bs = "ts") + s(plon, plat, k = 200, bs = "tp", 
    by = as.factor(yr)) + as.factor(yr)
<environment: 0x8fbd1f0>

Parametric coefficients:
                  Estimate Std. Error z value Pr(>|z|)  
(Intercept)        -1.0678     0.4507  -2.369   0.0178 *
as.factor(yr)2002  -0.9966     0.7196  -1.385   0.1660  
as.factor(yr)2003  -1.2596     0.6377  -1.975   0.0482 *
as.factor(yr)2004  -1.1337     0.5792  -1.957   0.0503 .
as.factor(yr)2005  -1.0483     0.5441  -1.927   0.0540 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                                     edf Ref.df Chi.sq  p-value    
s(weekno)                      1.603e+00   3.00 91.303  < 2e-16 ***
s(tmean)                       1.075e+00   1.14 20.962 9.30e-06 ***
s(dt.annual)                   5.710e-01   3.00  1.254 0.121554    
s(dt.seasonal)                 1.080e+00   3.00 12.454 0.000165 ***
s(tamp)                        6.907e-01   3.00  2.105 0.069745 .  
s(wmin)                        3.588e-01   3.00  0.552 0.204238    
s(z)                           1.000e+00   1.00 57.995 2.64e-14 ***
s(dZ)                          5.565e-01   3.00  1.175 0.130584    
s(substrate.mean)              1.059e+00   3.00  9.248 0.000639 ***
s(ca1)                         1.359e+00   3.00 23.341 2.27e-09 ***
s(ca2)                         2.500e+00   3.00 49.076 4.01e-13 ***
s(Npred)                       2.008e-05   3.00  0.000 0.279641    
s(Z)                           4.071e-06   3.00  0.000 1.000000    
s(smr)                         1.046e+00   3.00  6.066 0.002184 ** 
s(mr)                          6.653e-01   3.00  1.965 0.068155 .  
s(plon,plat):as.factor(yr)2001 1.102e+01  15.84 27.562 0.033695 *  
s(plon,plat):as.factor(yr)2002 1.669e+01  23.85 58.390 0.000101 ***
s(plon,plat):as.factor(yr)2003 9.801e+00  13.96 32.669 0.003163 ** 
s(plon,plat):as.factor(yr)2004 1.184e+01  16.83 43.117 0.000429 ***
s(plon,plat):as.factor(yr)2005 1.480e+01  20.95 60.791 9.52e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.594   Deviance explained = 60.5%
fREML score = 3268.7  Scale est. = 1         n = 2689
[1] "/home/jae/ecomod/snowcrab/R/gam/models/habitat/habitat.R0.mass.2003.rdata"
[1] "2014-02-20 11:12:28 AST"
        v  yrs
2 R0.mass 2004
Note: no visible binding for global variable 'wt' 
Note: no visible binding for global variable 'wt' 
Note: no visible binding for global variable 'wt' 
Note: no visible binding for global variable 'wgts' 
Note: no visible binding for global variable 'wts' 
Note: no visible binding for global variable 'wgts' 
Note: no visible binding for global variable 'wgts' 
Note: no visible binding for global variable 'RODBC' 
Note: no visible global function definition for 'odbcConnect' 
Note: no visible global function definition for 'sqlQuery' 
Note: no visible global function definition for 'odbcClose' 
Note: no visible binding for global variable 'RODBC' 
Note: no visible global function definition for 'odbcConnect' 
Note: no visible global function definition for 'sqlQuery' 
Note: no visible global function definition for 'odbcClose' 
Note: no visible binding for global variable 'RODBC' 
Note: no visible global function definition for 'odbcConnect' 
Note: no visible global function definition for 'sqlQuery' 
Note: no visible global function definition for 'odbcClose' 
Note: no visible binding for global variable 'wt' 
Note: no visible binding for global variable 'wt' 
Note: no visible binding for global variable 'wt' 
Note: no visible binding for global variable 'wgts' 
Note: no visible binding for global variable 'wts' 
Note: no visible binding for global variable 'wgts' 
Note: no visible binding for global variable 'wgts' 
[1] "bam"
[1] "2014-02-20 11:12:36 AST"
[1] "/home/jae/ecomod/snowcrab/R/gam/models/habitat/habitat.R0.mass.2004.rdata"

Family: binomial 
Link function: logit 

Formula:
Y ~ s(weekno, k = 4, bs = "ts") + s(tmean) + s(dt.annual, k = 4, 
    bs = "ts") + s(dt.seasonal, k = 4, bs = "ts") + s(tamp, k = 4, 
    bs = "ts") + s(wmin, k = 4, bs = "ts") + s(z) + s(dZ, k = 4, 
    bs = "ts") + s(substrate.mean, k = 4, bs = "ts") + s(ca1, 
    k = 4, bs = "ts") + s(ca2, k = 4, bs = "ts") + s(Npred, k = 4, 
    bs = "ts") + s(Z, k = 4, bs = "ts") + s(smr, k = 4, bs = "ts") + 
    s(mr, k = 4, bs = "ts") + s(plon, plat, k = 200, bs = "tp", 
    by = as.factor(yr)) + as.factor(yr)
<environment: 0x6d250f8>

Parametric coefficients:
                  Estimate Std. Error z value Pr(>|z|)   
(Intercept)        -1.9768     0.6845  -2.888  0.00388 **
as.factor(yr)2003  -0.6659     0.8862  -0.751  0.45243   
as.factor(yr)2004  -0.5856     0.7492  -0.782  0.43443   
as.factor(yr)2005  -0.3575     0.7276  -0.491  0.62318   
as.factor(yr)2006  -0.1288     0.7212  -0.179  0.85829   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                                     edf Ref.df Chi.sq  p-value    
s(weekno)                      1.592e+00  3.000 99.669  < 2e-16 ***
s(tmean)                       1.551e+00  1.915 32.949 9.04e-08 ***
s(dt.annual)                   1.075e+00  3.000  9.283 0.000938 ***
s(dt.seasonal)                 9.989e-01  3.000  8.412 0.001483 ** 
s(tamp)                        1.417e-05  3.000  0.000 1.000000    
s(wmin)                        4.750e-06  3.000  0.000 0.617437    
s(z)                           2.889e+00  3.678 48.167 1.09e-09 ***
s(dZ)                          8.056e-01  3.000  2.998 0.041322 *  
s(substrate.mean)              1.407e+00  3.000 19.376 1.39e-06 ***
s(ca1)                         1.114e+00  3.000  9.646 0.000202 ***
s(ca2)                         1.714e+00  3.000 21.226 9.94e-07 ***
s(Npred)                       4.186e-06  3.000  0.000 0.920731    
s(Z)                           5.232e-01  3.000  1.046 0.106785    
s(smr)                         1.300e+00  3.000 11.512 6.08e-05 ***
s(mr)                          9.718e-06  3.000  0.000 0.470363    
s(plon,plat):as.factor(yr)2002 1.794e+01 25.575 58.462 0.000226 ***
s(plon,plat):as.factor(yr)2003 1.206e+01 17.129 40.561 0.001157 ** 
s(plon,plat):as.factor(yr)2004 1.012e+01 14.349 33.097 0.003371 ** 
s(plon,plat):as.factor(yr)2005 1.232e+01 17.430 43.881 0.000458 ***
s(plon,plat):as.factor(yr)2006 1.354e+01 19.221 60.495 3.81e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.594   Deviance explained = 61.1%
fREML score = 3349.4  Scale est. = 1         n = 2760
[1] "/home/jae/ecomod/snowcrab/R/gam/models/habitat/habitat.R0.mass.2004.rdata"
[1] "2014-02-20 11:35:21 AST"
        v  yrs
3 R0.mass 2005
[1] "bam"
[1] "2014-02-20 11:35:25 AST"
[1] "/home/jae/ecomod/snowcrab/R/gam/models/habitat/habitat.R0.mass.2005.rdata"

Family: binomial 
Link function: logit 

Formula:
Y ~ s(weekno, k = 4, bs = "ts") + s(tmean) + s(dt.annual, k = 4, 
    bs = "ts") + s(dt.seasonal, k = 4, bs = "ts") + s(tamp, k = 4, 
    bs = "ts") + s(wmin, k = 4, bs = "ts") + s(z) + s(dZ, k = 4, 
    bs = "ts") + s(substrate.mean, k = 4, bs = "ts") + s(ca1, 
    k = 4, bs = "ts") + s(ca2, k = 4, bs = "ts") + s(Npred, k = 4, 
    bs = "ts") + s(Z, k = 4, bs = "ts") + s(smr, k = 4, bs = "ts") + 
    s(mr, k = 4, bs = "ts") + s(plon, plat, k = 200, bs = "tp", 
    by = as.factor(yr)) + as.factor(yr)
<environment: 0xddd01a0>

Parametric coefficients:
                  Estimate Std. Error z value Pr(>|z|)   
(Intercept)       -2.16059    0.69769  -3.097  0.00196 **
as.factor(yr)2004 -0.68593    0.74608  -0.919  0.35790   
as.factor(yr)2005 -0.21682    0.74050  -0.293  0.76968   
as.factor(yr)2006 -0.09753    0.75304  -0.130  0.89695   
as.factor(yr)2007 -0.52505    0.74143  -0.708  0.47884   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                                     edf Ref.df Chi.sq  p-value    
s(weekno)                      2.748e+00  3.000 98.731  < 2e-16 ***
s(tmean)                       1.000e+00  1.001 35.708 2.31e-09 ***
s(dt.annual)                   1.116e+00  3.000 10.642 0.000429 ***
s(dt.seasonal)                 1.111e+00  3.000 17.170 9.53e-06 ***
s(tamp)                        4.276e-06  3.000  0.000 0.775505    
s(wmin)                        9.923e-06  3.000  0.000 0.451487    
s(z)                           1.951e+00  2.475 56.454 5.55e-12 ***
s(dZ)                          5.188e-01  3.000  1.009 0.148110    
s(substrate.mean)              1.370e+00  3.000 18.880 2.09e-06 ***
s(ca1)                         1.010e+00  3.000  5.873 0.003159 ** 
s(ca2)                         1.665e+00  3.000 29.092 8.03e-09 ***
s(Npred)                       5.357e-06  3.000  0.000 1.000000    
s(Z)                           7.340e-01  3.000  2.328 0.041996 *  
s(smr)                         1.254e+00  3.000 17.413 8.21e-07 ***
s(mr)                          3.578e-01  3.000  0.556 0.181064    
s(plon,plat):as.factor(yr)2003 1.379e+01 19.587 58.297 1.07e-05 ***
s(plon,plat):as.factor(yr)2004 9.599e+00 13.603 28.588 0.010052 *  
s(plon,plat):as.factor(yr)2005 1.351e+01 19.147 46.988 0.000390 ***
s(plon,plat):as.factor(yr)2006 1.312e+01 18.620 59.858 3.39e-06 ***
s(plon,plat):as.factor(yr)2007 1.105e+01 15.610 55.183 2.73e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.574   Deviance explained = 59.6%
fREML score = 3171.4  Scale est. = 1         n = 2651


