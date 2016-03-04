
  # --------------------------
  # 0. Initialise work space 

	loadfunctions( "snowcrab", functionname="initialise.local.environment.r") 
  
  # --------------------------
  # 1. Define some additional starting parameters for debugging
  #    choose various over-rides: these are initially defined in parameters.r
  
  p$regions = c("cfa4x", "cfanorth","cfasouth" )
  p$vars.to.model = c("R0.mass")
  # p$vars.to.model = c("R0.mass",  "R1.no")
  # p$vars.to.model = c("R0.mass", "R0.no", "R1.no", "totno.female.primiparous","totno.female.multiparous", "totno.female.berried", "fecundity","totno.female.imm", "totno.male.imm" )  
  # p$vars.to.model = c("R0.no", "R1.no", "totno.female.primiparous","totno.female.multiparous", "totno.female.berried", "fecundity","totno.female.imm", "totno.male.imm" )  
  p$years.to.model=1996:2015


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
  habitat.model.db( DS="large.male.auxillary.data.redo", p=p )

    # Define controlling parameters 
    p$auxilliary.data = c( 
      "t", "tmean", "tmean.cl", "tamp", "wmin", "z", "substrate.mean", "dZ", "ddZ"
    )
      # "ca1", "ca2", 
      # "nss.rsquared", "nss.shannon", 
      # "smr", "Ea", "A", "qm", "mass",
      # "Z", "Npred" ) 

    print( "Make sure variable list here matches those in ecomod/habitat/src/habitat.complete.r ") 
    print( "and in the model statement in ecomod/snowcrab/_Rfunctions/analysis/model.formula.r")

    # p$model.type = "gam.full" # choose method for habitat model :
    # p$model.type = "gam.simple" # choose method for habitat model :
    
    p$habitat.threshold.quantile = 0.05 # quantile at which to consider zero-valued abundance
    p$threshold.distance = 15  # limit to extrapolation/interpolation in km ( over-ride default is 5)
   
      # ---------------------
      # model habitat and intermediate predictions
      # ~ 1 MB / process  .. use all 24 CPUs
      # Parameterize habitat space models for various classes, 
      # see: habitat.model.db( DS="habitat.redo" ... )
      
      p$clusters = rep("localhost", detectCores() )
      p = make.list( list(v=p$vars.to.model), Y=p )
      # parallel.run( habitat.model.db, DS="habitat.redo", p=p )  
      habitat.model.db( DS="habitat.redo", p=p, yr=p$years.to.model )  # 10 hrs 

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
      p = make.list( list(v=p$vars.to.model), Y=p )
      #parallel.run( habitat.model.db, DS="abundance.redo", p=p )
      habitat.model.db( DS="abundance.redo", p=p)   # 1.5 hr for yrs: 2000:2015
      
      
      # ---------------------
      # compute posterior simulated estimates using habitat and abundance predictions 
      # and then map, stored in R/gam/maps/

      p$vars.to.model= "R0.mass"
      p$nsims = 4000 # n=2000 ~ 1 , 15 GB/run for sims 
      p$ItemsToMap = c( "map.habitat", "map.abundance", "map.abundance.estimation" )

      # p$clusters = c( rep( "nyx.beowulf",3), rep("tartarus.beowulf",3), rep("kaos", 3 ) )
      
      p$clusters = c( rep( "localhost", 2) )
      p = make.list( list(y=p$years.to.model, v=p$vars.to.model ), Y=p )
      parallel.run( interpolation.db, DS="interpolation.redo", p=p )
			# interpolation.db ( DS="interpolation.redo", p=p )  ~ 30 min for 1 yr

      # collect all results into a single file and return: 
      K = interpolation.db( DS="interpolation.simulation", p=p  ) 
      table.view( K ) 
 
      abund.v = c("yr", "total", "lbound", "ubound" )

      Pmeta = K[ which( K$region=="cfanorth") ,]
      Pmeta = K[ which( K$region=="cfasouth") ,]
      Pmeta = K[ which( K$region=="cfa4x") ,]
      Pmeta = K[ which( K$region=="cfaall") ,]
     
      outdir = file.path( project.datadirectory("snowcrab"), "R", "gam", "timeseries")
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
#      set$weekno = floor(set$julian / 365 * 52) + 1
#      set$dyear = floor(set$julian / 365 ) + 1
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
				PS$dyear = p$prediction.dyear  # must be same as above
				PS$t = NA
      
        PST = temperature.db( p=p, DS="spatial.interpolation", yr=y  )
				if (is.null(PST)) next ()
				
        dyears = (c(1:(p$nw+1))-1)  / p$nw # intervals of decimal years... fractional year breaks
        dyr = as.numeric( cut( p$prediction.dyear, breaks=dyears, include.lowest=T, ordered_result=TRUE ) ) # integer representation of season
        PS$t = PST[, dyr ]
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
  


  
