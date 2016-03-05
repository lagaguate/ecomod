
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
      # p$vars.to.model = c("R1.no")
      # p$vars.to.model = c("R0.mass")
      # p$vars.to.model = c("male.large.mass", "male.small.mass", "female.large.mass", "female.small.mass" )
      # p$vars.to.model = c("R0.mass", "R0.no", "R1.no", "totno.female.primiparous","totno.female.multiparous", "totno.female.berried", "fecundity","totno.female.imm", "totno.male.imm" ) 
      # p$vars.to.model = c("R0.mass", "R0.no", "R1.no", "R2.no", "R3.no", "R4.no", "R5p.no", 
      #  "totno.female.berried", "totno.female.imm", "totno.female.mat", "totno.female.primiparous","totno.female.multiparous",
      #  "fecundity", "totno.male.com", "totno.male.mat", "totno.male.imm","dwarf.no", "totno.male.skip.moulter", "totno.male.com.CC5" 
      # )

    # over-ride cluster requirements
      n = detectCores()
      # n = 2
      p$clusters = rep("localhost", n )
      # p$clusters = c( rep( "nyx", n), rep("tartarus", n), rep("kaos", n) )
  }


  # -----------------
  # 2. Abundance estimation via GAM   

  # create some intermediary files to speed up analysis
    habitat.model.db( DS="large.male.auxillary.data.redo", p=p )

    # covariates in model as dtermined by "model.formula( p$vars.to.model )"   
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
    p$nsims = 4000 # n=2000 ~ 1 , 15 GB/run for sims 
    p$ItemsToMap = c( "map.habitat", "map.abundance", "map.abundance.estimation" )

  
      # ---------------------
      # model habitat and intermediate predictions
      # ~ 1 MB / process  .. use all 24 CPUs
      # Parameterize habitat space models for various classes, 
      # see: habitat.model.db( DS="habitat.redo" ... )
      
      p = make.list( list(v=p$vars.to.model), Y=p )
      # parallel.run( habitat.model.db, DS="habitat.redo", p=p )  
      habitat.model.db( DS="habitat.redo", p=p, yr=p$years.to.model )  # 10 hrs 

      # ---------------------
      testing.environmentals.only = FALSE
      if ( testing.environmentals.only ) {
        # habitat surface area estimation for R0.mass from 1970 to present --- for timeseries and direct prediction maps
        p = make.list( list(v=c("R0.mass.environmentals.only", "R0.mass") ), Y=p )
        habitat.model.db (DS="habitat.redo", p=p ) 
  
        p = make.list( list(y=1970:p$current.assessment.year, v=c("R0.mass.environmentals.only", "R0.mass") ), Y=p )
        # parallel.run( snowcrab.habitat.db, p=p ) 
        snowcrab.habitat.db (p=p) 
      }      


      # ---------------------
      # model abundance and intermediate predictions 
      p = make.list( list(v=p$vars.to.model), Y=p )
      #parallel.run( habitat.model.db, DS="abundance.redo", p=p )
      habitat.model.db( DS="abundance.redo", p=p)   # 1.5 hr for yrs: 2000:2015
      
      
      # ---------------------
      # compute posterior simulated estimates using habitat and abundance predictions 
      # and then map, stored in R/gam/maps/
      # p$vars.to.model= "R0.mass"
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


      ### --------- examine prediction success:
      set = snowcrab.db( DS="set.logbook" )
      set = set[ set$yr %in% p$years.to.model ,]
      set$total.landings.scaled = scale( set$total.landings, center=T, scale=T )
      set = presence.absence( set, "R0.mass", p$habitat.threshold.quantile )  # determine presence absence(Y) and weighting(wt)
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
      

