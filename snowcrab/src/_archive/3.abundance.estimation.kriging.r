
  # --------------------------
  # 0. Initialise work space 

  required.libraries = c( "mgcv", "chron", "lattice"  )
  for ( L in required.libraries) require( L, character.only=T )
  source( file.path( project.directory("snowcrab"), "src", "initialise.local.environment.r" ) )



  # --------------------------
  # 1. Define some additional starting parameters for debugging
  #    choose various over-rides: these are initially defined in parameters.r

  debug = F
  if (debug) {
    # identify areas to interpolate
      p$regions = c("cfanorth")
      p$regions = c("cfa4x", "cfanorth","cfasouth" )
      p$regions = c("cfa4x", "cfanorth","cfasouth", "cfaall" )
    
    # identify variables to interpolate and create abundance estimates
      
      p$vars.to.model = c( "R0.no", "R1.no")
      p$vars.to.model = c("R0.mass",  "R1a.no")
      p$vars.to.model = c("R0.mass")
      p$vars.to.model = c("R0.mass", "male.large.mass", "male.small.mass", "female.large.mass", "female.small.mass" )
      p$vars.to.model = c("R0.mass", "R0.no", "R1.mass", "R1.no", "R2.no", "R3.no", "R4.no", "R5p.no", "totmass.male.com.CC1",
        "totno.female.berried","totno.female.imm", "totno.female.mat", "totno.female.primiparous","totno.female.multiparous",
        "fecundity", "totno.male.com", "totno.male.mat", "totno.male.imm","dwarf.no", "totno.male.skip.moulter", 
        "totno.male.com.CC1to2",  "totno.male.com.CC5",  "male.large.mass", "male.small.mass", "female.large.mass", "female.small.mass", "male.large.np", "male.small.no", "female.large.no", "female.small.no"  )

    # modify cluster requirements
      p$do.parallel =F
      p$clusters = rep("localhost",8)
      # p$clusters = rep("tethys", 7 )
      # p$clusters = rep("kaos",23 )
      # p$clusters = rep("nyx",24 )
      # p$clusters = rep("tartarus",24 )
  }



  #-----------------------------------------------------
  # 3. KRIGING estimation of abundance and mapping
      
    # First, predict habitat surface locations: takes about 2hr for 15 yrs .. 
    # snowcrab.habitat.db.old ( DS="habitat.redo", model.type=p$model.type, vclass=vclass, pyears=p$yearswithTdata, env.init=init.files, p=p ) 
    # or 
         
      p$habitat.threshold.quantile = 0.05  # quantile at which to consider zero-valued abundance
      p$habitat.templates =  "R0.mass"  
      p$model.type = "gam.full" # choose method for habitat model : 
  
      model.habitat( model.type="gam.full.redo", p=p, plotdata=F ) # only a single habitat type is used with this method

      parallel.run( clusters=p$clusters, n=length(p$yearswithTdata), snowcrab.habitat.db.old, 
        DS="habitat.redo",   model.type=p$model.type, vclass=vclass, pyears=p$yearswithTdata,  env.init=init.files, p=p ) 


      # V prepare prediction surface data sets for kriging analysis and GAM
      # snowcrab.habitat.db.old( DS="PS.redo", p=p, yrs=p$years.to.model,  model.type=p$model.type, vclass=vclass) 
      # or 
      parallel.run( clusters=p$clusters, n=length(p$years.to.model), snowcrab.habitat.db.old, DS="PS.redo", p=p, 
        yrs=p$years.to.model, model.type=p$model.type, vclass=vclass, env.init=init.files) 
    

			# determine potential habitat stats in historical data ( 1950 to present ) for timeseries plots
      # --- is this necessary ??
      prediction.surface( p, DS="annual.redo", vclass=vclass )

      
      # debug
      # x = snowcrab.habitat.db.old(DS="habitat", model.type=p$model.type, vclass=vclass, pyears=yr )
      # x11(); levelplot( x$fit ~ plon+plat, habitat.db( DS="baseline", p=p), aspect="iso" )
   
      if ( point.kriging) { 
        # this is to point krige and then map results 

        p$kriging.type =  "point.kriging"
        p$use.local.variograms = F
        p$plot.variogram = F  # set to true only if master thread is on the same computer as the parallel threads
      
        p$knmax = 40
        p$vgm.dist = p$vgm.dist [ which( p$vgm.dist <= 100 ) ]
        p$kriging.distance.buffer = 10  # use additional bounding data to this extent to predict 
        p$kriging.extrapolation.limit = 10  # limit extrap to within x km of a surevey station
          

        # define compact list of variable year combinations for parallel processing
        p = make.list( list(y=p$years.to.model, r="cfaall", v=p$vars.to.model ), Y=p )

        # kriging.db( DS="UK.redo", p=p, env.init=init.files  ) 
        # or
        parallel.run( clusters=p$clusters, n=p$nruns, kriging.db, DS="UK.redo", p=p, env.init=init.files  ) 
        
        # the following is completed within the kriging.db but can be re-run this way: 
        # map.krige.lattice( M=p, env.init=init.files )
        # parallel.run( clusters=p$clusters, n=p$nruns, map.krige.lattice, M=p, env.init=init.files )

        # test plot 
        # PS = kriging.db( DS="UK.point.PS", p=list(v="R0.mass", y=p$current.assessment.year, r="cfanorth")  ) 
        # levelplot( R0.mass.pred ~ plon+plat, PS, aspect="iso")
        # 
        # plot with GMT 
        # krige.map ( p, init.files=init.files  )  # ~ 1 day?
        # gmt.cleanup() # clean up any stragglers

      }

    
      if ( abundance.estimation.via.kriging ) {
      
        p$kriging.type = "block.conditional.sims" 
        p$use.local.variograms = T 
        p$n.conditional.sims = 150
         
        p$knmax = 40
        p$vgm.dist = p$vgm.dist [ which( p$vgm.dist <= 100 ) ]
        p$kriging.distance.buffer = 10  # use additional bounding data to this extent to predict 
        p$kriging.extrapolation.limit = 10  # limit extrap to within x km of a surevey station
     
        debug.4X = F
        if (debug.4X) {
          problem.years = c(2006, 2009 )
          p$years.to.model = setdiff( c(2004:p$current.assessment.year), problem.years)   # highly problematic years
          p$years.to.model = c(2006, 2008, 2009)  # highly problematic years
          p$regions = "cfa4x"
          p$n.conditional.sims = 150
          p$vgm.dist =  p$vgm.dist [ which( p$vgm.dist <= 80 ) ]
          p$use.local.variograms = F 
          p$knmax = 40
          p$kriging.distance.buffer = 10  # use additional bounding data to this extent to predict 
          p$kriging.extrapolation.limit = 10  # limit extrap to within x km of a survey station
          p$vars.to.model = "R0.mass"
        }
        
 
        # define compact list of variable year combinations for parallel processing
        p = make.list( list(y=p$years.to.model, r=p$regions, v=p$vars.to.model ), Y=p )
    
        # kriging.db( DS="UK.redo", p=p )  
        # kriging.db( DS="UK.redo", p=p, overwrite.threshold=NULL ) # overwrite all data files  
        # kriging.db( DS="UK.redo", p=p, overwrite.threshold=-1 ) # complete only if no data files exist 
        # kriging.db( DS="UK.redo", p=p, overwrite.threshold=100 ) # overwrite if over a certain number of days  
        parallel.run( clusters=p$clusters, n=p$nruns, kriging.db, DS="UK.redo", p=p, env.init=init.files, overwrite.threshold=NULL  )  
        # parallel.run( clusters=p$clusters, n=p$nruns, kriging.db, DS="UK.redo", p=p, env.init=init.files, overwrite.threshold=200  )  # ~ 0.4 GB / process (GSTAT)
        # ~ 0.4 GB / process (GSTAT);; 
        # overwrite.threshold is # days required to trigger an over-write
        # overwrite.threshold=NULL  is overwrite all

        
        # collect all results into a single file and return: 
        K = kriging.db( DS="UK.conditional.simulation.K", p=p  ) 
        table.view( K )      
    
        if ( exists( "compare.density.with.biomass" ) ) {

          s = snowcrab.db( DS ="set.complete" )
          s1 = as.data.frame.table ( tapply( s$R0.mass, list(cfa=s$cfa, yr=s$yr ), mean, na.rm=T ) )
          names(s1) = c("cfa", "yr", "R0.mass.mean.density" )
          K = kriging.db( DS="UK.conditional.simulation.K", p=p  ) 
          o = K[ which(K$vars=="R0.mass"),]
          o1 = as.data.frame.table ( tapply( o$total, list(cfa=o$region, yr=o$yr ), mean, na.rm=T ) )
          names(o1) = c("cfa", "yr", "R0.mass.total.kriged" )
          o2 = as.data.frame.table ( tapply( o$surfacearea, list(cfa=o$region, yr=o$yr ), mean, na.rm=T ) )
          names(o2) = c("cfa", "yr", "sa" )
          o3 = merge( o1, o2, by=c("cfa", "yr") )
          o4 = merge( o3, s1, by=c("cfa", "yr") )
          sa = as.data.frame.table( tapply( o4$sa, o4$cfa, mean, na.rm=T) )
          names( sa ) = c("cfa", "sa.mean" )
          o = merge( o4, sa, by="cfa" )
          o$R0.mass.total.fixed = o$R0.mass.mean.density * o$sa.mean 
          o$R0.mass.total.variable = o$R0.mass.mean.density * o$sa 
          plot( o$R0.mass.total.variable, o$R0.mass.total.fixed, pch=19, xlab="Abundance (variable area)", ylab="Abundance (fixed area)" )
         #  text(  o$R0.mass.total.variable, o$R0.mass.total.fixed, o$yr, pos=4)
          abline(0,1)
          Pr("png", dname="~/", fname="tmp")
          hist(o$R0.mass.total.fixed/o$R0.mass.total.variable)

        }

        xyplot( total /1000~ yr | region, K, subset=which(K$vars=="R0.mass"), type="b", scales=list(y="free") ) 
        xyplot( total /1000~ yr | region, K, subset=which(K$vars=="R1.no"), type="b", scales=list(y="free") )
        xyplot( total /1000~ yr | region, K, subset=which(K$vars=="fecundity"), type="b", scales=list(y="free") )

        # ------------------------------------------
        # plot( total ~ yr, K, subset=which(region=="cfanorth" & vars=="R0.mass" ), type="b" )
        y=list(y=1998, r="cfanorth", v="R0.mass", transgaussian.kriging=T, nruns=1)  # order is important
        # GLS simulation predictions
        PS = kriging.db( DS="UK.conditional.simulation.PS", p=y, env.init=init.files  ) 
        levelplot( pred.mean~ plon+plat, PS, aspect="iso")
   
     } 




