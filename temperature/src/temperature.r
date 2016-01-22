
  # ----------------
  # Prep OSD, snow crab and groundfish temperature profiles    
  # this one has to be done manually .. no longer mainted by anyone ..

  p = list( project.name = "temperature" )
  p$project.root = project.datadirectory( p$project.name )

  p$libs = RLibrary( c( "chron", "gstat", "sp", "rgdal", "parallel", "mgcv", "bigmemory", "fields" ) )
  p$init.files = loadfunctions( c( "spacetime", "parallel", "utility", "bathymetry", "polygons" , "temperature" ) ) 

  # p$tyears = c(1910:2013)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
  p$tyears = c(1970:2015)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
  p$mon = 1:12 
  p$nw = length(p$mon)
  p$ny = length(p$tyears)
  p$gam.optimizer = "nlm" ## other optimizers:: "bam" (slow), "perf"(ok), "nlm" (good), "bfgs" (ok), "newton" (default)
  p$nMin.tbot = p$ny*2 # min number of data points req before attempting to model timeseries in a localized space 
  p$dist.km = c( 2.5, 5, 7.5, 10, 15 ) # "manhattan" distances to extend search for data
  p$maxdist = 20 # if using gstat  max dist to interpolate in space
  # choose: temporal interpolation method ... harmonic analysis seems most reasonable
  # .. do not use more than 2 as it chases noise too much .. 1 harmonic seems the best in terms of not chasing after noise 
  # possible methods: "annual", "seasonal.basic", "seasonal.smoothed", "harmonics.1", "harmonics.2", "harmonics.3", "inla.ts.simple"
  p$tsmethod = "harmonics.1"  
  # p$spmethod = "inverse.distance"  ## too slow
  # p$spmethod = "gam" ## too smooth
  p$spmethod = "kernel.density" ## best
  p$theta = 10 # dist to interpolate ~ 1/2 autocor range in method  p$spmethod = "kernel.density
  p$nsd = 5 # number of SD distances to pad boundaries with 0 for FFT  in method  p$spmethod = "kernel.density

  p$newyear = 2015


  # ------------------------------

  if ( create.baseline.database ) {
    # data up-take for all of the "canada.east" only one data stream necessary at present 
    p = spatial.parameters( p=p, type="canada.east" )

    if (historical.data.redo) {
      hydro.db( DS="osd.rawdata.allfiles.redo", p=p )   # redo whole data set (historical) from 1910 to 2010
      hydro.db( DS="osd.initial", p=p ) # 2008:2014 
    }
    # Roger Petipas has been maintaining a database, the following loads this data
    hydro.db( DS="osd.current", p=p, yr=2014:p$newyear ) # specify range or specific year 

    # Merge depth profiles from all data streams: OSD, groundfish, snowcrab
    p = make.list( list( yrs=c(2008:p$newyear), Y=p ))   # specify range or specific year
    p$clusters = rep("localhost", detectCores() )  # run only on local cores ... file swapping seem to reduce ep = make.list( list( yrs=c(2008:p$newyear), Y=p ))   # specify range or specific year
    p$current.assessment.year = p$newyear # required to access groundfish and snow crab data
    hydro.db( DS="profiles.annual.redo", yr=c(2008:p$newyear), p=p  )  # specify range or specific year
    # parallel.run( hydro.db, p=p, yr=p$tyears, DS="profiles.annual.redo", init.files=p$init.files ) 

    # Extract bottom data from each profile
    p = make.list( list( yrs=2008:p$newyear), Y=p )  # specify range or specific year
    hydro.db( DS="bottom.annual.redo", yr=2008:p$newyear, p=p ) # yr argument overrides p$tyears .. e.g. for a new year of data
    # hydro.db( DS="bottom.annual.redo", p=p ) 
    # parallel.run( hydro.db, p=p, yr=p$tyears, DS="bottom.annual.redo", init.files=p$init.files ) 
  }

  # ------------------------------
  # Basic data uptake now complete  .. move to interpolations
  # ------------------------------
 
  if (create.interpolated.results ) { 
    # to optimize speed for snow crab /SSE only results 
    # p = spatial.parameters( p=p, type="canada.east" ) #  can be completed later (after assessment) when time permits if required
    p = spatial.parameters( p=p, type="SSE" ) #  type="canada.east"  can be completed later (after assessment) when time permits if required
 
    # 1. grid bottom data to internal spatial resolution ; <1 min  
    p = make.list( list( yrs=p$tyears), Y=p )
    # parallel.run( hydro.db, p=p, DS="bottom.gridded.redo" )
    hydro.db( p=p, DS="bottom.gridded.redo" )  # all p$tyears, for a single year use with yr argument: yr=p$newyear
    hydro.db( p=p, DS="bottom.gridded.all.redo" )  # all p$tyears, for a single year use with yr argument: yr=p$newyear
   

    # 2. temporal interpolations assuming some seasonal pattern 
    # 1950-2013, SSE took ~ 35 hrs on laptop (shared RAM, 24 CPU; 1950-2013 run April 2014 ) ... 17 GB req of shared memory
    # 1950-2015, SSE 22 hrs, 42 GB RAM, 8 CPU on hyperion (10 Jan 2015), using BAM
    # define output mattrix
    # predictions are made upon the locations defined by bathymetry "baseline" 
    p$nP = nrow( bathymetry.db( p=p, DS="baseline" ) )
    p = temperature.db(p=p, DS="bigmemory.initiate" ) # return pointers to bigmemory objects 
    
      # test to make sure attach is possible .. 
      # sometimes requires restart of R or even computer .. bigmemory memory leak or file caching
      tbot <- bigmemory::attach.big.matrix( p$descriptorfile.tbot  )
      tbot.se <- bigmemory::attach.big.matrix( p$descriptorfile.tbotse  )

    p$clusters = rep("localhost", detectCores() )  # run only on local cores ... file swapping seem to reduce efficiency using the beowulf network
    # p = make.list( list( loc=sample.int( p$nP) ), Y=p ) # random order helps use all cpus
    p = make.list( list( loc=sample.int( p$nP ) ), Y=p ) # random order helps use all cpus
    # temperature.timeseries.interpolate ( p=p )
    parallel.run( temperature.timeseries.interpolate, p=p)
    temperature.db( p=p, DS="temporal.interpolation.redo" ) # save interpolation as time slices to disk 


    # 3. simple spatial interpolation (complex/kriging takes too much time/cpu) ==> 3-4 hr/run
    # temperature.db( p=p, DS="spatial.interpolation.redo" ) 
    # using localhost in 2014 6+ hr for each run but with multiple cycles ~ 10 hr total 
    # use all clusters if available
    p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
    p$clusters = rep("localhost", detectCores() ) 
    p = make.list( list( yrs=p$tyears), Y=p )
    parallel.run( temperature.db, p=p, DS="spatial.interpolation.redo" ) 


    # 4. extract relevant statistics
    # hydro.modelled.db(  p=p, DS="bottom.statistics.annual.redo" )
    # or parallel runs: ~ 1 to 2 GB / process
    # 4 cpu's ~ 10 min
    p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
    p = make.list( list( yrs=p$tyears), Y=p )
    parallel.run( hydro.modelled.db, p=p, DS="bottom.statistics.annual.redo" ) 


    # 5. climatology database 
    # 4 cpu's ~ 5 min
    p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
    bstats = c("tmean", "tamplitude", "wmin", "thalfperiod", "tsd" )
    # hydro.modelled.db(  p=p, DS="bottom.mean.redo", vname=bstats ) 
    p = make.list( list( vname=bstats), Y=p )
    parallel.run( hydro.modelled.db, p=p, DS="bottom.mean.redo", vname=bstats  )  
 

    # 6. glue climatological stats together
    temperature.db ( p=p, DS="climatology.redo") 
    

    # 7. annual summary temperature statistics for all grid points --- used as the basic data level for interpolations 
    p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
    p = make.list( list( yrs=p$tyears), Y=p )
    parallel.run( temperature.db, p=p, DS="complete.redo") 


    # 8. Maps 
    p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
    # hydro.map( p=p, yr=p$tyears, type="annual" ) # or run parallel ;;; type="annual does all maps
    p = make.list( list( yrs=p$tyears), Y=p )
    parallel.run( hydro.map, p=p, type="annual"  ) 
    # hydro.map( p=p, yr=p$tyears, type="global" ) # or run parallel ;;; type="annual does all maps
    p = make.list( list( yrs=p$tyears), Y=p )
    parallel.run( hydro.map, p=p, type="global") 
  }

  # finished interpolations


  #-----------------------------------------
  # the following a just for testing / notes
  
  if (0) { 
    # to access data:
    p = spatial.parameters( p=p, type="SSE" )
    tp = hydro.db(  p=p, DS="profiles.annual", yr=2007 )
    tp = hydro.db(  p=p, DS="bottom.annual", yr=2007 )


    #  test analysis spatial variations in temperature
    require( gstat )
    p = spatial.parameters( p=p, type="SSE" )
    testyear = 2006
    O = hydro.db( p=p, DS="bottom.gridded", yr=testyear )
    O = O[, c("plon", "plat", "t", "yr", "mon")]
    O = O[ which( is.finite(O$t)) ,]
    ee.g = gstat( id = "t", formula=t~plon+plat, locations=~plon+plat, data=O ) 
    vg <- variogram( ee.g, boundaries=c(2, 4, 8, 16, 20, 32, 40, 64, 80, 128, 150 ) )
    vm0 = vgm( psill=max(vg$gamma) * 0.75, model="Mat", range=max(vg$dist)*0.5, nugget=max(vg$gamma) * 0.25, kappa = 2 ) #initial settings
    vg.fit = fit.variogram( vg, vm0 )
    plot( vg, model=vg.fit, plot.numbers=T )
    # running various models suggest on dates with good data :
    #   range: about 40 km
    #   nugget: 1.5
    #   partial sill: 3.7
    

    # ----------------
    #  test analysis GAMM ... localised


    # ----------------
    #  test analysis GAMM ... global 

    require( mgcv )
    p = spatial.parameters( p=p, type="SSE" )
 
    O = hydro.db( p=p, DS="bottom.gridded.all"  )
    O = O[, c("plon", "plat", "t", "yr", "mon")]
    O = O[ which( is.finite(O$t)) ,]

   
    testyears = c(1950:2007)
    testyears = sort( unique(O$yr) )
     
    i = which( O$yr %in% testyears ) 
    # i = sample( i, floor(length(i) * 0.1 )) 

    O = O[ i, ]

    e = gam( t ~ s(yr, mon) + s(plon, plat) , data=O[i,] )
    e = gam( t ~ s(yr, ttime) + s(plon, plat) , data=O )
    e = lme( t ~ yr + ttime  + plon+plat, data=O, random=list(yr=~1 ) )
    e = lme( t ~ yr + ttime  + plon+plat, data=O, random=list(yr=~1,ttime=~1 ) )
    e = gamm( t~ s(yr) + ttime + s(plon,plat), data=O, random=list(plonplat=~1 ) )

    e = gamm( t~ s(yr,mon) + s(plon,plat), data=O, random=list(plon=~1, plat=~1), correlation=corSpher( c(range, nugg), form = ~ plon+plat, nugget=T) )
    summary(e)
    AIC(e)
    plot(e, all.terms=T, pers=T,theta=60)
  }


  if (0) {
    # data extraction for Katja Fennel
    p = list()
    p$init.files = loadfunctions( c("spacetime", "utility", "parallel", "bathymetry", "temperature" ) ) 
    p$tyears = c(1950:2012)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
    p = spatial.parameters( p=p, type= "SSE" )
    out = NULL
    out = hydro.modelled.db( p=p, DS="bottom.statistics.annual", yr=p$tyears[1] )
    out = planar2lonlat( out, proj.type=p$internal.projection )
    out = out[ , c("lon", "lat", "tmean" ) ]
    names(out) = c( "lon", "lat", paste("tmean", p$tyears[1],sep="_") ) 
    for ( y in p$tyears[-1] ) {
      r = NULL 
      r = hydro.modelled.db( p=p, DS="bottom.statistics.annual", yr=y )
      names(r) = paste( names(r), y, sep="_") 
      iname = grep( "tmean", names(r) )
      out = cbind( out, r[,iname] )
    }
    names(out) = c( "lon", "lat", paste( "tmean", p$tyears, sep="_") ) 
    library(R.matlab)
    writeMat("tdata.mat", tdata=out )
  }




         
