

  # ----------------
  # Prep OSD, snow crab and groundfish temperature profiles    
  # this one has to be done manually .. 2008 data still pending,
  # temperature.profiles = hydro.db( DS="osd.rawdata.refresh" )  
  
  # from choijae; Jc#00390 :: (http://www.mar.dfo-mpo.gc.ca/science/ocean/database/data_query.html) 
  # depths: 500,500, "complete profile"   .. raw data  for the SS (region: jc.ss")
  # must download manually to this directory and run gzip

  # "canada.east" = Nafo2J + Nafo3K + Nafo3L + Nafo3N +  Nafo3O + Nafo4R + Nafo4S + Nafo4V + Nafo4W + Nafo4X + Nafo5Ze + Nafo5Zw
     

    # start data uptake and processing

    p = list()
    
    p$libs = loadlibraries( c( "chron", "gstat", "sp", "parallel", "mgcv", "bigmemory" ) )
    
    p$init.files = loadfunctions( c("common", "bathymetry", "temperature" ) ) 
		
    # faster to use RAM-based data objects but this forces use only of local cpu's
    # configure SHM (shared RAM memory to be >18 GB .. in fstab .. in windows not sure how to do this?)
    p$use.bigmemory.file.backing = FALSE  
    # p$use.bigmemory.file.backing = TRUE  # file-backing is slower but can use all cpu's in a distributed cluster

 
    # p$tyears = c(1910:2013)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
    p$tyears = c(1950:2013)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
    p$newyear = newyear = c( 2013)


    # only one data stream necessary at present .. the largest extent
    p = spatial.parameters( p=p, type= "canada.east" )

    p$wtimes = 1:52 
    p$nw = length(p$wtimes)
    p$ny = length(p$tyears)

    p$gam.optimizer = "bam" ## other optimizers:: "bam", "perf", "nlm", "bfgs", "optim", "newton", "nlm.fd"

    p$nMin.tbot = 400 # min number of data points req before attempting to model timeseries in a localized space.
    p$dist.km = c( 2, 4, 8, 10, 15, 20, 25, 30, 40, 50, 60, 80, 100, 120 ) / 2 # additional (1/2) distances to extend search for data
    p$depthrange.fraction = c( 2/3, 4/3 ) # -/+ 33% of actual depth to include in interpolation

    p$tsmethod ="harmonics"  # temporal interpolation method ... harmonic analysis seems most reasonable
    p$tsharmonics = 2  # highest harmonic to use ... will use lower if it fails .. do not use more than 3 as it chases noise too much .. 2 seems sufficient and faster
 

    p$clusters = rep("localhost", detectCores() )
  # p$clusters = rep("localhost",  1) # debug
  # p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
	



  # ----------------
  # data up-take for all of the "canada.east" only

    # ----------------
    # extract all hydro data and add snow crab and groundfish data
    # hydro.db( DS="osd.rawdata.allfiles.redo", p=p  )   # redo whole data set (historical) from 1910 to 2010
    # hydro.db( DS="osd.rawdata.singleyear.redo", yr=newyear, p=p ) # temp data not maintained any longer ???
    # hydro.db( DS="osd.oneoff.singleyear.redo", yr=2011, p=p ) 
    # .. not sure where data are being stored 
    # .. right now using data obtained directly from Roger Pettipas (OSD)  
      hydro.db( DS="osd.pettipas.redo", p=p, yr=c(2010:2013) ) 
 

    # ----------------
      p = make.list( list( yrs=p$tyears), Y=p )
      hydro.db( DS="profiles.annual.redo", yr=newyear, p=p  ) # can also choose all years: yr=p$tyears
    # parallel.run( hydro.db, p=p, yr=p$tyears, DS="profiles.annual.redo", init.files=p$init.files ) 

    # ----------------
    # extract bottom data
      p = make.list( list( yrs=p$tyears), Y=p )
      hydro.db( DS="bottom.annual.redo", yr=newyear, p=p )
    # hydro.db( DS="bottom.annual.redo", yr=p$tyears, p=p )
    # parallel.run( hydro.db, p=p, yr=p$tyears, DS="bottom.annual.redo", init.files=p$init.files ) 



  # ----------------
  # Basic data uptake now complete 
  # .. subset the data to smaller areas of interest
  # .. ie. area-specific divisions of data and gridding 
  # to optimize speed for snow crab /SSE only results 


    
    # update spatial parameters for the region of interest
    #  j = "canada.east" # can be completed later (after assessment) when time permits
    p = spatial.parameters( p=p, type="SSE" )
 	
    # ----------------
    # grid bottom data to internal spatial resolution ; <1 min  
    p = make.list( list( yrs=p$tyears), Y=p )
  	# parallel.run( hydro.db, p=p, DS="bottom.gridded.redo", yr=p$tyears )
		hydro.db( p=p, DS="bottom.gridded.redo", yr=p$tyears )
		

 		# ----------------
    # this glues all the years together; <1 min
    hydro.db( p=p, DS="bottom.gridded.all.redo", yr=p$tyears  ) 
   			
 		# ----------------
    # temporal interpolations assuming some seasonal pattern 
    # 1950-2013, SSE took ~ 40 hrs (shared RAM, 24 CPU) ... 17 GB req of shared memory
    # this is parallelized ... the call is internal to this 
    temperature.interpolations( p=p, DS="temporal.interpolation.redo" ) 


 		# ----------------
    # simple spatial interpolation (complex/kriging takes too much time/cpu) ==> 3-4 hr/run
    # temperature.interpolations( p=p, DS="spatial.interpolation.redo" ) 
    # p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
    p = make.list( list( yrs=p$tyears), Y=p )
    parallel.run( temperature.interpolations, p=p, DS="spatial.interpolation.redo" ) 
  

 		# ----------------
    # extract relevant statistics
    # hydro.modelled.db(  p=p, DS="bottom.statistics.annual.redo" )
    # or parallel runs: ~ 1 to 2 GB / process
    # 4 cpu's ~ 10 min
    # p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
    p = make.list( list( yrs=p$tyears), Y=p )
    parallel.run( hydro.modelled.db, p=p, DS="bottom.statistics.annual.redo" ) 

    # ----------------
    # climatology database 
    # 4 cpu's ~ 5 min
    bstats = c("tmean", "tamplitude", "wmin", "thalfperiod", "tsd" )
    # hydro.modelled.db(  p=p, DS="bottom.mean.redo", vname=bstats ) 
    p = make.list( list( vname=bstats), Y=p )
    parallel.run( hydro.modelled.db, p=p, DS="bottom.mean.redo", vname=bstats  )  
 

    # glue climatological stats together
    temperature.db ( p=p, DS="climatology.redo") 
    
    # annual summary temperature statistics for all grid points --- used as the basic data level for interpolations 
    p = make.list( list( yrs=p$tyears), Y=p )
    parallel.run( temperature.db, p=p, DS="complete.redo") 



    # ----------------
    # Maps 
    #
    # p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
        
    # hydro.map( p=p, yr=p$tyears, type="annual" ) # or run parallel ;;; type="annual does all maps
    p = make.list( list( yrs=p$tyears), Y=p )
    parallel.run( hydro.map, p=p, type="annual"  ) 
    
    # hydro.map( p=p, yr=p$tyears, type="global" ) # or run parallel ;;; type="annual does all maps
    p = make.list( list( yrs=p$tyears), Y=p )
    parallel.run( hydro.map, p=p, type="global") 




debug = FALSE

if (debug) { 

# ----------------
# to access data:
        p = spatial.parameters( p=p, type="SSE" )
        tp = hydro.db(  p=p, DS="profiles.annual", yr=2007 )
        tp = hydro.db(  p=p, DS="bottom.annual", yr=2007 )


        
# ----------------
#  test analysis spatial variations in temperature

        require( gstat )
        p = spatial.parameters( p=p, type="SSE" )
        
        O = hydro.db( p=p, DS="bottom.gridded" )
        O = O[, c("plon", "plat", "t", "yr", "weekno")]
        O = O[ which( is.finite(O$t)) ,]

        testyear = 2006
        i = which(O$yr == testyear & O$weekno %in% testweeks ) 

        ee.g = gstat( id = "t", formula=t~plon+plat, locations=~plon+plat, data=O[i,] ) 
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
        O = O[, c("plon", "plat", "t", "yr", "weekno")]
        O = O[ which( is.finite(O$t)) ,]

       
        testyears = c(1950:2007)
        testyears = sort( unique(O$yr) )
         
        i = which( O$yr %in% testyears ) 
        # i = sample( i, floor(length(i) * 0.1 )) 

        O = O[ i, ]

        e = gam( t ~ s(yr, weekno) + s(plon, plat) , data=O[i,] )



       
        e = gam( t ~ s(yr, ttime) + s(plon, plat) , data=O )
        e = lme( t ~ yr + ttime  + plon+plat, data=O, random=list(yr=~1 ) )
        e = lme( t ~ yr + ttime  + plon+plat, data=O, random=list(yr=~1,ttime=~1 ) )
        e = gamm( t~ s(yr) + ttime + s(plon,plat), data=O, random=list(plonplat=~1 ) )

        e = gamm( t~ s(yr,weekno) + s(plon,plat), data=O, random=list(plon=~1, plat=~1), correlation=corSpher( c(range, nugg), form = ~ plon+plat, nugget=T) )
        
        
        summary(e)
        AIC(e)
        plot(e, all.terms=T, pers=T,theta=60)



        # ---------------------
        # data extraction for Katja Fennel



        # start data uptake and processing

        p = list()
        p$init.files = loadfunctions( c("common", "bathymetry", "temperature" ) ) 
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
        

        # ----------------
     

}




       
