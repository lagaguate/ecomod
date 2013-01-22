
  require(chron)
  require(gstat)
  require(snow)

	env.init = loadfunctions( c("common", "bathymetry", "temperature" ) ) 
 
# ----------------
# define basic parameters  ... years need to be updated as appropriate
  p$tyears = c(1950:2012)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
  newyear = c( 2012)



  NOTE :: SSE seems to contain more data than the canada.east ! check this with OSD people why
"canada.east" =
Nafo2J
Nafo3K
Nafo3L
Nafo3N
Nafo3O
Nafo4R
Nafo4S
Nafo4V
Nafo4W
Nafo4X
Nafo5Ze
Nafo5Zw
  
  # ----------------
  # Prep OSD, snow crab and groundfish temperature profiles    
  # this one has to be done manually .. 2008 data still pending,
  # temperature.profiles = hydro.db( DS="osd.rawdata.refresh" )  
  
  # from choijae; Jc#00390 :: (http://www.mar.dfo-mpo.gc.ca/science/ocean/database/data_query.html) 
  # depths: 500,500, "complete profile"   .. raw data  for the SS (region: jc.ss")
  # must download manually to this directory and run gzip


  baseleveldata = TRUE

    if (baseleveldata) {

      j = "canada.east"  # only one data stream necessary at present .. the largest extent
      p = spatial.parameters( type=j )
      p$env.init = env.init
      
      # ----------------
      # extract all hydro data and add snow crab and groundfish data
        hydro.db( DS="osd.rawdata.singleyear.redo", yr=newyear, p=p ) 
      # hydro.db( DS="osd.oneoff.singleyear.redo", yr=2011, p=p ) 
      # hydro.db( DS="osd.rawdata.allfiles.redo", p=p  )   # redo whole data set (historical)
      # hydro.db( DS="osd.rawdata.all.redo", yr=p$tyears, p=p )  
   
      # ----------------
        hydro.db( DS="profiles.annual.redo", yr=newyear, p=p  ) # can also choose all years: yr=p$tyears
      # or if in parallel mode: 
      # parallel.run( clusters=rep("localhost",23), n=length(p$tyears), p=p, FUNC=hydro.db, yr=p$tyears, DS="profiles.annual.redo", env.init=env.init ) 

      # ----------------
      # extract bottom data
        hydro.db( DS="bottom.annual.redo", yr=newyear, p=p )
      # hydro.db( DS="bottom.annual.redo", yr=p$tyears )
      # parallel.run( clusters=rep("localhost",16), n=length(p$tyears), FUNC=hydro.db, yr=p$tyears, p=p,  DS="bottom.annual.redo", env.init=env.init ) 

  }




  # Now do area-specific divisions to optimize speed for snow crab /SSE only results 
  # ... canada.east can be completed when time permits
  #   j = "SSE"

  for ( j in c("SSE", "canada.east" ) ) {
      
		# ----------------
    # parameters 
      p = spatial.parameters( type=j )
			p$env.init = env.init
		  # p$clusters = rep("localhost",  4) # debug
      # p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
		
 		# ----------------
    # grid bottom data    
			hydro.db( p=p, DS="bottom.gridded.redo", yr=p$tyears )
		
 		# ----------------
    # this glues all the years together
      hydro.db( p=p, DS="bottom.gridded.all.redo", yr=p$tyears  ) 
   			
 		# ----------------
    # temporal interpolations assuming a sinusoidal seasonal pattern 
    # operates in parallel mode only taking ~ 4 days for four processors
    # going across computers using a backing file and is slow and prone to overloading socket connections
    # but, cannot use RAM as bigmemory does not allocate RAM > 20GB ? 
    # It is faster to use RAM but this limits the no of CPUS that can be used to that found on a single machine:
      uselocalonly = TRUE
      if ( uselocalonly ) {
        p$clusters = rep("localhost",  20) # debug
        temperature.interpolations( p=p, DS="temporal.interpolation.redo.RAM") 
      } else {
        temperature.interpolations( p=p, DS="temporal.interpolation.redo" ) 
      }

 		# ----------------
    # simple spatial interpolation (complex takes too much time/cpu)
    # in parallel mode with 4 cpus ~ 30 minutes
    # hydro.db( p=p, DS="spatial.interpolation.redo", yr=p$tyears ) 
    p$clusters = c( rep("kaos.beowulf",20), rep("nyx.beowulf",20), rep("tartarus.beowulf",20) )
    parallel.run( clusters=p$clusters, n=length(p$tyears), 	hydro.db, p=p, DS="spatial.interpolation.redo", yr=p$tyears ) 
  
 		# ----------------
    # extract relevant statistics
    # hydro.modelled.db(  p=p, DS="bottom.statistics.annual.redo" )
    # or parallel runs: ~ 1 to 2 GB / process
    # 4 cpu's ~ 10 min
    p$clusters = c( rep("kaos.beowulf",20), rep("nyx.beowulf",20), rep("tartarus.beowulf",20) )
    parallel.run( clusters=p$clusters, n=length(p$tyears), 	hydro.modelled.db, p=p, DS="bottom.statistics.annual.redo" ) 

    # ----------------
    # annual means of key statistics
    # 4 cpu's ~ 5 min
    bstats = c("tmean", "tamplitude", "wmin", "thalfperiod", "tsd" )
    # hydro.modelled.db(  p=p, DS="bottom.mean.redo", vname=bstats ) 
    p$clusters = rep( "nyx", length(bstats) )
    parallel.run( clusters=p$clusters, n=length(bstats), hydro.modelled.db, p=p, DS="bottom.mean.redo", vname=bstats  )  

    # ----------------
    # hydro.map( p=p, yr=p$tyears, type="annual" ) # or run parallel ;;; type="annual does all maps
    # hydro.map( p=p, yr=p$tyears, type="global" ) # or run parallel ;;; type="annual does all maps
    p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
    parallel.run( clusters=p$clusters, n=length(p$tyears), hydro.map, p=p, yr=p$tyears, type="annual"  ) 
    parallel.run( clusters=p$clusters, n=length(p$tyears), hydro.map, p=p, yr=p$tyears, type="global") 

  }



# ----------------
# to access data:
    p = spatial.parameters( type="SSE" )
    tp = hydro.db(  p=p, DS="profiles.annual", yr=2007 )
    tp = hydro.db(  p=p, DS="bottom.annual", yr=2007 )


    
# ----------------
#  test analysis spatial variations in temperature

    require( gstat )
    p = spatial.parameters( type="SSE" )
    
    O = hydro.db( p=p, DS="bottom.gridded"  )
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
    p = spatial.parameters( type="SSE" )
 
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


   
