
## http://www.meds-sdmm.dfo-mpo.gc.ca/biochemQuery/authenticate.do?errors=yes



source( file.path("C:","Users", "choij", "Documents", ".Rprofile"))

p = list()
p$libs = RLibrary( c( "lubridate", "lattice", "INLA", "sp", "RODBC" ) )
p$init.files = loadfunctions( c( "utility", "polygons", "spacetime", "bathymetry", "biochem" ) ) 

p$current.year = 2015

p$bottles.years = c( 1955:p$current.year )
p$zoop.years = c( 1990:p$current.year )

recreate.polygon5kmcoast=FALSE
if (recreate.polygon5kmcoast) {
  polygon5kmcoast(plotdata=TRUE)
}

bottles.db( DS="bottles.dump.odbc", p=p ) 
bottles.db( DS="bottles.odbc.all.redo", p=p ) 
bottles.db( DS="bottles.qa.qc.redo", p=p ) 


zoop.db( DS="zoop.data.dump.odbc", p=p ) 
zoop.db( DS="zoop.data.odbc.all.redo", p=p ) 
zoop.db( DS="zoop.qa.qc.redo", p=p )
zoop.db( DS="zoop.totalSamples.redo", p=p )
zoop.db( DS="zoop.speciesAbund.redo", p=p )






















# ---- 


    biochem.db( DS="odbc.datadump" ) 
    biochem.db( DS="flatten" ) 
    biochem.db( DS="scotian.shelf.redo" )
    

    # Biochem data analysis  .. focus on bottom oxygen

    # start data uptake and processing

    p = list()
    p$init.files = loadfunctions( c( "spatialmethods", "utility", "parallel", "bathymetry", "temperature", "biochem" ) ) 
    p$tyears = c(1950:2012)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
    p$newyear = newyear = c( 2012)


    # only one data stream necessary at present .. the largest extent
    p = spatial.parameters( p=p, type= "canada.east" )
    

    for ( j in c("SSE", "canada.east" ) ) {
        
      # ----------------
      # parameters 
      
      #   j = "SSE"
   
        p = spatial.parameters( p=p, type=j )
        p$clusters = rep("localhost",  1) # debug
        # p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
      
      # ----------------
      # grid bottom data    
        hydro.db( p=p, DS="bottom.gridded.redo" )
      
      # ----------------
      # this glues all the years together and returns the data
      t0 = hydro.db( p=p, DS="bottom.gridded.all"  ) 
          
      # ----------------
      # temporal interpolations assuming a sinusoidal seasonal pattern 
        p$clusters = rep("localhost",  24) # ~ 155 hours with 24 cpus and 1950:2012, ESS; 20 GB total
        # ?? p$clusters = c( rep("kaos.beowulf",20), rep("nyx.beowulf",20), rep("tartarus.beowulf",20) ) # speeded ??
        temperature.interpolations( p=p, DS="temporal.interpolation.redo" ) 
          # 1950-2012, SSE took +46 hrs  

      # ----------------
      # simple spatial interpolation (complex/kriging takes too much time/cpu) ==> 3-4 hr/run
      # temperature.interpolations( p=p, DS="spatial.interpolation.redo" ) 
      p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
      parallel.run( clusters=p$clusters, n=length(p$tyears), temperature.interpolations, p=p, DS="spatial.interpolation.redo" ) 
    
      # ----------------
      # extract relevant statistics
      # hydro.modelled.db(  p=p, DS="bottom.statistics.annual.redo" )
      # or parallel runs: ~ 1 to 2 GB / process
      # 4 cpu's ~ 10 min
      p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
      parallel.run( clusters=p$clusters, n=length(p$tyears),	hydro.modelled.db, p=p, DS="bottom.statistics.annual.redo" ) 

      # ----------------
      # climatology database 
      # 4 cpu's ~ 5 min
      bstats = c("tmean", "tamplitude", "wmin", "thalfperiod", "tsd" )
      # hydro.modelled.db(  p=p, DS="bottom.mean.redo", vname=bstats ) 
      p$clusters = rep( "nyx", length(bstats) )
      parallel.run( clusters=p$clusters, n=length(bstats), hydro.modelled.db, p=p, DS="bottom.mean.redo", vname=bstats  )  
   

      # glue climatological stats together
      temperature.db ( p=p, DS="climatology.redo") 
      
      # annual summary temperature statistics for all grid points --- used as the basic data level for interpolations 
      parallel.run( clusters=p$clusters, n=length(p$tyears), temperature.db, p=p, DS="complete.redo") 



      # ----------------
      # hydro.map( p=p, yr=p$tyears, type="annual" ) # or run parallel ;;; type="annual does all maps
      # hydro.map( p=p, yr=p$tyears, type="global" ) # or run parallel ;;; type="annual does all maps
      p$clusters = c( rep("kaos.beowulf",23), rep("nyx.beowulf",24), rep("tartarus.beowulf",24) )
      parallel.run( clusters=p$clusters, n=length(p$tyears), hydro.map, p=p, yr=p$tyears, type="annual"  ) 
      parallel.run( clusters=p$clusters, n=length(p$tyears), hydro.map, p=p, yr=p$tyears, type="global") 



    }



