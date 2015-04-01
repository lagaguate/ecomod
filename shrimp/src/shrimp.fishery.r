
	loadfunctions( "shrimp", functionname="load.shrimp.environment.r" )

  refresh.data=F
  if (refresh.data) {
   s = shrimp.db( DS="shrimp.shrcomlog.redo" )
   s = shrimp.db( DS="shrimp.marfis.redo" )
  }
   
  # mapping of fisher stats
  loc = file.path( project.datadirectory("shrimp"), "maps" )

  s = shrimp.db( DS="shrimp.shrcomlog" )
  sm = s[ which( s$btype.simple=="mobile" & is.finite(s$fhours) ) ,]
  sm = sm[ filter.region.polygon ( sm, region="scotia.fundy" ), ]
  
  # rename a few vars to use the "fish.stat" function
  sm$catch = sm$weight
  sm$effort = sm$fhours
  sm$cpue = sm$catch / sm$effort


  map.planar = F
  if (map.planar) {
    require(grid)
    require(lattice)

    loc.p = file.path( loc, "png" )
    dir.create(path=loc.p, recursive=T, showWarnings=F)

    sm = lonlat2planar( sm, proj.type="utm20" )
    
    res = 5  # resolution in km
    sm$plat = trunc( sm$plat / res, 0) * res
    sm$plon = trunc( sm$plon / res, 0) * res
      
    
    planar.corners = data.frame(rbind( cbind( plon=c(220, 990), plat=c(4750, 5270) ))) # for plots in planar coords
    
    for ( i in c( "annual", "monthly", "all", "fiveyear" ) ) {
      sm$w = recode.time.block ( X=sm, type=i )  
      o = fish.stats( sm, type="planar"  )
      
      for ( y in sort( unique( o$w ) ) ) {
        oY = o[ which(o$w==y ) , ]
        
        for (v in c("catch", "effort", "cpue" ) ) {
          xyz = oY[ , c( "plon", "plat", v )]
          xyz[, v] = log10 ( xyz[, v] )  
          xyz = xyz [ which( is.finite( xyz[,v] ) ) ,]
          er = quantile( log10( o[ which( is.finite(o[,v]) ),v] ), probs=c(0.05, 0.95) ) 
          datarange = seq( er[1], er[2], length.out=150)
          cols = color.code( "seis", datarange )
          outfn = paste( i, v, y, sep=".")
          annot = paste( i, v, y )
          map( xyz=xyz, cfa.regions=T, depthcontours=T, pts=NULL, annot=annot, annot.cex=1,
            corners=planar.corners, fn=outfn, loc=loc.p, at=datarange , col.regions=cols, rez=c(res,res) )

        }
      }
    } # end for
  
  } # end if planar

  
  map.googleearth = T
  if (map.googleearth) {
      
    loadfunctions( "plottingmethods" )

    loc.k = file.path( loc, "kml" )
    dir.create(path=loc.k, recursive=T, showWarnings=F)

    coords = c("lon", "lat", "elevation")
    pins = c( "pin.blue", "pin.yellow", "pin.red" )
    
    res = 2 # resolution in minutes
    sm = lonlat.change.res ( sm, res=res ) 

    
    for ( i in c( "annual", "monthly", "all", "fiveyear" ) ) {
      
      outfn =  file.path( loc.k, paste( i, "kml", sep=".") ) 
          
      # start kml document
      con = kml.start( outfn,  i  )
        
        # define point styles/colours, etc
        kml.placemark.make( con, item="style", style.id="pin.red", colour="c0ffffff", scale=0.25, 
          href='files/reddot.png' )  # red dot
        kml.placemark.make( con, item="style", style.id="pin.yellow", colour="a0ffffff", scale=0.25, 
          href='files/yellowdot.png' )  # yellow dot
        kml.placemark.make( con, item="style", style.id="pin.blue", colour="a0ffffff", scale=0.25, 
          href='files/bluedot.png' )  

        # main folder start
        kml.folder.start( con, folder.name="Scotian Shelf Shrimp", 
          desc="Scotian Shelf Shrimp fishery statitics (Bedford Institute of Oceanography)" 
        )

          for (v in c("catch", "effort", "cpue" ) ) {
            sm$w = recode.time.block ( X=sm, type=i )  
            o = fish.stats( sm, type="lonlat"  )
             
            kml.folder.start( con, v )
          
              for ( y in sort( unique( o$w ) ) ) {
                oY = o[ which(o$w==y ) , ]
                xyz = oY[ , c( "lon", "lat", v )]
                xyz = xyz [ which( is.finite( xyz[,v] ) ) ,]
                xyz$elevation = 0  # a dummy variable
                xyz[, v] = log10 ( xyz[, v] ) 
                er = quantile( log10( o[ which( is.finite(o[,v]) ),v] ), probs=c(0, 0.333, 0.666, 1) ) 
                xyz$val = as.numeric( cut( xyz[,v], breaks=er, ordered_result=T, include.lowest=T ))
                      
                kml.folder.start( con, y )
                  for ( h in 1:nrow(xyz) ) { 
                    kml.placemark.make( con, desc=round(10^(xyz[h,v])), style.id=pins[ xyz[h,"val"] ], x=xyz[h, coords] ) 
                  }
                kml.folder.end( con ) # yr
              } # end y  

            kml.folder.end( con ) # v
          } # end  v
            
        kml.folder.end( con ) # end main
        kml.end( con )  # end file
        print( outfn )

      } # end i
    
    } # end if googleearth
 


