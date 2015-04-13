
  hydro.map = function( ip=NULL, p=NULL, type="annual", ... ) {
    
    # ip is the first parameter passed in the parallel mode
    
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    
    if (is.null(ip)) ip = 1:p$nruns

		require( lattice )
		datarange = seq(-0.5,12, length.out=50)
    cols = color.code( "blue.black", datarange )
    
    if ( type=="weekly" ) {
      bottomdir.maps = file.path( project.datadirectory("temperature"), "maps", p$spatial.domain, "bottom.predictions", "weekly" )
      dir.create( bottomdir.maps, recursive=T, showWarnings=F )
      datarange = seq(-0.5,12, length.out=50)
      cols = color.code( "blue.black", datarange )
      xyz = bathymetry.db( p=p, DS="Z.planar" )
      xyz = xyz[, c("plon", "plat")]
      for (iy in ip ) {
        y = p$runs[iy, "yrs"] 
        H = temperature.interpolations( p=p, DS = "temporal.interpolation", yr=y  )
        if (is.null(H)) next ()
        for (w in 1:52) {
          wchar = paste( "0", w, sep="" )
          wchar = substr( wchar, nchar(wchar)-1, nchar(wchar) )
          outfn = paste( "temperatures.bottom", y, wchar, sep=".")
          annot = paste("Temperature\n", y, " - ", w, sep="")
          map( xyz=cbind(xyz, H[,w]), cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols, corners=p$corners )
        }
      } 
      return( "Completed maps")
    } 
    
    if ( type %in% c("annual", "amplitudes", "temperatures", "week.of.minima", "halfperiod", "tsd" ) ) {
    
      bottomdir.maps = file.path( project.datadirectory("temperature"), "maps", p$spatial.domain , "bottom.predictions", "annual" )
      dir.create( bottomdir.maps, recursive=T, showWarnings=F )
 
      for (iy in ip ) {
        y = p$runs[iy, "yrs"] 
        print(y)
        
        H = hydro.modelled.db( p=p, DS="bottom.statistics.annual", yr=y )
        
        if ( p$spatial.domain=="snowcrab" ) {
          i = which( H$plon< 990 &  H$plon > 220  &   ## these are in planar coords  ..should fix this hack one day
                     H$plat< 5270 &  H$plat > 4675 
          )
          H = H[ i, ]
        } 
        if (is.null(H)) next ()
    
        if (type %in% c("temperatures", "annual") ) {
          datacols = c("plon", "plat", "tmean")
          datarange = seq(-1,11, length.out=50)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom", y, sep=".")
          annot = y
          map( xyz=H[,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols, corners=p$corners  )
        }  
    
        if (type %in% c("amplitudes", "annual") ) {
          datacols = c("plon", "plat", "tamplitude")
          datarange = seq(0,10, length.out=50)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom.amplitude", y, sep=".")
          annot = y
          map( xyz=H[,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols, corners=p$corners  )
        }  
    
        if (type %in% c("week.of.minima", "annual") ) {
          datacols = c("plon", "plat", "wmin")
          datarange = seq(0,52, length.out=50)
          cols = color.code( "blue.yellow.blue", datarange )
          outfn = paste( "week.temperature.minima", y, sep=".")
          annot = y
          map( xyz=H[,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols, corners=p$corners  )
        }  
    
        if (type %in% c("halfperiod", "annual") ) {
          datacols = c("plon", "plat", "thalfperiod")
          datarange = seq(0, 20, length.out=50)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "halfperiod.length", y, sep=".")
          annot = y
          map( xyz=H[,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols, corners=p$corners  )
        }

        if (type %in% c("tsd", "annual") ) {
          datacols = c("plon", "plat", "tsd")
          datarange = seq(0, 4, length.out=50)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom.sd", y, sep=".")
          annot = y
          map( xyz=H[,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols, corners=p$corners  )
        }
      
      } }
      
      if ( type %in% c("global", "amplitudes", "temperatures", "week.of.minima", "halfperiod", "tsd" ) ) {
    
      bottomdir.maps = file.path( project.datadirectory("temperature"), "maps", p$spatial.domain, "bottom.predictions", "global" )
      dir.create( bottomdir.maps, recursive=T, showWarnings=F )
 
        if (type %in% c("temperatures", "global") ) {
          H = hydro.modelled.db( p=p, DS="bottom.mean",  vname="tmean" )
          datacols = c("plon", "plat", "tmean")
          datarange = seq(-1,11, length.out=50)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom", sep=".")
          annot = paste("Temperature\n", sep="")
          map( xyz=H[,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols, corners=p$corners  )
        }  
    
        if (type %in% c("amplitudes", "global") ) {
          H = hydro.modelled.db( p=p, DS="bottom.mean",  vname="tamplitude")
          datacols = c("plon", "plat", "tamplitude")
          datarange = seq(0,10, length.out=50)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom.amplitude", sep=".")
          annot = paste("Temperature amplitude\n",  sep="")
          map( xyz=H[,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols, corners=p$corners  )
        }  
    
        if (type %in% c("week.of.minima", "global") ) {
          H = hydro.modelled.db( p=p, DS="bottom.mean",  vname="wmin")
          datacols = c("plon", "plat", "wmin")
          datarange = seq(0,52, length.out=50)
          cols = color.code( "blue.yellow.blue", datarange )
          outfn = paste( "week.temperature.minima", sep=".")
          annot = paste("Week of temperature minima\n", sep="")
          map( xyz=H[,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols, corners=p$corners  )
                 }  
    
        if (type %in% c("halfperiod", "global") ) {
          H = hydro.modelled.db( p=p, DS="bottom.mean",  vname="thalfperiod" )
          datacols = c("plon", "plat", "thalfperiod")
          datarange = seq(0, 20, length.out=50)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "halfperiod.length", sep=".")
          annot = paste("Length of half-period\n", sep="")
          map( xyz=H[,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols, corners=p$corners  )
        }
        if (type %in% c("tsd", "global") ) {
          H = hydro.modelled.db( p=p, DS="bottom.mean",  vname="tsd" )
          datacols = c("plon", "plat", "tsd")
          datarange = seq(0, 5, length.out=50)
          cols = color.code( "blue.black", datarange )
          outfn = paste( "temperatures.bottom.sd",  sep=".")
          annot = paste("Temperature SD\n", sep="")
          map( xyz=H[,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=bottomdir.maps, at=datarange , col.regions=cols, corners=p$corners  )
        }

    }
    return (NULL)
  }



