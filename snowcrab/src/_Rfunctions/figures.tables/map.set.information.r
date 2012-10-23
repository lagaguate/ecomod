  map.set.information = function(p, plottimes, outdir, conversions, init.files, method="gmt" ) {
    
    set = snowcrab.db( DS="set.complete")
    variables = get.variables("all.data")

    if (method =="gmt") {
      p$mapres = "2min"
      p = gmt.resolution(p) # refresh due to change in mapres
      
      p$tension = "-T.4"  # 0.35+ for steep; 0.25 for smooth
      p$maskres = "-S16k"
      p$interpres = "-Sb"
      
      outdir = file.path( outdir, paste( p$mapres, p$spatial.domain, sep=".") )
           
      make.maps( set, p=p, variables=variables, plottimes=plottimes, 
        basedir=outdir, conversions=conversions, init.files=init.files )
    }
    
    if (method=="levelplot") {
      
      # define compact list of variable year combinations for parallel processing
        p = list()
        p = make.list( list(variables, mapyears ), Y=p )
 
      for (i in init.files) source( i )
      if ( is.null(id)) id = c(1: p$nruns ) 
      id = as.numeric(id)

      for (i in ip ) {
        v = p$runs[i,1]
        y = p$runs[i,2]
        outfn = paste( "test", sep=".")
        outloc = file.path( getwd() )
        xyz = set[ which(set$yr==y), c("plon","plat",v) ]
        er = empirical.ranges( db="snowcrab", v )  # range of all years
        datarange = seq( er[1], er[2], length.out=50)
        corners = data.frame(rbind( cbind( plon=c(220, 990), plat=c(4750, 5270) )))
        cols = colorRampPalette(c("darkblue","cyan","green", "yellow", "orange","darkred", "black"), space = "Lab")
        # cols = colorRampPalette(c("darkblue","cyan","green", "yellow", "orange","darkred", "black"), space = "Lab")
        map( xyz, xyz.coords="planar", cfa.regions=T, depthcontours=T, pts=xyz, annot=y, fn=outfn, loc=outloc, at=datarange , col.regions=cols(length(datarange)+1), colpts=T, corners=planar.corners )
      }

    }
          
    return("Done")
  }


