 
  isobath.db = function( ip=NULL, p=NULL, depths=NULL, DS="" ) {

    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 

    if (p$spatial.domain == "snowcrab" )  p = spatial.parameters( type="SSE", p=p )
  
      if ( DS == "" ) {
        out = NULL
        for (d in depths) {
          fn = file.path( project.directory("bathymetry"), "isobaths", paste( p$spatial.domain, "isobath", d, "rdata", sep=".") )
          if (file.exists(fn) ) load( fn)
          out = rbind( out, isobath )
        }
        return (out)    
      } 

			if ( DS != "redo" ) return( NULL)
      
      #overrides to defaults
			p$bathy.zrange="-Sa1010/NaN -Sb1/NaN"
			tmpdir =  tempdir()

			# ip is the first parameter passed in the parallel mode
      if (is.null(ip)) ip = 1:p$nruns
			for (id in ip ) {

				d = p$runs[ id, "depths" ]
        
				fn = file.path( project.directory("bathymetry"), "isobaths", paste( p$spatial.domain, "isobath", d, "rdata", sep=".") )
        ib = NULL
        
				print (fn)

				  isobaths = file.path(tmpdir, make.random.string(".tmp.isobaths"))
          tmp.iso = file.path(tmpdir, make.random.string(".tmp.iso"))
          if (d==0) {
            cmd( "pscoast", p$region, p$gmtproj, " -Df -M -W  >", isobaths )   # shorelines
            cmd( "pscoast", p$region, p$gmtproj, " -Df -M -N1 -N3  >>", isobaths ) # political boundaries
            cmd( "pscoast", p$region, p$gmtproj, " -Df -M -Ir >>", isobaths ) # rivers and lakes

          } else {
						# gmt.bin = gsub(".xyz$", ".bin", inp)
						gmt.bin = p$bathymetry.bin 
            gmt.clip = file.path(tmpdir, make.random.string(".gmt.clip"))
            gmt.depths = file.path(tmpdir, make.random.string(".gmt.depths"))
            gmt.depth.mask = file.path(tmpdir, make.random.string(".gmt.depth.mask"))
            gmt.data.landmask = file.path(tmpdir, make.random.string(".gmt.data.landmask"))
            basemap = file.path(tmpdir, make.random.string(".gmt.basemap.ps"))
						bathy.masked = file.path(tmpdir, make.random.string(".gmt.bathy.masked"))
            bathy.contour= paste( "-S10", paste("-D", isobaths, sep=""), 
              paste("-C", abs(d), sep=""), 
              paste("-L",d-1,"/",d+1,sep="") ) # override default

					# cmd( "gmtconvert -bo", inp, ">", gmt.bin )
            cmd( "blockmedian -bi3 -bo", p$bathymetry.bin, p$region, p$res.isobaths , ">", gmt.clip )
            cmd( "surface -bi3", gmt.clip, p$region, p$res.isobaths , p$bathy.tension, paste("-G", gmt.depths, sep="" ))
            
            cmd( "grdclip", gmt.depths, p$bathy.zrange, paste("-G", gmt.depth.mask, sep="") )
            cmd( "grdlandmask", p$region, p$res.isobaths , "-N1/NaN/NaN/NaN/NaN -Dif", paste("-G", gmt.data.landmask, sep=""))
            cmd( "grdmath", gmt.data.landmask, gmt.depth.mask, "MUL =", bathy.masked)

            cmd( "grdcontour", bathy.masked, p$gmtproj, bathy.contour, ">", basemap )
            remove.files ( c( basemap, gmt.clip, gmt.depths, gmt.data.landmask,
										gmt.depth.mask, bathy.masked ) )

          }
          
					if ( file.exists( isobaths ) ) {
						cmd( "gawk '!/>/' ", isobaths, ">", tmp.iso )
		        isobath = read.table(tmp.iso)
			      isobath = isobath[, c(1,2)]
           
				    names( isobath ) = c( "lon", "lat" )
					  
            check = min( isobath$lon) 
            while ( check < -360 ) {
              isobath$lon = isobath$lon + 360 
              check = min( isobath$lon)
            }
            
            check = max( isobath$lon) 
            while ( check > 360 ) {
              isobath$lon = isobath$lon - 360 
              check = max( isobath$lon)
            }
						
            save( isobath, file=fn, compress=T)
						remove.files ( c(isobaths, tmp.iso) ) 
					}
      }
      
      return(fn)
    }


