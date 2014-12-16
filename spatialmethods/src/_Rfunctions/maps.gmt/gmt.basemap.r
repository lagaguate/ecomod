  gmt.basemap = function( P ) {
      
    if(! exists("isobaths_toplot", P)) P$isobaths_toplot = seq(0, 500, 100 )

     P = gmt.resolution(P)
     P = gmt.projection(P)
     P = gmt.defineregion(P)

     # override defaults:
     P$polygon.options = "-W0.2p"

		 tmpdir  = tempdir()
  
      gmtappend = "-O -K"
      bathy.mask = file.path(tmpdir, make.random.string("gmt.depth.mask") )
      gmt.annot = file.path(tmpdir, make.random.string("gmt.annot") )
      gmt.clip = file.path(tmpdir, make.random.string("gmt.clip") )
      gmt.isobathdata = file.path(tmpdir, make.random.string(".gmt.isobathdata") )
      gmt.depths = file.path(tmpdir, make.random.string(".gmt.depths") )
      gmt.depth.mask = file.path(tmpdir, make.random.string(".gmt.depth.mask") )
      gmt.data.landmask = file.path(tmpdir, make.random.string(".gmt.data.landmask") )

      basemap.options =  " -B2neSW "

      cmd( "blockmedian -bi -bo", p$bathymetry.bin, P$region, P$res, ">", gmt.clip )
      cmd( "surface -bi", gmt.clip, P$region, P$res, P$bathy.tension, paste("-G", gmt.depths, sep="" ) )
      cmd( "psmask -bi", p$bathymetry.bin, P$region, P$res, P$bathy.maskres, P$gmtproj, gmtappend, ">", P$basemap )
 
      for (d in P$isobaths_toplot ){
        isob = NULL
        isob = isobath.db( p=p, depths=d ) 
        if (is.null(isob)) next()
        write.table( isob, file=gmt.isobathdata, quote=FALSE, col.names=FALSE, row.names=FALSE )
        cmd( "gmt psxy", gmt.isobathdata, P$region, P$gmtproj, P$polygon.options, gmtappend, ">>", P$basemap )
      }
  
      cmd( "gmt psbasemap", P$region, P$gmtproj, basemap.options,  gmtappend, ">>", P$basemap )
      cmd( "gmt psmask -C", gmtappend, ">>", P$basemap )
      
#      remove.files ( c( bathy.mask, gmt.clip, gmt.depths, gmt.depth.mask, gmt.data.landmask) )

      remove.files ( c( gmt.isobathdata , gmt.clip ) ) 

      if (FALSE){
        cmd( "gmt blockmedian -bi -bo", p$bathymetry.bin, P$region, P$res, ">", gmt.clip )
        # cmd( "surface -bi", gmt.clip, P$region, P$res, P$bathy.tension, paste("-G", gmt.depths, sep="" ) )
        cmd( "gmt psmask -bi", p$bathymetry.bin, P$region, P$res, P$bathy.maskres, P$gmtproj, gmtappend, ">", P$basemap )
        for (d in P$isobaths_toplot ){
          isob = NULL
          isob = isobath.db( p=p, depths=d ) 
          if (is.null(isob)) next()
          write.table( isob, file=gmt.isobathdata, quote=FALSE, col.names=FALSE, row.names=FALSE )
          cmd( "gmt psxy", gmt.isobathdata, P$region, P$gmtproj, P$polygon.options, gmtappend, ">>", P$basemap )
        }
        cmd( "gmt psmask -C", gmtappend, ">>", P$basemap )

#      cmd( "grdclip", gmt.depths, P$bathy.zrange, paste("-G", gmt.depth.mask, sep="") )
#      cmd( "grdlandmask", P$region, P$res,"-N1/NaN/NaN/NaN/NaN -Di", paste("-G", gmt.data.landmask, sep="") )
#      cmd( "grdmath", gmt.data.landmask,  gmt.depth.mask, "MUL =", bathy.mask)
#      cmd( "grdcontour", bathy.mask, P$gmtproj, P$bathy.contour, gmtappend, ">>", P$basemap )
#      cmd( "psmask -C", gmtappend, ">>", P$basemap )

        output.ps = "~/tmp/test.ps"
        cmd(" cat " ,  P$basemap, ">", output.ps )
        cmd( "pscoast", P$region, P$gmtproj, gmtappend, "-Di -W0.1p >>", output.ps  )
      # FINISH WITH A BLANK ANNOTATION  
        annotation = "-63.7 47.25 6 0 Helvetica LT . " 
        write( annotation, file=gmt.annot )
        cmd( "pstext", gmt.annot, P$region, P$gmtproj,  "-O >>", output.ps )  
      }


  }


