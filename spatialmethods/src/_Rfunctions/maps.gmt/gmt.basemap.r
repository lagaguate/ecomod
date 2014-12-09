  gmt.basemap = function( P, redo.bin=F ) {
     
     P = gmt.resolution(P)
     P = gmt.projection(P)
     P = gmt.defineregion(P)

		 tmpdir  = tempdir()
  
      gmtappend = "-O -K"
      bathy.mask = file.path(tmpdir, make.random.string("gmt.depth.mask") )
      gmt.clip = file.path(tmpdir, make.random.string(".gmt.clip") )
      gmt.depths = file.path(tmpdir, make.random.string(".gmt.depths") )
      gmt.depth.mask = file.path(tmpdir, make.random.string(".gmt.depth.mask") )
      gmt.data.landmask = file.path(tmpdir, make.random.string(".gmt.data.landmask") )

      # make the basemap with bathimetry; "basemap" is defined in the P list
      bath.inp.bin =  p$bathymetry.bin
      if ( !file.exists(bath.inp.bin) | redo.bin ) cmd( "gmtconvert -bo", p$bathymetry.xyz, ">", bath.inp.bin )

      cmd( "blockmedian -bi -bo", bath.inp.bin, P$region, P$res, ">", gmt.clip )
      cmd( "surface -bi", gmt.clip, P$region, P$res, P$bathy.tension, paste("-G", gmt.depths, sep="" ) )
      cmd( "psmask -bi",  bath.inp.bin, P$region, P$res, P$bathy.maskres, P$gmtproj, gmtappend, ">", P$basemap )
      cmd( "grdclip", gmt.depths, P$bathy.zrange, paste("-G", gmt.depth.mask, sep="") )
      cmd( "grdlandmask", P$region, P$res,"-N1/NaN/NaN/NaN/NaN -Di", paste("-G", gmt.data.landmask, sep="") )
      cmd( "grdmath", gmt.data.landmask,  gmt.depth.mask, "MUL =", bathy.mask)
      cmd( "grdcontour", bathy.mask, P$gmtproj, P$bathy.contour, gmtappend, ">>", P$basemap )
      cmd( "psmask -C", gmtappend, ">>", P$basemap )

      remove.files ( c( bathy.mask, gmt.clip, gmt.depths, gmt.depth.mask, gmt.data.landmask) )
  
  }


