  gmt.basemap = function( params, redo.bin=F ) {
     params = gmt.resolution(params)
     params = gmt.projection(params)
     params = gmt.defineregion(params)

		 tmpdir  = tempdir()
  
     with(params, {
      append = "-O -K"
      bathy.mask = file.path(tmpdir, make.random.string("gmt.depth.mask") )
      gmt.clip = file.path(tmpdir, make.random.string(".gmt.clip") )
      gmt.depths = file.path(tmpdir, make.random.string(".gmt.depths") )
      gmt.depth.mask = file.path(tmpdir, make.random.string(".gmt.depth.mask") )
      gmt.data.landmask = file.path(tmpdir, make.random.string(".gmt.data.landmask") )

      # make the basemap with bathimetry; "basemap" is defined in the params list
      bath.inp.bin = gsub(".xyz$", ".bin", inp)

      if ( !file.exists(bath.inp.bin) | redo.bin ) cmd( "gmtconvert -bo", inp, ">", bath.inp.bin )

      cmd( "blockmedian -bi -bo", bath.inp.bin, region, res, ">", gmt.clip )
      cmd( "surface -bi", gmt.clip, region, res, bathy.tension, paste("-G", gmt.depths, sep="" ) )
      cmd( "psmask -bi",  bath.inp.bin, region, res, bathy.maskres, gmtproj, append, ">", basemap )
      cmd( "grdclip", gmt.depths, bathy.zrange, paste("-G", gmt.depth.mask, sep="") )
      cmd( "grdlandmask", region, res,"-N1/NaN/NaN/NaN/NaN -Di", paste("-G", gmt.data.landmask, sep="") )
      cmd( "grdmath", gmt.data.landmask,  gmt.depth.mask, "MUL =", bathy.mask)
      cmd( "grdcontour", bathy.mask, gmtproj, bathy.contour, append, ">>", basemap )
      cmd( "psmask -C", append, ">>", basemap )

      remove.files ( c( bathy.mask, gmt.clip, gmt.depths, gmt.depth.mask, gmt.data.landmask) )
    }) # end with
  }


