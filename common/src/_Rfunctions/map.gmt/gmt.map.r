  gmt.map = function( params, data, year, vname, conversions="ps2png" ) {

    with( params, {

    # tmp local GMT files
      cpt   = file.path(tmpdir, make.random.string(".gmt.cpt"))  # palette file
      grid  = file.path(tmpdir, make.random.string("gmt.grid"))
      bin   = file.path(tmpdir, make.random.string(".gmt.bin"))  # binary data
      clip  = file.path(tmpdir, make.random.string(".gmt.clip"))
      indat = file.path(tmpdir, make.random.string(".gmt.indat"))
      blocked = file.path(tmpdir, make.random.string(".gmt.blocked"))
      annotations = file.path(tmpdir, make.random.string(".gmt.annotations"))
      outfile = paste( file.path(tmpdir, make.random.string(".gmt.outfile")), ".ps", sep="") # final postscript file

    # final data check
      data = data[is.finite(rowSums(data)) ,]
      if (dim(data)[2]==3) data$dummy=1  # mimic a weighting variable when one does not exist

      write.table( data, file=indat, quote=F, col.names=F, row.names=F)
    # set up colour scale
      colscale = paste( "-C", gmtcol, sep="" )
      palette  = paste("-C", cpt, sep="")
      cmd( "makecpt", colscale, crange, colourflags, ">", cpt )

    # define some local parameters
      append = "-O -K"
      gmt.prep (fnt=fnt, psres=psresolution)

    # begin mapping
      cmd( "psbasemap", region, gmtproj, annot, "-K >", outfile )

     # overlay data
      if (block) {
        add.flag = ""
        if (blocktype=="mean") block.prg="blockmean"
        if (blocktype=="median") block.prg="blockmedian"
        if (blocktype=="mode") block.prg="blockmode"
        if (blocktype=="sum") {
          block.prg = "blockmean"
          add.flag = "-Sz"  # forces a sum as output
        }
        cmd( block.prg, region, res, indat, add.flag, "-W >", blocked )
        cmd( "surface", region, res, blocked, tension, paste("-G", grid, sep="") )
      } else if (! block) {
        cmd( "surface", region, res, indat, tension, paste("-G", grid, sep="") )
      }

      cmd( "psmask", indat, region, gmtproj, res, maskres, append, ">>", outfile )
      cmd( "grdimage", grid, region, gmtproj, append, interpres, palette, ">>", outfile )
      cmd( "psmask -C", append, ">>", outfile )

      # various overlays
      cmd( "cat", basemap, ">>", outfile ) # bathymetry
      if (!is.null(overlay)) {
      if ( length(overlay)>0 & overlay != "" ) {
        for (o in overlay) {
          cmd( "psxy", get.polygon.data.location(o), region, gmtproj, append, polygon.options, ">>", outfile ) # polygons
      }}}
      cmd( "pscoast", region, gmtproj, coast.options, append, ">>", outfile ) # coastline
      cmd( "psscale", palette, incscale, scale.location, append, ">>", outfile ) # colourscale
      if (annot.text=="") annot.text = year
      if (as.character( annot.text) == "1000" ) annot.text = "Mean state"
      write( paste(annot.lon0, annot.lat0, annot.fontsize, annot.angle, annot.fontno, annot.justify, annot.text), file=annotations )
      cmd( "pstext", annotations, region, gmtproj, "-O >>", outfile )  # variablename, year
      remove.files ( c( cpt, grid, bin, clip, indat, blocked, annotations ))
          
      if (!is.finite(conversions)) { #assume postscript not needed
        graphics.convert ( outfile, params$outfile.basename, conversions )
      } else { # assume just the postscript file is required
        file.copy(from=outfile, to=paste(params$outfile.basename,"ps",sep="."), overwrite=T)
      }
    
    }) # end with
              
    gmt.cleanup ()

 }


