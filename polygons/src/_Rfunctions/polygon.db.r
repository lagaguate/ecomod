  polygon.db = function(object) {
    # polygons aliased by keywords rather than file name 
		root.polygons = project.directory("polygons","data")
		out=NULL
    
    if ("snowcrab.boundingbox" %in%  object)  out = "snowcrab.boundingbox.dat" 
    if ("snowcrab.boundingbox.redo" %in%  object) {
      ss = snowcrab.db( DS="set.logbook")
      plot( ss$lon, ss$lat )

      bb=locator( type="l" ) 
      bb = as.data.frame(bb)
      names(bb) = c("lon", "lat")
      write.table( bb, file=file.path(  root.polygons, "snowcrab.boundingbox.dat" ), col.names=FALSE, row.names=FALSE )
    }

    if ("haddockbox" %in% object) out = "haddockbox.dat"  # (lon, lat)
    if ("cfaall" %in% object)     out = "cfaall.dat"  # (lon, lat)
    if ("cfanorth" %in% object)   out = "cfanorth.dat"  # (lon, lat)
    if ("cfasouth" %in% object)   out = "cfasouth.dat"  # (lon, lat)
    if ("cfa20" %in% object)      out = "cfa20.dat"  # (lon, lat)
    if ("cfa20inner" %in% object) out = "cfa20inner.dat"  # (lon, lat)
    if ("cfa20outer" %in% object) out = "cfa20outer.dat"  # (lon, lat)
    if ("cfa21" %in% object)      out = "cfa21.dat"  # (lon, lat)
    if ("cfa22" %in% object)      out = "cfa22.dat"  # (lon, lat)
    if ("cfa22inner" %in% object) out = "cfa22inner.dat"  # (lon, lat)
    if ("cfa22outer" %in% object) out = "cfa22outer.dat"  # (lon, lat)
    if ("cfa23" %in% object)      out = "cfa23.dat"  # (lon, lat)
    if ("cfa23a" %in% object)     out = "cfa23a.dat"  # (lon, lat)
    if ("cfa23b" %in% object)     out = "cfa23b.dat"  # (lon, lat)
    if ("cfa23c" %in% object)     out = "cfa23c.dat"  # (lon, lat)
    if ("cfa23d" %in% object)     out = "cfa23d.dat"  # (lon, lat)
    if ("cfa23slope" %in% object)     out = "cfa23slope.dat"  # (lon, lat)
    if ("cfa24" %in% object)     out = "cfa24.dat"  # (lon, lat)
    if ("cfa24a" %in% object)     out = "cfa24a.dat"  # (lon, lat)
    if ("cfa24b" %in% object)     out = "cfa24b.dat"  # (lon, lat)
    if ("cfa24c" %in% object)     out = "cfa24c.dat"  # (lon, lat)
    if ("cfa24d" %in% object)     out = "cfa24d.dat"  # (lon, lat)
    if ("cfa24e" %in% object)     out = "cfa24e.dat"  # (lon, lat)
    if ("cfa24slope" %in% object)     out = "cfa24slope.dat"  # (lon, lat)
    if ("cfaslope" %in% object)     out = "cfaslope.dat"  # (lon, lat)
    if ("cfa4x" %in% object)     out = "cfa4x.dat"  # (lon, lat)
    if ("4X" %in% object)     out = "cfa4x.dat"  # (lon, lat)
    if ("ez200" %in% object)     out = "ez200.xy"  # (lon, lat)
    if ("limit200" %in% object)     out = "limit200.xy"  # (lon, lat)
    if ("strat.gf" %in% object)     out = "strat.gf.xy"  # (lon, lat)
    if ("isobath1000m" %in% object)     out = "isobath1000m.dat"  # (lon, lat)
    if ("nafo.2j"  %in% object)     out ="nafo.2j.dat"
    if ("nafo.3k"  %in% object)     out ="nafo.3k.dat"
    if ("nafo.3l"  %in% object)     out ="nafo.3l.dat"
    if ("nafo.3n"  %in% object)     out ="nafo.3n.dat"
    if ("nafo.3o"  %in% object)     out ="nafo.3o.dat"
    if ("nafo.3p"  %in% object)     out ="nafo.3p.dat"
    if ("nafo.4r"  %in% object)     out ="nafo.4r.dat"
    if ("nafo.4s"  %in% object)     out ="nafo.4s.dat"
    if ("nafo.4t"  %in% object)     out ="nafo.4t.dat"
    if ("nafo.4v"  %in% object)     out ="nafo.4v.dat"
    if ("nafo.4w"  %in% object)     out ="nafo.4w.dat"
    if ("nafo.4x"  %in% object)     out ="nafo.4x.dat"
    if ("nafo.5y"  %in% object)     out ="nafo.5y.dat"
    if ("nafo.5z"  %in% object)     out ="nafo.5zew.dat"
    if ("nafo.5zew"  %in% object)     out ="nafo.5zew.dat"
    if ("nafo.5ze"  %in% object)     out ="nafo.5ze.dat"
    if ("nafo.5zw"  %in% object)     out ="nafo.5zw.dat"

    if ("nafo.2j3kl"  %in% object)     out ="nafo.2j3kl.dat"
    if ("nafo.3no"  %in% object)     out ="nafo.3no.dat"
    if ("nafo.4rs"  %in% object)     out ="nafo.4rs.dat"
    if ("nafo.4vw"  %in% object)     out ="nafo.4vw.dat"
    if ("nafo.5yz"  %in% object)     out ="nafo.5yz.dat"

    if ("cfa4t.12"  %in% object)     out ="cfa4t.12.dat"
    if ("cfa4t.1"  %in% object)     out ="cfa4t.1.dat"
    if ("StMargaretsBay"  %in% object)     out ="StMargaretsBay.dat"
    if ("MahoneBay"  %in% object)     out ="MahoneBay.dat"
    if ("scotia.fundy"  %in% object)     out ="scotia.fundy.dat"

    out = file.path(  root.polygons, out )
    return (out)
  }

