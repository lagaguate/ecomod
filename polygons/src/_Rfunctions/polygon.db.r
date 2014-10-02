  polygon.db = function(object) {
    
		#root.polygons = project.directory("polygons","data")
		out=NULL
    
    if ("snowcrab.boundingbox" %in%  object)  out = find.ecomod.gis("snowcrab.boundingbox.dat") 
    if ("snowcrab.boundingbox.redo" %in%  object) {
      ss = snowcrab.db( DS="set.logbook")
      plot( ss$lon, ss$lat )

      bb=locator( type="l" ) 
      bb = as.data.frame(bb)
      names(bb) = c("lon", "lat")
      write.table( bb, find.ecomod.gis("snowcrab.boundingbox.dat" ), col.names=FALSE, row.names=FALSE )
    }

    if ("haddockbox" %in% object) out = find.ecomod.gis("haddockbox.dat")  # (lon, lat)
    if ("cfaall" %in% object)     out = find.ecomod.gis("cfaall.dat")  # (lon, lat)
    if ("cfanorth" %in% object)   out = find.ecomod.gis("cfanorth.dat")  # (lon, lat)
    if ("cfasouth" %in% object)   out = find.ecomod.gis("cfasouth.dat")  # (lon, lat)
    if ("cfa20" %in% object)      out = find.ecomod.gis("cfa20.dat")  # (lon, lat)
    if ("cfa20inner" %in% object) out = find.ecomod.gis("cfa20inner.dat")  # (lon, lat)
    if ("cfa20outer" %in% object) out = find.ecomod.gis("cfa20outer.dat")  # (lon, lat)
    if ("cfa21" %in% object)      out = find.ecomod.gis("cfa21.dat")  # (lon, lat)
    if ("cfa22" %in% object)      out = find.ecomod.gis("cfa22.dat")  # (lon, lat)
    if ("cfa22inner" %in% object) out = find.ecomod.gis("cfa22inner.dat")  # (lon, lat)
    if ("cfa22outer" %in% object) out = find.ecomod.gis("cfa22outer.dat")  # (lon, lat)
    if ("cfa23" %in% object)      out = find.ecomod.gis("cfa23.dat")  # (lon, lat)
    if ("cfa23a" %in% object)     out = find.ecomod.gis("cfa23a.dat")  # (lon, lat)
    if ("cfa23b" %in% object)     out = find.ecomod.gis("cfa23b.dat")  # (lon, lat)
    if ("cfa23c" %in% object)     out = find.ecomod.gis("cfa23c.dat")  # (lon, lat)
    if ("cfa23d" %in% object)     out = find.ecomod.gis("cfa23d.dat")  # (lon, lat)
    if ("cfa23slope" %in% object)     out = find.ecomod.gis("cfa23slope.dat")  # (lon, lat)
    if ("cfa24" %in% object)     out = find.ecomod.gis("cfa24.dat")  # (lon, lat)
    if ("cfa24a" %in% object)     out = find.ecomod.gis("cfa24a.dat")  # (lon, lat)
    if ("cfa24b" %in% object)     out = find.ecomod.gis("cfa24b.dat")  # (lon, lat)
    if ("cfa24c" %in% object)     out = find.ecomod.gis("cfa24c.dat")  # (lon, lat)
    if ("cfa24d" %in% object)     out = find.ecomod.gis("cfa24d.dat")  # (lon, lat)
    if ("cfa24e" %in% object)     out = find.ecomod.gis("cfa24e.dat")  # (lon, lat)
    if ("cfa24slope" %in% object)     out = find.ecomod.gis("cfa24slope.dat")  # (lon, lat)
    if ("cfaslope" %in% object)     out = find.ecomod.gis("cfaslope.dat")  # (lon, lat)
    if ("cfa4x" %in% object)     out = find.ecomod.gis("cfa4x.dat")  # (lon, lat)
    if ("4X" %in% object)     out = find.ecomod.gis("cfa4x.dat")  # (lon, lat)
    if ("ez200" %in% object)     out = find.ecomod.gis("ez200.xy")  # (lon, lat)
    if ("limit200" %in% object)     out = find.ecomod.gis("limit200.xy")  # (lon, lat)
    if ("strat.gf" %in% object)     out = find.ecomod.gis("strat.gf.xy")  # (lon, lat)
    if ("isobath1000m" %in% object)     out = find.ecomod.gis("isobath1000m.dat")  # (lon, lat)
    if ("nafo.2j"  %in% object)     out =find.ecomod.gis("nafo.2j.dat")
    if ("nafo.3k"  %in% object)     out =find.ecomod.gis("nafo.3k.dat")
    if ("nafo.3l"  %in% object)     out =find.ecomod.gis("nafo.3l.dat")
    if ("nafo.3n"  %in% object)     out =find.ecomod.gis("nafo.3n.dat")
    if ("nafo.3o"  %in% object)     out =find.ecomod.gis("nafo.3o.dat")
    if ("nafo.3p"  %in% object)     out =find.ecomod.gis("nafo.3p.dat")
    if ("nafo.4r"  %in% object)     out =find.ecomod.gis("nafo.4r.dat")
    if ("nafo.4s"  %in% object)     out =find.ecomod.gis("nafo.4s.dat")
    if ("nafo.4t"  %in% object)     out =find.ecomod.gis("nafo.4t.dat")
    if ("nafo.4v"  %in% object)     out =find.ecomod.gis("nafo.4v.dat")
    if ("nafo.4w"  %in% object)     out =find.ecomod.gis("nafo.4w.dat")
    if ("nafo.4x"  %in% object)     out =find.ecomod.gis("nafo.4x.dat")
    if ("nafo.5y"  %in% object)     out =find.ecomod.gis("nafo.5y.dat")
    if ("nafo.5z"  %in% object)     out =find.ecomod.gis("nafo.5zew.dat")
    if ("nafo.5zew"  %in% object)     out =find.ecomod.gis("nafo.5zew.dat")
    if ("nafo.5ze"  %in% object)     out =find.ecomod.gis("nafo.5ze.dat")
    if ("nafo.5zw"  %in% object)     out =find.ecomod.gis("nafo.5zw.dat")

    if ("nafo.2j3kl"  %in% object)     out =find.ecomod.gis("nafo.2j3kl.dat")
    if ("nafo.3no"  %in% object)     out =find.ecomod.gis("nafo.3no.dat")
    if ("nafo.4rs"  %in% object)     out =find.ecomod.gis("nafo.4rs.dat")
    if ("nafo.4vw"  %in% object)     out =find.ecomod.gis("nafo.4vw.dat")
    if ("nafo.5yz"  %in% object)     out =find.ecomod.gis("nafo.5yz.dat")

    if ("cfa4t.12"  %in% object)     out =find.ecomod.gis("cfa4t.12.dat")
    if ("cfa4t.1"  %in% object)     out =find.ecomod.gis("cfa4t.1.dat")
    if ("StMargaretsBay"  %in% object)     out =find.ecomod.gis("StMargaretsBay.dat")
    if ("MahoneBay"  %in% object)     out =find.ecomod.gis("MahoneBay.dat")
    if ("scotia.fundy"  %in% object)     out =find.ecomod.gis("scotia.fundy.dat")
		out = file.path(  out )
    return (out)
  }

