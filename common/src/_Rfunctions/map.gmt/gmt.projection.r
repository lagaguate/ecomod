  gmt.projection = function(params) {
    gproj = params$gmt.projection.long
    params$gmtproj = NA
    if (gproj=="Ptolemy.equidistant.conic") params$gmtproj = "-JD-62/44.5/46/43/8.5i"
    if (gproj=="Albers.equalarea.conic") params$gmtproj = "-JB-62/44.5/46/43/8.5i"
    if (gproj=="Lambert.conformal.conic") params$gmtproj = "-JL-61/45/46/44/6.5i"
    if (gproj=="Lambert.conformal.conic.crab") params$gmtproj = "-JL-61.5/45/46.5/43.5/6.5i"
    if (gproj=="Canada.east.PlateCarre-62") params$gmtproj = "-JQ-62/6.5i"
    if (gproj=="Lambert.conformal.conic.ecnasap") params$gmtproj = "-JL-55/50/40/60/6.5i"
    if (gproj=="Lambert.conformal.conic.ecnasap2") params$gmtproj = "-JL-58/50/45/55/6.5i"
    if (gproj=="Lambert.conformal.conic.porbeagle") params$gmtproj = "-JL-56.5/47.5/42/53/6i"
    if (gproj=="Mercator") params$gmtproj = "-JM6i"
    if (gproj=="UTM") params$gmtproj = "-JU20/7i"    # this is zone 20 ???
    return (params)
  }


