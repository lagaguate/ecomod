
  map.gullydata = function( gully ) {
    
    G = gully$G
    G$sa = 1  # dummy weight variable
    
    p$tension = "-T0.9"  # 0.35+ for steep; 0.25 for smooth
    p$maskres = "-S10k"
    p$interpres = "-Sb"
    p$mapres = "2min"
    p$do.parallel = F
    p$colourscale.quantiles = T
    p$colourscale.quantiles.probs = c(0.05, 0.95)

    p = gmt.resolution(p) # refresh due to change in mapres
    basedir = file.path( project.gully, "maps" )
     
    variables = c("PC1.organics", "PC2.organics", "PC1.metals", "PC2.metals", gully$metals, gully$organics )
    to.log = c( gully$metals, gully$organics)
    for (vs in variables ){
      subset.vars =  c("lon", "lat", "yr", "sa", vs)
      subset = G[ which( is.finite(rowSums( G[, subset.vars]))) , subset.vars ]
      if (vs %in% to.log) { subset[,vs] = log10(subset[,vs]) }
      make.maps( subset, p, variables=vs, plottimes="annual", basedir=basedir, conversions=conversions, init.files=init.files)
    }

  }


