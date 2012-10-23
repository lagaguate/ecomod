
  map.cat.information = function(p, plottimes, outdir, conversions, init.files ) {
    cat = snowcrab.db( DS="cat.georeferenced")
    species = c( "cod", "skates", "thornyskate", "northernshrimp",
               "brittlestar", "basketstar",
               "bluecrab", "snowcrab", "greencrab", "hermitcrab", "jonahcrab",
               "lessertoadcrab", "northernstonecrab", "porcupinestonecrab", "portlyspidercrab",
               "redcrab", "atlanticrockcrab", "toadcrab", "snowcrab")
    p$tension = "-T.4"  # 0.35+ for steep; 0.25 for smooth
    p$maskres = "-S16k"
    p$interpres = "-Sb"
    p$mapres = "2min"
    p$do.parallel = F

    p = gmt.resolution(p) # refresh due to change in mapres
    
    for (sp in species) {
      outvars = c("trip", "set", "yr", "lon", "lat", "totmass", "totno", "sa")
      basedir = file.path( outdir, paste(p$mapres, p$spatial.domain, sep="."), sp )
      subset = cat[ filter.taxa (x=cat$spec, method=sp), outvars]
      if (dim(subset)[1] > 1) {
        variables = c("totno")
        subset = subset[ is.finite(rowSums(subset[, c("yr", "lon", "lat")])), ]
        subset$totno = log10( subset$totno )  ###### ---------------------- log transform done here as "totno" is not part of the snowcrab recoed db
        # must  sum the catches or numbers by set/trip (across species) before plotting
        # incase multiple species are selected
        make.maps( subset, p, variables=variables, plottimes=plottimes, basedir=basedir, conversions=conversions, init.files=init.files)
       }
    }
    return( "Done" )
  }


