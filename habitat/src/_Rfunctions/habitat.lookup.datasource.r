    habitat.lookup.datasource = function( DS, yr=NULL, p=NULL ) {
      # define and load starting habitat.db source from which to do the lookup 
      H = NULL
    
      if ( DS %in% c("speciesarea", "sizespectrum", "metabolism","speciescomposition", "condition", "biochem" ) ) { 
        p$project.name = DS
        p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )
        H = habitat.interpolate( DS=DS, p=p ) 
        H = H[,  c("plon", "plat", p$varstomodel ) ]  # add variable names
      }

      if ( DS=="depth") {
        H = bathymetry.db( p=p, DS="baseline" )
      }

      if ( DS=="depth.complete") {
        H = bathymetry.db( p=p, DS="complete" )
      }
      
      if ( DS=="substrate") {
        H = substrate.db ( p=p, DS="planar")
        H$substrate.mean = log(H$grainsize)
        H = H[, c("plon", "plat", "substrate.mean") ]
      }

      if ( DS %in% c("temperature", "temperature.weekly" ) ) {
        H = temperature.interpolations( p=p, DS="spatial.interpolation", yr=yr  )
      }
 
      if ( DS %in% c("temperature.climatology" ) ) {
        H = temperature.db( p=p, DS="climatology", year=yr  ) 
        # H = hydro.modelled.db( p=p, DS="bottom.mean",  vname="tmean" )
      }

      if ( DS %in% c( "temperature.complete" ) ) {
        H = temperature.db( p=p, DS="complete", year=yr  ) 
      }

      if ( DS %in% c( "temperature.annual" ) ) {
        H = hydro.modelled.db( p=p, DS="bottom.statistics.annual", yr=yr )
      }        

      if ( DS %in% c("default", "environmentals", "time.variant" ) ) {
        H = habitat.db( DS="environmentals", p=p, year=yr )  
      }

      if ( DS %in% c("all", "all.data") ) {
        H = habitat.db( DS="complete", p=p, year=yr )
      }

      if ( DS %in% c("baseline", "time.invariant") ) {
        H = habitat.db( DS="baseline", p=p ) 
      }
 
      if (is.null(H)) {
        H = habitat.db( DS="baseline", p=p ) # default for most, including depth, substrate, etc
      }
     
      return( H )
    }


