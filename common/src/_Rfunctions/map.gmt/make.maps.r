 
  # ---------------------------------------------------------------------
  # Wrapping function to access GMT (Generic Mapping Tools)
  #     
  #     Warning: GMT calls must be understood by your system: 
  #     i.e., GMT program must be in the file path 
  #     "Gawk" is used in some instances ..  this can eventually be implemented directly in R
  # ---------------------------------------------------------------------


  # ---------------------------------------------------------------------
  # main calling program
  #
  
  make.maps = function( U, params, variables, plottimes, basedir, conversions="ps2png", delta=1, init.files=NULL, db="snowcrab") {
    nvars = length( variables )
    if (!params$do.parallel) {
      make.maps.core(U=U, params=params, variables=variables, plottimes=plottimes, basedir=basedir, conversions=conversions, delta=delta, init.files=init.files, db=db)
    } else if (params$do.parallel) {
      pp = prep.parallel.run(params$clusters, nvars)
      clusterApplyLB( pp$cl, pp$ssplt, make.maps.core, U=U, params=params, variables=variables, plottimes=plottimes, basedir=basedir, conversions=conversions, delta=delta, init.files=init.files, db=db )
      stopCluster(pp$cl)
    }
    return ("Completed mapping")
  }


