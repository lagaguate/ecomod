
  habitatsuitability.generic.db = function( DS="", p=NULL ) {
    
    SSSdir = file.path( project.directory("habitatsuitability"), "analysis", p$speciesofinterest, "data" )
    
    if (DS %in% c("complete.redo", "complete") ) {
      
      fn = file.path( SSSdir, paste( p$speciesofinterest, "complete.rdata", sep="." ) )
      
      if (DS=="complete") {
        W = NULL
        if (file.exists ( fn)) load(fn)
        return (W)
      }
   
     
      if ( p$speciesofinterest == "wolfish" ) {
        wolffish.db( DS="complete.redo", p=p ) # Atlantic wolffish, spec code=5 contains more data from US, etc.
        return (fn)
      }

      sc = extract.from.snowcrab.db( p$speciesofinterest )
      gf = extract.from.groundfish.db( spname=p$speciesofinterest )
      
      ex = c( "trip", "set", "survey", "lat", "lon", "chron", "abundance", "metric", "z", "sal", "t", "sa" ) 
      W = rbind( sc[, ex] , gf[, ex] )


      surveys.with.real.zeros = c( "snowcrab", "groundfish" )
      P = which( W$abundance > 0 ) # presence
      A = which( W$abundance == 0  & W$survey %in% surveys.with.real.zeros ) # absence

      W$presence = NA
      W$presence[ P ] = 1
      W$presence[ A ] = 0

      rm( P, A ) 

      W = habitat.data.prep (X=W, p=p)  # filter and interpolate covariates
      W = habitat.abundance.quantiles( X=W, p=p)   
     
      # when quantiles are lower than a given threshold in surveys with informative relative abundance metrics, consider it absent: ~5% or 1%
      PP = which(W$q < p$habitat.threshold.quantile & W$survey %in% surveys.with.real.zeros )
      if (length(PP)>0) W$presence[ PP ] = 0  # about 132 cases

      
      W = W[ which(is.finite(W$presence + W$abundance )),]

      W$tmean.annual[ which( W$tmean.annual > 12 ) ] = 12
      W$tamp.annual[ which( W$tamp.annual > 15 ) ] = 15
      W$massTot[ which( W$massTot > 200 ) ] = 200
      W$Npred[ which( W$Npred > 120 ) ] = 120
      W$Npred[ which( W$Npred < 10 ) ] = 10
      W$smr[ which( W$smr > 0.0065 ) ] = 0.0065

      dir.create( SSSdir, recursive=T, showWarnings=F )
      save( W, file=fn, compress=T )
      return(fn)
  
    }
  
  }


