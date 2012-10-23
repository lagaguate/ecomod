

  extract.from.groundfish.db = function( spname ) {

      # groundfish data 
      	
		loadfunctions( "groundfish", functionpattern="load.groundfish.environment.r") 
		loadfunctions( "groundfish", functionpattern="current.year.r") 

      sm = groundfish.db( "sm.base" )
      wf = groundfish.db( "catchbyspecies", taxa=spname )
      wf = wf[ ,c("id", paste( c("totno", "totwgt"), spname, sep=".") ) ]
      names(wf) = c("id", "abundance", "biomass")

      sm = merge (sm, wf, by = "id", sort=F, all.x=T, all.y=F )
      oo = as.data.frame( matrix( unlist( strsplit( sm$id, ".", fixed=T )), ncol=2, byrow=T ))
      names(oo) = c("trip", "set" )
      oo$trip = as.character( oo$trip)
      oo$set = as.numeric( as.character( oo$set ))
      sm = cbind( sm, oo)
      sm$survey = "groundfish"
      sm$metric = "number"
      sm$z = sm$sdepth
      sm$t = sm$temp
      sm$sa = sm$cftow  # combines sa with catchability "corrections"
      sm$abundance [ which( !is.finite( sm$abundance)) ] = 0 # assume to be real zeros as dervied from trawls
      sm$temp = NULL
      sm$sdepth = NULL
      sm$cftow = NULL
      sm$sa = sm$sakm2
      sm$sakm2 = NULL

      return (sm)
  }



