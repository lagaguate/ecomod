
  extract.from.snowcrab.db = function( spname ) {

     #  snow crab trawl-based data tables

      loadfunctions( "snowcrab", functionpattern="initialise.local.environment.r"  ) 
      loadfunctions( "taxonomy" )
	
      tx = taxa.db( "complete" )
      txi = tx[ grep( spname,  tx$name.common, ignore.case=T ) ,]
      
      # if (nrow(txi) != 1 ) stop( txi)

      wf = snowcrab.db ( DS="cat.georeferenced" ) 
      
      # wf$spec = taxa.specid.correct( wf$spec, method=spname )
      wf = filter.taxa( x=wf, method=spname )
      wf = wf[ , c("trip", "set", "totno" ) ]
      names(wf) =  c("trip", "set", "abundance" )

      sc = snowcrab.db ( DS="set.complete" ) 
      sc = sc[ sc$yr >=2004 ,]
      sc = merge( sc, wf, by=c("trip", "set"), all.x=T, all.y=F )
      sc$survey = "snowcrab"
      sc$metric = "number"
      sc$sal = NA
      sc$abundance[ which( !is.finite( sc$abundance ) ) ] = 0  # trawl-based NA==0
      sc$z = exp( sc$z )

      return (sc)
  }


