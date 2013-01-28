
  speciescomposition.model = function(ip=NULL, p=NULL, DS="saved", modeltype=NULL, var=NULL ) { 
    
  
    if (DS=="saved") {
      models = NULL
      ddir = file.path( project.directory("speciescomposition"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      fn.models = file.path( ddir, paste("speciescomposition.models", var, "rdata", sep=".") )
      if (file.exists( fn.models ) ) load( fn.models)
      return( models )

    }

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns
   
    require(chron) 
    require(mgcv)
    require(snow)
    
    for ( iip in ip ) {
      ww = p$runs[iip,"vars"]
      modeltype = p$runs[iip,"modtype"]

      ddir = file.path( project.directory("speciescomposition"), "data", p$spatial.domain, p$taxa, p$season, modeltype  )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("speciescomposition.models", ww, "rdata", sep=".") )
			
			SC = speciescomposition.db( DS="speciescomposition.merged", p=p )
			
			if (modeltype=="simple" ) {
        formu = formula( paste( ww, ' ~  s(plon,plat) + s(yr) + s(julian, k=3) ' ))
      }

      if (modeltype=="simple.highdef") { 
        formu = formula ( paste( ww, ' ~   s(plon,plat, k=400) + s(yr) + s(julian,k=3)' ))
      }
    
      if (modeltype=="time.invariant") { 
        loadfunctions ( "habitat")
        SC = habitat.lookup(x=SC, p=p, dist.scale=p$interpolation.distances, datatype="time.invariant" )
        formu = formula( paste( ww, 
          ' ~ s(plon,plat, k=400) + s(yr) + s(julian, k=3) 
            + s(z, k=4 , bs="ts" ) 
            + s(dZ, k=4, bs="ts" )  
            + s(substrate.mean, k=4, bs="ts" ) 
          '            
            )) 
      }
    
    
      if (modeltype=="complex") { 
        loadfunctions ( "habitat")
        SC = habitat.lookup(x=SC, p=p, dist.scale=p$interpolation.distances, keep.lon.lat=TRUE,  datatype="all.data" )

        formu = formula( paste( ww, 
          ' ~ s(plon,plat, k=400) 
            + s(yr, julian ) 
            + s(tmean, k=3, bs="ts") 
            + s(dt.annual, k=3, bs="ts" ) 
            + s(dt.seasonal, k=3, bs="ts" ) 
            + s(tamp.annual, k=3, bs="ts" )
            + s(wmin.annual, k=3 , bs="ts" ) 
            + s(z, k=3 , bs="ts" ) 
            + s(dZ, k=3, bs="ts" )  
            + s(substrate.mean, k=3, bs="ts" ) 
          '      
        ))
      }

      spcomp.model = function(ww) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit", family=gaussian() )}
      models = spcomp.model (ww)
      save( models, file=fn.models, compress=T)

    }

    return( "Done" )
  }



