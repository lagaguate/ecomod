

  speciesarea.model.spatial = function( ip=NULL, p=NULL, DS="saved", modeltype=NULL, var=NULL ) {
  
    if (DS=="saved") {
      models = NULL
      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain, p$taxa, p$season, paste(p$data.sources, collapse="."),   p$speciesarea.method, modeltype )
      fn.models =  file.path( ddir, paste("speciesarea.models", var, "rdata", sep=".") )
      if (file.exists( fn.models ) ) load( fn.models)
      return( models )
    }

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns
   
    require(mgcv)
    require(snow)
    require(chron)

    for ( iip in ip ) {
      ww = p$runs[iip,"vars"]
      modeltype = p$runs[iip,"modtype"]

      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain, p$taxa, p$season, paste(p$data.sources, collapse=".")  , p$speciesarea.method, modeltype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("speciesarea.models", ww, "rdata", sep=".") )
    
      P0 = bathymetry.db( p=p, DS="baseline" )  # prediction surface appropriate to p$spatial.domain, already in ndigits = 2
      P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )

      require(mgcv)
      require(multicore)

      ks = speciesarea.db( DS="speciesarea.stats.filtered", p=p )
      
      SC = merge( ks, P0, by="platplon", all.x=T, all.Y=F, sort= F, , suffixes=c("", ".P0") )
      SC$chron = as.chron( as.numeric(string2chron( paste( paste( SC$yr, "Jan", "01", sep="-" ), "12:00:00") )) + SC$julian ) # required for time-dependent lookups
   
      SC$t = NA
      SC$t = habitat.lookup.simple( SC,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 


      rm( P0, ks) ; gc()
  

      if (modeltype=="simple" ) {
        formu = formula( paste( ww, ' ~  s(plon,plat) + s(yr) + s(julian, k=3) ' ))
      }

      if (modeltype=="simple.highdef") { 
        formu = formula ( paste( ww, ' ~   s(plon,plat, k=400) + s(yr) + s(julian,k=3)' ))
      }
          
      if (modeltype=="time.invariant") { 
        SC = habitat.lookup(x=SC, p=p, dist.scale=p$interpolation.distances, datatype="time.invariant" )
        formu = formula( paste( ww, 
          ' ~ s(plon,plat, k=400) + s(yr) + s(julian, k=3) 
            + s(z, k=3 , bs="ts" ) 
            + s(dZ, k=3, bs="ts" )  
            + s(substrate.mean, k=3, bs="ts" ) 
          '            
        ))
      }
    
      if (modeltype=="complex") { 
        loadfunctions("habitat")
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

      if ( ww %in% c("Npred", "Npred.se" ) ) {
          sar.model = function(ww) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit",
              family=gaussian("log") )}
      } else {
          sar.model = function(ww) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit",
              family=gaussian() ) }
      }

      models = sar.model(ww)
      save( models, file=fn.models, compress=T)

      return( fn.models )
    }
  }


