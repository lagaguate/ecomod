
  metabolism.model = function( ip=NULL, p=NULL, DS="saved", modeltype=NULL, var=NULL ) {
  
    if (DS=="saved") {
      models = NULL
      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      fn.models =  file.path( ddir, paste("metabolism.models", var, "rdata", sep=".") )
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

      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("metabolism.models", ww, "rdata", sep=".") )
      
      P0 = bathymetry.db( p=p, DS="baseline" )  # prediction surface appropriate to p$spatial.domain, already in ndigits = 2
      P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )

      ks = metabolism.db( DS="metabolism.filtered", p=p )
      ks = na.omit(ks)

      # bring back in plon, plat .. is this necessary still?
      SC = merge( ks, P0, by="platplon", all.x=T, all.Y=F, suffixes=c("", ".P0"), sort= F)
      SC$z.P0 = NULL

      SC$chron = as.chron( as.numeric(string2chron( paste( paste( SC$yr, "Jan", "01", sep="-" ), "12:00:00") )) + SC$julian ) # required for time-dependent lookups
 
      SC$t = habitat.lookup.simple( SC,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 


      rm( P0, ks) ; gc()
  

      if (modeltype=="simple" ) {
        formu = formula( paste( ww, ' ~  s(plon,plat) + s(yr) + s(julian, k=3) ' ))
      }

      if (modeltype=="simple.highdef") { 
        formu = formula ( paste( ww, ' ~   s(plon,plat, k=400) + s(z, k=3) + s(t, k=3) + s(yr) + s(julian,k=3)' ))
      }
      
      if (modeltype=="complex") { 
        source ( file.path(project.directory("habitat"), "src", "functions.habitat.r" ) ) 
        SC = habitat.lookup(x=SC, p=p, dist.scale=p$dist.scale, keep.lon.lat=TRUE,  datatype="all.data" )

        formu = formula( paste( ww, 
          ' ~ s(plon,plat, k=400) 
            + s(yr) + s(julian, k=3 ) 
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

      if ( ww %in% c("smr", "smrA" ) ) {
          metab.model = function(ww) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit", family=gaussian() )}
      } else {
          metab.model = function(ww) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit", family=gaussian("log") ) }
      }
      models = metab.model(ww)
      save( models, file=fn.models, compress=T)

    }
  }      



