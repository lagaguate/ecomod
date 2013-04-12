
  sizespectrum.model.spatial = function( ip=NULL, p=NULL, DS="saved", modeltype=NULL, var=NULL ) {
  
    if (DS=="saved") {
      models = NULL
      ddir = file.path( project.directory("sizespectrum"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      fn.models =  file.path( ddir, paste("sizespectrum.models", var, "rdata", sep=".") )
      if (file.exists( fn.models ) ) load( fn.models)
      return( models )
    }

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns
   
    require(mgcv)
    require(snow)
    require(chron)
    require(multicore)

    for ( iip in ip ) {
      ww = p$runs[iip,"vars"]
      modeltype = p$runs[iip,"mods"]

      ddir = file.path( project.directory("sizespectrum"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("sizespectrum.models", ww, "rdata", sep=".") )
 
      SC = sizespectrum.db( DS="sizespectrum.stats.merged", p=p )
      SC = habitat.lookup.data( p=p, sc=SC, modtype=modeltype )

      formu = habitat.lookup.model.formula( YY=ww, modeltype=modeltype, indicator="sizespectrum" )
  
      fmly = gaussian("log")  # default
      if ( ww %in% c( "nss.b0", "nss.b1", "nss.evenness", "nss.Hmax" ) )  fmly = gaussian()
    
      nss.model = function(ww, SC, fmly) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit", family=fmly )}

      models = nss.model(ww, SC, fmly)

      save( models, file=fn.models, compress=T)

    }
  }



