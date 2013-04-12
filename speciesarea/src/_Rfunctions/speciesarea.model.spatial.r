

  speciesarea.model.spatial = function( ip=NULL, p=NULL, DS="saved", modeltype=NULL, var=NULL ) {
  
    if (DS=="saved") {
      models = NULL
      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain, p$taxa, p$season, paste(p$data.sources, collapse="."), p$speciesarea.method, modeltype )
      fn.models =  file.path( ddir, paste("speciesarea.models", var, "rdata", sep=".") )
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
      modeltype = p$runs[iip,"modtype"]
     

      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain, p$taxa, p$season, paste(p$data.sources, collapse=".") , p$speciesarea.method, modeltype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("speciesarea.models", ww, "rdata", sep=".") )
            
      SC = speciesarea.db( DS="speciesarea.stats.merged", p=p )
      SC = habitat.lookup.data( p=p, sc=SC, modtype=modeltype )

      formu = habitat.lookup.model.formula( YY=ww, modeltype=modeltype, indicator="speciesarea" )
 
      fmly = gaussian()  # default
      if ( ww %in% c("Npred", "Npred.se" ) )  fmly = gaussian("log")
       
      sar.model = function(ww, SC, fmly) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit", family=fmly )}

      models = sar.model(ww, SC, fmly)
      save( models, file=fn.models, compress=T)
    }
    return( "Done" )
  }


