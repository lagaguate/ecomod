  condition.model = function( ip=NULL, p=NULL, DS="saved", modeltype=NULL, var=NULL ) {
    
    # compute the spatial interpolation model
    
    if (DS=="saved") {
      models = NULL
      ddir = file.path( project.directory("condition"), "data", p$spatial.domain, p$season, modeltype )
      fn.models =  file.path( ddir, paste("condition.models", var, "rdata", sep=".") )
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

      ddir = file.path( project.directory("condition"), "data", p$spatial.domain, p$season, modeltype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("condition.models", ww, "rdata", sep=".") )
    
      SC = condition.db( DS="condition", p=p )
      # SC = habitat.lookup.data( p=p, sc=SC, modtype=modeltype )

      formu = habitat.lookup.model.formula( YY=ww, modeltype=modeltype, indicator="condition", spatial.knots=p$spatial.knots )
      vlist = setdiff( all.vars( formu ), "spatial.knots" )
      SC = SC[, vlist]
      SC = na.omit( SC )

      fmly = gaussian()  # default

      cond.model = function(ww, SC, fmly) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit", family=fmly )}
      
      models = cond.model(ww, SC, fmly)
      
      save( models, file=fn.models, compress=T)
      print( fn.models )
      rm(models, SC); gc()
    }
    return( "Done" )
  }      



