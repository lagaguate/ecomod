
  condition.model = function( ip=NULL, p=NULL, DS="saved", modeltype=NULL, var=NULL ) {
  
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
    require(multicore)

    for ( iip in ip ) {
      ww = p$runs[iip,"vars"]
      modeltype = p$runs[iip,"modtype"]

      ddir = file.path( project.directory("condition"), "data", p$spatial.domain, p$season, modeltype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("condition.models", ww, "rdata", sep=".") )
    
      SC = condition.db( DS="condition.merged", p=p )
      SC = habitat.lookup.modeltype( p=p, sc=SC, modtype=modeltype )

      formu =  habitat.model.selection( ww, modeltype )
    
      fmly = gaussian("log")  # default
      if ( ww %in% c("smr", "smrA" ) )  fmly = gaussian()

      condition.interpolation.model = function(ww, SC, fmly) { 
        gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit", family=fmly )
      }
      
      models = condition.interpolation.model(ww, SC, fmly)
      save( models, file=fn.models, compress=T)

    }
      return( "Done" )

  }      



