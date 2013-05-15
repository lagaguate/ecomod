
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
    require(multicore)

    for ( iip in ip ) {
      ww = p$runs[iip,"vars"]
      modeltype = p$runs[iip,"modtype"]
      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("metabolism.models", ww, "rdata", sep=".") )
      SC = metabolism.db( DS="metabolism", p=p )
      
      formu = habitat.lookup.model.formula( YY=ww, modeltype=modeltype, indicator="metabolism" )
          
      vlist = setdiff( all.vars( formu ), "spatial.knots" )
      SC = SC[, vlist]

      if ( ww %in% c("smr", "zn", "zm", "qn", "qm", "Pr.reaction", "Ea", "A" ) ) {
        fmly = gaussian()
      } else {
        fmly = gaussian("log")  # default
      }
      
      metab.model = function(ww, SC, fmly) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit", family=fmly )}


      models = metab.model(ww, SC, fmly)
      save( models, file=fn.models, compress=T)
      print(fn.models)
      rm (models, SC); gc() 
    }
      return( "Done" )

  }      



