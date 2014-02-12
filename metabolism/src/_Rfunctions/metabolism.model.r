
  metabolism.model = function( ip=NULL, p=NULL, DS="saved", modeltype=NULL, var=NULL, yr=1000 ) {
    # year = 1000 to indicicate all years
    if (DS=="saved") {
      models = NULL
      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      fn.models =  file.path( ddir, paste("metabolism.models", var, yr, "rdata", sep=".") )
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
      yr = p$runs[iip,"years"]
      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("metabolism.models", ww, yr, "rdata", sep=".") )
      SC = metabolism.db( DS="metabolism", p=p )
      ioo = which( SC$yr == yr )
      if (length(ioo) < 50 ) next() 
      SC = SC[ioo,]

      formu = habitat.lookup.model.formula( YY=ww, modeltype=modeltype, indicator="metabolism", spatial.knots=p$spatial.knots )
          
      vlist = setdiff( all.vars( formu ), "spatial.knots" )
      SC = SC[, vlist]

      if ( ww %in% c("smr", "zn", "zm", "qn", "qm", "Pr.reaction", "Ea", "A" ) ) {
        fmly = gaussian()
      } else {
        fmly = gaussian("log")  # default
      }
      
      metab.model = function(ww, SC, fmly) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit", family=fmly )}

      models = try ( metab.model(ww, SC, fmly) )
      if  ( "try-error" %in% class(models) ) next()
      save( models, file=fn.models, compress=T)
      print(fn.models)
      rm (models, SC); gc() 
    }
      return( "Done" )

  }      



