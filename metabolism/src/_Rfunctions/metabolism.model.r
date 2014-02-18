
  metabolism.model = function( ip=NULL, p=NULL, DS="saved", modeltype=NULL, var=NULL, yr=1000) {
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
   
    loadlibraries (p$libs)

    for ( iip in ip ) {
      ww = p$runs[iip,"vars"]
      yr = p$runs[iip,"yrs"]
      modeltype = p$runs[iip,"modtype"]
      print( p$runs[iip,])

      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("metabolism.models", ww, yr, "rdata", sep=".") )
      SC = metabolism.db( DS="metabolism", p=p )
      formu = habitat.lookup.model.formula( YY=ww, modeltype=modeltype, indicator="metabolism", spatial.knots=p$spatial.knots )
      vlist = setdiff( all.vars( formu ), "spatial.knots" )
      SC = SC[, vlist]
      SC = na.omit( SC )
 
      yrsw = c( p$movingdatawindow + yr  ) 
      ioo = which( SC$yr %in% yrsw ) # default year window centered on focal year
      nyrsw = length ( unique( SC$yr[ ioo ] ) )
      if ( nyrsw  < p$movingdatawindowyears ) {
        for ( ny in 1:5 ) {
          yrsw = c( (min(yrsw)-1), yrsw, (max(yrsw)+1) )
          ioo = which( SC$yr %in% yrsw ) # five year window
          yrs_selected = sort( unique( SC$yr[ ioo ] ) )
          nyrsw = length ( yrs_selected )
          if (nyrsw == p$movingdatawindowyears ) break() 
        }
      }

      if (length(ioo) < 200 ) next() 
      SC = SC[ioo,]
      
      if ( ww %in% c("smr", "zn", "zm", "qn", "qm", "Pr.reaction", "Ea", "A" ) ) {
        fmly = gaussian()
      } else {
        fmly = gaussian("log")  # default
      }
      
      # first attempt : use bam .. 
      metab.model = function(ww, SC, fmly) { bam( formu, data=SC, na.action="na.omit", family=fmly  )}
      models = try ( metab.model(ww, SC, fmly) )
      if  ( "try-error" %in% class(models) ) { 
        metab.model = function(ww, SC, fmly) { gam( formu, data=SC, optimizer=p$optimizer.alternate, na.action="na.omit", family=fmly )}
        models = try ( metab.model(ww, SC, fmly) )
      }
      if  ( "try-error" %in% class(models) ) next()
      save( models, file=fn.models, compress=T)
      print(fn.models)
      rm (models, SC); gc() 
    }
    return( "Done" )
  }      



