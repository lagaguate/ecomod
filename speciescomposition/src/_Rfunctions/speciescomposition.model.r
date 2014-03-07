
  speciescomposition.model = function(ip=NULL, p=NULL, DS="saved", modeltype=NULL, var=NULL, yr=1000) { 
    
    if (exists( "init.files", p)) loadfilelist( p$init.files ) 
    if (exists( "libs", p)) loadlibraries( p$libs ) 
    if (is.null(ip)) ip = 1:p$nruns

    if (DS=="saved") {
      models = NULL
      ddir = file.path( project.directory("speciescomposition"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      fn.models = file.path( ddir, paste("speciescomposition.models", var, yr, "rdata", sep=".") )
      if (file.exists( fn.models ) ) load( fn.models)
      return( models )
    }

    for ( iip in ip ) {
      ww = p$runs[iip,"vars"]
      modeltype = p$runs[iip,"modtype"]
      yr = p$runs[iip,"yrs"]
      print( p$runs[iip,])
  
      ddir = file.path( project.directory("speciescomposition"), "data", p$spatial.domain, p$taxa, p$season, modeltype  )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("speciescomposition.models", ww, yr, "rdata", sep=".") )
			SC = speciescomposition.db( DS="speciescomposition.merged", p=p )
      formu = habitat.lookup.model.formula( YY=ww, modeltype=modeltype, indicator="speciescomposition", spatial.knots=p$spatial.knots )
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
  
      fmly = gaussian()
      # first attempt : use bam .. as it allows clusters and memory optimized 
      spcomp.model = function(ww, SC, fmly) { bam( formu, data=SC, na.action="na.omit", family=fmly   )}
      models = try ( spcomp.model (ww, SC, fmly) )
      if  ( "try-error" %in% class(models) ) { 
        spcomp.model = function(ww, SC, fmly) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit", family=fmly )}
        models = try ( spcomp.model (ww, SC, fmly) )
      }
      if  ( "try-error" %in% class(models) ) next()
       
      save( models, file=fn.models, compress=T)
      print( fn.models )
      rm(models, SC); gc()
    }

    return( "Done" )
  }



