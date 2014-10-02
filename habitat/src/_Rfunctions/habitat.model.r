
  habitat.model = function( ip=NULL, p=NULL, DS="saved", vn=NULL, yr=0 ) {
    
    # compute the spatial interpolation model
    
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    
    outdir = file.path( p$project.outdir.root, p$spatial.domain, p$season, p$modtype, "models" )
    dir.create( outdir, showWarnings=FALSE, recursive=TRUE )
    
    if (DS=="saved") {
      models = NULL
      fn.models =  file.path( outdir, paste("models", vn, yr, "rdata", sep=".") )
      if (file.exists( fn.models ) ) load( fn.models)
      return( models )
    }

    if (is.null(ip)) ip = 1:p$nruns
   
    pdat0 = habitat.db( DS=p$project.name, p=p ) 
    pdat0 = habitat.truncate.data( pdat0, p$varstomodel )
    
    if ( p$movingdatawindow == 0 ) {  # no moving time window .. single model 
      for ( iip in ip ) {
        ww = p$runs[iip,"vars"]
        print( p$runs[iip,])
        formu = habitat.model.formula( YY=ww, modeltype=p$modtype, indicator=p$project.name, spatial.knots=p$spatial.knots )
        vlist = setdiff( all.vars( formu ), "spatial.knots" )
        pdat = pdat0[, vlist]
        pdat = na.omit( pdat )
        models = habitat.model.run( ww, pdat, formu, p%optimizer.alternate ) {
        if (models=="model.failure") next() 
        fn.models =  file.path( outdir, paste("models", ww, "rdata", sep=".") )
        save( models, file=fn.models, compress=T)
        print( fn.models )
        rm(models, pdat); gc()
      }
    } 

    # -------------------------

    if ( p$movingdatawindow != 0 ) {  # moving window approach 
      for ( iip in ip ) {
        ww = p$runs[iip,"vars"]
        yr = p$runs[iip,"yrs"]
        print( p$runs[iip,])
        formu = habitat.model.formula( YY=ww, modeltype=p$modtype, indicator=p$project.name, spatial.knots=p$spatial.knots )
        vlist = setdiff( all.vars( formu ), "spatial.knots" )
        pdat = pdat0[, vlist]
        pdat = na.omit( pdat )
        
        yrsw = c( p$movingdatawindow + yr  ) 
        ioo = which( pdat$yr %in% yrsw ) # default year window centered on focal year
        nyrsw = length ( unique( pdat$yr[ ioo ] ) )
        if ( nyrsw  < p$movingdatawindowyears ) {
          for ( ny in 1:5 ) {
            yrsw = c( (min(yrsw)-1), yrsw, (max(yrsw)+1) )
            ioo = which( pdat$yr %in% yrsw ) # five year window
            yrs_selected = sort( unique( pdat$yr[ ioo ] ) )
            nyrsw = length ( yrs_selected )
            if (nyrsw == p$movingdatawindowyears ) break() 
          }
        }
        if (length(ioo) < 200 ) next() 
        pdat = pdat[ioo,]
 
        models = habitat.model.run( ww, pdat, formu, p%optimizer.alternate ) {
        if (models=="model.failure") next() 
     
        fn.models =  file.path( outdir, paste("models", ww, yr, "rdata", sep=".") )
        save( models, file=fn.models, compress=T)
        print( fn.models )
        rm(models, pdat); gc()
      }
    }
    return( "Completed modelling" )
  }      


 


