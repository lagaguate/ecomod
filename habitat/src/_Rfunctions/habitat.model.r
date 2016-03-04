
  habitat.model = function( ip=NULL, p=NULL, DS="saved", vn=NULL, yr=0 ) {
    # compute the spatial interpolation model

    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    
    outdir = file.path( p$project.outdir.root, p$spatial.domain, p$season, p$modtype, "models" )
    dir.create( outdir, showWarnings=FALSE, recursive=TRUE )
    
    if (DS=="saved") {
      models = NULL
      fn.models =  file.path( outdir, paste("models", vn, "rdata", sep=".") )
      if (file.exists( fn.models ) ) load( fn.models)
      return( models )
    }

    if (is.null(ip)) ip = 1:p$nruns
   
    pdat0 = habitat.db( DS=p$project.name, p=p ) 
    pdat0 = habitat.truncate.data( pdat0, p$varstomodel )
  
    pdat0$z = log(pdat0$z)
    pdat0$tamp = log(pdat0$tamp)
    pdat0$tamp.cl = log(pdat0$tamp.cl)

    for ( iip in ip ) {
        ww = p$runs[iip,"vars"]
        print( p$runs[iip,])
        formu = habitat.model.formula( YY=ww, modeltype=p$modtype, indicator=p$project.name, spatial.knots=p$spatial.knots )
        vlist = setdiff( all.vars( formu ), "spatial.knots" )
        pdat = pdat0[, vlist]
        pdat = na.omit( pdat )
        models = habitat.model.run( ww, pdat, formu, p$optimizer.alternate ) 
        if (models=="model.failure") next() 
        fn.models =  file.path( outdir, paste("models", ww, "rdata", sep=".") )
        save( models, file=fn.models, compress=T)
        print( fn.models )
        rm(models, pdat); gc()
    } 
    return( "Completed modelling" )
  }      


 


