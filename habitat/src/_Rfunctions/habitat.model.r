
  habitat.model = function( ip=NULL, p=NULL, DS="saved", vn=NULL, yr=1000 ) {
    
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
    dr = list()
    for ( ww in p$varstomodel ) {
      dr[[ww]] = quantile( pdat0[,ww], probs=c(0.025, 0.975), na.rm=TRUE ) # use 95%CI
      il = which( pdat0[,ww] < dr[[ww]][1] )
      if ( length(il) > 0 ) pdat0[il,ww] = dr[[ww]][1]
      iu = which( pdat0[,ww] > dr[[ww]][2] )
      if ( length(iu) > 0 ) pdat0[iu,ww] = dr[[ww]][2]
    }

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

      fmly = gaussian()  # default  
      if ( ww %in% c("Npred" ) ) {
        fmly = gaussian("log")
      } 
      if ( ww %in% c("smr", "zn", "zm", "qn", "qm", "Pr.reaction", "Ea", "A" )) {
        fmly = gaussian() 
      }

      mfunc = function(ww, pdat, fmly) { bam( formu, data=pdat, na.action="na.omit", family=fmly )}
      models =  try ( mfunc(ww, pdat, fmly) )
      
      if  ( "try-error" %in% class(models) ) { 
        mfunc = function(ww, pdat, fmly) { gam( formu, data=pdat, optimizer=p$optimizer.alternate, na.action="na.omit", family=fmly )}
        models =  try ( mfunc(ww, pdat, fmly) )
      }
      
      if  ( "try-error" %in% class(models) ) { 
        mfunc = function(ww, pdat, fmly) { gam( formu, data=pdat, na.action="na.omit", family=fmly )}
        models =  try ( mfunc(ww, pdat, fmly) )
      }
 
      if  ( "try-error" %in% class(models) ) next()
      
      fn.models =  file.path( outdir, paste("models", ww, yr, "rdata", sep=".") )
      save( models, file=fn.models, compress=T)
      print( fn.models )
      rm(models, pdat); gc()
    }
    return( "Completed modelling" )
  }      


 


