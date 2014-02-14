
  sizespectrum.model.spatial = function( ip=NULL, p=NULL, DS="saved", modeltype=NULL, var=NULL ) {
  
    if (DS=="saved") {
      models = NULL
      ddir = file.path( project.directory("sizespectrum"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      fn.models =  file.path( ddir, paste("sizespectrum.models", var, "rdata", sep=".") )
      if (file.exists( fn.models ) ) load( fn.models)
      return( models )
    }

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns
   
    require(mgcv)
    require(chron)
    require(multicore)
    require(parallel)  

    for ( iip in ip ) {
      ww = p$runs[iip,"vars"]
      modeltype = p$runs[iip,"mods"]
      ddir = file.path( project.directory("sizespectrum"), "data", p$spatial.domain, p$taxa, p$season, modeltype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn.models =  file.path( ddir, paste("sizespectrum.models", ww, "rdata", sep=".") )
      SC = sizespectrum.db( DS="sizespectrum.stats.merged", p=p )
      formu = habitat.lookup.model.formula( YY=ww, modeltype=modeltype, indicator="sizespectrum", spatial.knots=p$spatial.knots  )
      vlist = setdiff( all.vars( formu ), "spatial.knots" )
      SC = SC[, vlist]
      SC = na.omit( SC )
      fmly = gaussian()
      
      # first attempt : use bam .. as it allows clusters and memory optimized 
      cl <- makeCluster(p$n.cores ) 
      nss.model = function(ww, SC, fmly) { bam( formu, data=SC, na.action="na.omit", family=fmly, cluster=cl, samfrac=0.1, use.chol=TRUE  )}
      models = try ( nss.model( ww, SC, fmly) )
      stopCluster(cl)
      if  ( "try-error" %in% class(models) ) { 
        nss.model = function(ww, SC, fmly) { gam( formu, data=SC, optimizer=c("outer","nlm"), na.action="na.omit", family=fmly )}
        models = try ( nss.model( ww, SC, fmly) )
      }
      if  ( "try-error" %in% class(models) ) next()
      save( models, file=fn.models, compress=T)
      print( fn.models )
      rm(models, SC); gc()
    }
  }



