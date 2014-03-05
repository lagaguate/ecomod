
  sizespectrum.db = function( DS="", p=NULL ) {
    ### dependency is only groundfish db for now. ... 
 
    if (DS %in% c("sizespectrum.by.set", "sizespectrum.by.set.redo") ) {
      
      # make the base normalised size spectral statistics summaries
      ddir = file.path( project.directory("sizespectrum"), "data" )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      if (DS == "sizespectrum.by.set" ) {
        fn = file.path( ddir, paste(  "size.spectrum", p$nss.taxa, p$nss.type, p$nss.base, sep="." ) )
        load( fn )
        return (ss )
      }

      x =  groundfish.db( "det" )  # mass and length are not transformed
      # x = x[ which(x$settype %in% c(1,2,5) ), ]
      # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage),
      #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment,
      #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
   
      x$spec = taxa.specid.correct( x$spec )

      for (tx in p$nss.taxa) {

        i.tx = filter.taxa( x=x$spec, method=tx)
        if ( is.null(i.tx) || length(i.tx) < 30) next()
        XX = x[ i.tx, ]
        rm( i.tx ); gc()

        for (vname in p$nss.type) {

          XX.log = log( XX[,vname], base=p$nss.base )  
          XX$sizeclass = cut( XX.log, breaks=p$nss.bins$lb, labels=F, include.lowest=F, right=T )

          jjj = which( is.finite(XX$sizeclass + XX$cf) )
          XX = XX[jjj,]
          
          XX$id=as.factor(XX$id)
          XX$sizeclass=as.factor(XX$sizeclass)
 
          # closed on the right: (x,x]
          # midpoints = (l.bound [2:n.size] + l.bound [1:(n.size-1)] ) /2

          fn = file.path( ddir, paste(  "size.spectrum", tx, vname, p$nss.base, sep="." ) )

          ss = NULL
          tt = XX$cf*XX[,vname]
          ss = xtab.2way( xval=tt, factors=XX[,c("id", "sizeclass")] )
          
          ### ss contains number per km2 broken down by age class and weighted for sa, etc
          save( ss, file=fn, compress=T)
          
          rm (XX, ss); gc()
        }
      }
      return( "Done" )
    }


    if (DS %in% c( "sizespectrum.stats", "sizespectrum.stats.redo" ) ) {

      ddir = file.path( project.directory("sizespectrum"), "data"  )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.sizespectrum.rdata" )
      # fn = file.path( ddir, "set.sizespectrum.rdata.tmp" )
        
      if (DS=="sizespectrum.stats") {
        nss = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( nss )
      }
      
      sm = bio.db( "set" )
      sm = sm[ which( sm$data.source=="groundfish") ,]
      sm$area = sm$sa
      sm$sa = NULL

      sm$id = as.character( sm$id )
      smdups = which( duplicated (sm$id ) )
      if (length(smdups) > 0) sm = sm[ -smdups, ]
      sm = sm[ order(sm$id) , ]
      gc()

      nss = NULL
      p$newvars = c( "id",  "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon", "nss.evenness", "nss.Hmax" ) 

      p$nsets = nrow( sm )
      p$nlengthscale = length(p$nss.bins)
      p$ntimescale = length(p$nss.stimes)
     
      if (p$use.bigmemory.file.backing) {
        p$fn.tmp = file.path(  make.random.string( "nss.bigmemory.tmp" ) )
        p$fn.desc = paste( p$fn.tmp, "desc", sep="." )
        nss = big.matrix( nrow=p$nsets, ncol=length(p$newvars), type="double" , init=NA, backingfile=p$fn.tmp, descriptorfile=p$fn.desc )  
      } else {
        nss = big.matrix( nrow=p$nsets, ncol=length(p$newvars), type="double" , init=NA, shared=TRUE )  
      }

      p$bigmem.desc = describe(nss)
  
      p = make.list( list( nsets=1:p$nsets ), Y=p ) 
 
      if ( length( p$clusters) > 1 ) {
        parallel.run( clusters=p$clusters, n=p$nruns, sizespectrum.compute, p=p, sm=sm )
      } else {
        sizespectrum.compute ( p=p, sm=sm )
      }

      nss <- attach.big.matrix( p$bigmem.desc )
      nss = as.data.frame(as.matrix(nss) )
      names(nss) = p$newvars
      
      nss = factor2number( nss, c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1","nss.shannon", "nss.evenness", "nss.Hmax" ) )
      nss = factor2character( nss, c("id") )
     
      nss$id = sort( unique(as.character( sm$id ) ) )  # overwrite

      save(nss, file=fn, compress=T)
   
      if (p$use.bigmemory.file.backing) {
        file.remove( p$fn.tmp )
        file.remove( p$fn.desc )
      }

      return ( "Done" )
    }


    
    # --------------------
    
    if (DS %in% c( "sizespectrum.stats.filtered", "sizespectrum.stats.filtered.redo" ) ) {
 
      ddir = file.path( project.directory("sizespectrum"), "data", p$spatial.domain, p$taxa, p$season )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.sizespectrum.filtered.rdata" )
        
      if (DS=="sizespectrum.stats.filtered") {
        ks = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( ks )
      }

      ks = sizespectrum.db( DS="sizespectrum.stats", p=p )
      sm = groundfish.db( "set.base" )
      ks = merge (ks, sm, by="id", all.x=T, all.y=F, sort= F) 

      ks = lonlat2planar( ks, proj.type=p$internal.projection, ndigits=2 )
      ks$platplon = paste( round( ks$plat ), round(ks$plon), sep="_" )
      
      ks$plon = ks$plat = NULL
      ks$lon = ks$lat = NULL
      
      # check for duplicates
      for ( y in p$yearstomodel ) {
        yy = which (ks$yr == y)
        ii = which( duplicated( ks$id[yy] ) )
        
        if (length(ii) > 0) {
          print( "The following sets have duplicated positions. The first only will be retained" )
          print( ks[yy,] [ duplicates.toremove( ks$id[yy] ) ] )
          ks = ks[ - ii,]
        }
      }
    
      save( ks, file=fn, compress=T )
      
      return (fn) 
 
    }
    
    # --------------------
    if (DS %in% c("sizespectrum.stats.merged", "sizespectrum.stats.merged.redo") ) {
      
      # make the base normalised size spectral statistics summaries
      
      ddir = file.path( project.directory("sizespectrum"), "data" )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "sm_nss.rdata" ) 
   
      if ( DS=="sizespectrum.stats.merged" ) {
        SC = NULL
        if (file.exists( fn) ) load( fn )
        return ( SC )
      }
     
			P0 = bathymetry.db( p=p, DS="baseline" )  # prediction surface appropriate to p$spatial.domain, already in ndigits = 2
			P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )

      sm = sizespectrum.db( DS="sizespectrum.stats.filtered", p=p )
      sm = sm[ which( is.finite(sm$nss.b0) ) ,]
      
			SC = merge( sm, P0, by="platplon", all.x=T, all.Y=F, sort= F)
			SC = SC[ -which(!is.finite( SC$plon+SC$plat ) ) , ]  # a required field for spatial interpolation
		  rm(sm, P0); gc()
      
			SC$chron = as.chron( as.numeric(string2chron( paste( paste( SC$yr, "Jan", "01", sep="-" ), "12:00:00") )) + SC$julian ) # required for time-dependent lookups
   
      if (!exists( "z", SC)) SC$z = NA
			SC$z = habitat.lookup.simple( SC,  p=p, vnames="z", lookuptype="depth", sp.br=p$interpolation.distances ) 
			
      if (!exists( "t", SC)) SC$t = NA
      SC$t = habitat.lookup.simple( SC,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 
	
      SC = habitat.lookup.data( p=p, sc=SC, modtype="default" )

      save(SC, file=fn, compress=T ) 
      return ( "Done" )
    }

  }



