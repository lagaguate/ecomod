
  sizespectrum.db = function( DS="", p=NULL ) {
   
    
    if (DS %in% c("sizespectrum.by.set", "sizespectrum.by.set.redo") ) {
      
      # make the base normalised size spectral statistics summaries
      ddir = file.path( project.directory("sizespectrum"), "data" )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      if (DS == "sizespectrum.by.set" ) {
        fn = file.path( ddir, paste(  "size.spectrum", p$nss.taxa, p$nss.type, p$nss.base, sep="." ) )
        load( fn )
        return (ss )
      }

      loadfunctions( "groundfish")
      
      x =  groundfish.db( "det" )  # mass and length are not transformed
      x = x[ which(x$settype %in% c(1,2,5) ), ]
      # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage),
      #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment,
      #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
   
      x$spec = taxa.specid.correct( x$spec )

      for (tx in p$nss.taxa) {

        i.tx = filter.taxa( x=x$spec, method=tx)
        if ( is.null(i.tx) || length(i.tx) < 30) next
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
          ss = xtab.2way( xval=tt, factors=XX[,c("id", "sizeclass")], k=10^4  )
          
          ### ss contains number per km2 broken down by age class and weighted for sa, etc
          save( ss, file=fn, compress=T)
          
          rm (XX, ss); gc()
        }
      }
      return( "Done" )
    }


    if (DS %in% c( "sizespectrum.stats", "sizespectrum.stats.redo" ) ) {

      loadfunctions( "groundfish")

      ddir = file.path( project.directory("sizespectrum"), "data"  )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.sizespectrum.rdata" )
      # fn = file.path( ddir, "set.sizespectrum.rdata.tmp" )
        
      if (DS=="sizespectrum.stats") {
        nss = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( nss )
      }
       
      gsstratum = groundfish.db( "gsstratum" )
      gsstratum = gsstratum[, c("strat","area")]
      gsstratum$area = as.numeric( gsstratum$area )

      sm = groundfish.db( "sm.base" )
      sm = sm[, c("id", "lon", "lat", "chron", "sdepth", "temp", "sal", "strat")]
      sm = merge(sm, gsstratum, by="strat", all.x=T, all.y=F, sort=F)
        dbug= FALSE
        if (dbug) {
          oo = which( years( sm$chron)==2012)
          sm= sm[oo,]
        }


      nss = NULL
      # p$newvars = c( "id",  "time", "distance", "nss.rsquared", "nss.df", "nss.b0",
      #  "nss.b1", "nss.shannon", "nss.evenness", "nss.Hmax" ) 
      p$newvars = c( "id",  "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon", "nss.evenness", "nss.Hmax" ) 

      p$nsets = nrow( sm )
      p$nlengthscale = length(p$nss.bins)
      p$ntimescale = length(p$nss.stimes)
     
      require(bigmemory)


      p$fn.tmp = file.path(  make.random.string( "nss.bigmemory.tmp" ) )
      p$fn.desc = paste( p$fn.tmp, "desc", sep="." )
      nss = big.matrix( nrow=p$nsets, ncol=length(p$newvars), type="double" , init=NA, backingfile=p$fn.tmp, descriptorfile=p$fn.desc )  
      p$bigmem.desc = describe(nss)


      if ( length( p$clusters) > 1 ) {
        # parallel mode
        # bigmemory's backingdir does not seem to be working? ... defaulting to working directory
        require(snow)
        cl = makeCluster( spec=p$clusters, type="SOCK" )
        ssplt = lapply( clusterSplit( cl, 1:p$nsets ), function(i){i} )
        clusterApplyLB( cl, ssplt, fun=sizespectrum.compute, p=p, sm=sm )
        stopCluster( cl )
       
      } else { 
        # serial mode 
        sizespectrum.compute ( p=p, sm=sm )
      }

      nss <- attach.big.matrix( p$bigmem.desc )
      nss = as.data.frame(as.matrix(nss) )
      names(nss) = p$newvars
      
      nss = factor2number( nss, c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1","nss.shannon", "nss.evenness", "nss.Hmax" ) )
      nss = factor2character( nss, c("id") )
     
      nss$id = sort( unique(as.character( sm$id ) ) )  # overwrite

      save(nss, file=fn, compress=T)
   
      file.remove( p$fn.tmp )
      file.remove( p$fn.desc )

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
      sm = groundfish.db( "sm.base" )
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
      
      require(chron)

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
		
      save(SC, file=fn, compress=T ) 
      return ( "Done" )
    }

   # --------------------

    if (DS %in% c( "sizespectrum.collapse.yr", "sizespectrum.collapse.yr.redo") ) {

      ### not used ?? -- a relic of when this function existed within the groundfish.db ? 

      loadfunctions( "groundfish")  
      
      fn = file.path(  project.directory("sizespectrum"), "data", "nss.yr.rdata" )
      if ( DS=="sizespectrum.collapse.yr" ) {
        load( fn )
        return (final)
      }
 
      sm.vars = c("id", "strat","yr", "temp", "sal", "sdepth", "lon", "lat", "area" )
      sm = groundfish.db( "sm.complete" ) [,sm.vars]

      ss = sizespectrum.db( DS="sizespectrum.by.set", p=p )
      variables =  colnames(ss)
      rm (ss); gc()

      final = NULL

      for (tx in p$nss.taxa) {
        ss = sizespectrum.db( DS="sizespectrum.by.set", p=p ) 
       
        ss0 = as.matrix(ss)
        offset = min(ss0[which(ss0>0)], na.rm=T) / 100
        rm (ss0); gc()
        ss = log( ss+offset, base=10 ) ## convert to base 10 for stats and plotting
        ss$id = rownames(ss)
        wm = merge ( ss, sm, by="id", sort=F, all.x=T, ally=F)
        rm (ss); gc()
      for (va in variables) {  # size classes

      for (ti in plottimes) {
        td = recode.time( wm$yr, ti, vector=T )
        yrs = sort( unique( td ) )

      for (y in yrs) {
        i.y = which( td==y )

        for (re in regions) {
          i.re = filter.region.polygon(wm, re)
          ww = wm[ intersect(i.y, i.re) ,]
          strat = sort(unique(as.character(ww$strat)) )
          nstrat = length(strat)
          if (length(nstrat) == 0 ) next
          out = NULL
          for ( j in 1:nstrat ) {
            q = which(ww$strat == strat[j])
            if (!is.null(q)) {
              c = as.data.frame( cbind(
                    strat[j],
                    wtd.mean(ww[q,va], ww$area[q], normwt=T, na.rm=T),
                    wtd.var( ww[q,va], ww$area[q], normwt=T, na.rm=T),
                    sum(ww$area[q], na.rm=T)
                  ) )
              out = rbind (out, c)
            }
          }
          if (is.null(out)) next
          colnames(out) <- c( "strat", "mean", "var", "sumwgt" )
          for (i in 2:(dim(out)[2]))  out[,i] = as.numeric(as.character(out[,i]))

          res = data.frame( cbind(
                  yr = y, region=re, variable=va, taxa=tx, period=ti,
                  mean = wtd.mean(out$mean, out$sumwgt, normwt=T, na.rm=T),
                  variance = wtd.var(out$mean, out$sumwgt, normwt=T, na.rm=T),
                  nsets = length(is.finite(ww[,va])),
                  totalarea = sum(out$sumwgt, na.rm=T)
                ))

          final = rbind( final, res )

        } # end region
      } # end yr
      } # end plottime
      } # end variable
      } # taxa

      numbers = c("yr", "mean", "variance", "nsets", "totalarea")
      for (i in numbers) final[,i] = as.numeric(as.character(final[,i]))

      save( final, file=fn, compress=T)

      return ( "Done" )
    }


  }



