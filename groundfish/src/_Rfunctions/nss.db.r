
  nss.db = function(  DS="", nss.taxa="all", nss.base=2, nss.type="mass", nss.distances=1, nss.stimes=30, clusters="localhost", init.files=NULL, plottimes=NULL, regions=NULL, nss.bins=NULL ) {
  
    if (length( clusters) > 1 ) {
      do.parallel = T
    } else {
      do.parallel = F 
    }

    loc = file.path( project.directory("groundfish"), "data" )
    dir.create( path=loc, recursive=T, showWarnings=F )
    

    if (DS %in% c("nss.det", "nss.det.redo") ) {
      # make the base normalised size spectral statistics summaries
      fn = file.path( project.directory("groundfish"), "R", "nss.rdata") 
      if ( DS=="nss.det" ) {
        load( fn )
        return ( nss )
      }
      
      gsstratum = groundfish.db( "gsstratum" )
      gsstratum = gsstratum[, c("strat","area")]
      gsstratum$area = as.numeric( gsstratum$area )

      set =  groundfish.db( "set.base" )
      set = set[, c("id", "lon", "lat", "chron", "sdepth", "temp", "sal", "strat")]
      set = merge(set, gsstratum, by="strat", all.x=T, all.y=F, sort=F)
#    set = set[1:10,] # debug

      nid = nrow(set)
      nss = NULL
      
      if (!do.parallel) {
        nss = nss.core( set=set, nss.base=nss.base, nss.distances=nss.distances, nss.stimes=nss.stimes, nss.taxa=nss.taxa, nss.type=nss.type, nss.bins=nss.bins, do.parallel=F,  init.files=init.files )
      } else if (do.parallel) {
        pp = prep.parallel.run( clusters, nid )
          nss.pp = clusterApplyLB( pp$cl, pp$ssplt, fun=nss.core, set=set, nss.base=nss.base,
            nss.distances=nss.distances, nss.stimes=nss.stimes, nss.taxa=nss.taxa, nss.type=nss.type, nss.bins=nss.bins, do.parallel=T, init.files=init.files )
        stopCluster(pp$cl)
        for (m in 1:length(nss.pp)) nss = rbind(nss , nss.pp[[m]])
      }

      save(nss, file=fn, compress=T)       # tmp save in case the next crashes
      nss = as.data.frame(nss)

      names(nss) =  c("id", "vname", "taxa", "time", "distance", "nss.rsquared", "nss.df", "nss.b0", "nss.b1",
        "nss.shannon", "nss.evenness", "nss.Hmax" )
      nss = factor2number( nss, c("time", "distance", "nss.rsquared", "nss.df", "nss.b0", "nss.b1","nss.shannon", "nss.evenness", "nss.Hmax" ) )
      nss = factor2character( nss, c("id", "vname", "taxa") )

      save(nss, file=fn, compress=T)

      return ( "Done" )
    }

    # -----------------

    if (DS %in% c("nss", "nss.redo") ) {
      # make the base normalised size spectral statistics summaries
      fn = file.path( project.directory("groundfish"), "R", "set_nss.rdata") 
      if ( DS=="nss" ) {
        load( fn )
        return ( set )
      }
       
      set = groundfish.db( "set.base" )[, c("id", "yr")] 
      
      # update "set"
      for ( distance in c(2,50)) {
      for ( stime in c(10,50)) {
        suffix = paste("all.", distance, "km.", stime, "day", sep="")
        print(suffix)

        nss = nss.db( "nss.det" )
        nss = nss[ which(nss$vname==nss.type & nss$taxa==nss.taxa & nss$time==stime & nss$distance==distance) ,]
        nss = nss[, c( "id", "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon","nss.evenness", "nss.Hmax")]

        set$rsquared = NA
        set$df = NA
        set$b0 = NA
        set$b1 = NA

        set = merge(set, nss, by="id", sort=F, all.x=T, all.y=F, suffixes=c("", paste(".",suffix,sep="")))
        set$rsquared = NULL
        set$df = NULL
        set$b0 = NULL
        set$b1 = NULL

      }}
      set$yr = NULL
      save(set , file=fn, compress=T ) 
      return ( "Done" )
    }


    if (DS %in% c("nss.by.set", "nss.by.set.redo") ) {
      
      # make the base normalised size spectral statistics summaries
      basefilename = file.path( project.directory("groundfish"), "R", "size.spectrum" )
      
      if (DS == "nss.by.set" ) {
        fn = paste( basefilename, nss.taxa, nss.type, nss.base, sep="." )
        load( fn )
        return (ss )
      }

      x =  groundfish.db( "det" )  # mass and length are not transformed
      x = x[ which(x$settype %in% c(1,2,5) ), ]
      # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage),
      #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment,
      #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography

      for (tx in nss.taxa) {

        i.tx = filter.taxa( x=x$spec, method=tx )
        if ( is.null(i.tx) || length(i.tx) < 30) next
        XX = x[ i.tx, ]
        rm( i.tx ); gc()

        for (vname in nss.type) {

          XX.log = log( XX[,vname], base=nss.base )  
          XX$sizeclass = cut( XX.log, breaks=nss.bins$lb, labels=F, include.lowest=F, right=T )

          XX$id=as.factor(XX$id)
          XX$sizeclass=as.factor(XX$sizeclass)
 
          jjj = which( is.finite(XX$sizeclass) )
          XX = XX[jjj,]
          # closed on the right: (x,x]
          # midpoints = (l.bound [2:n.size] + l.bound [1:(n.size-1)] ) /2

          fn = paste( basefilename, tx, vname, nss.base, sep="." )

          ss = NULL
          ss = xtab.2way( xval=XX$cf*XX[,vname], factors=XX[,c("id", "sizeclass")] )
          
          ### ss contains number per km2 broken down by age class and weighted for sa, etc
          save( ss, file=fn, compress=T)
          
          rm (XX, ss); gc()
        }
      }
      return( "Done" )
    }


    if (DS %in% c( "nss.collapse.yr", "nss.collapse.yr.redo") ) {

      fn = file.path(  project.directory("groundfish"), "R", "nss.yr.rdata" )
      if ( DS=="nss.collapse.yr" ) {
        load( fn )
        return (final)
      }

      set.vars = c("id", "strat","yr", "temp", "sal", "sdepth", "lon", "lat", "area" )
      set = groundfish.db( "set.complete" ) [,set.vars]

      ss = nss.db( "nss.by.set", nss.taxa=nss.taxa[1], nss.type=nss.type, nss.base=nss.base )
      variables =  colnames(ss)
      rm (ss); gc()

      final = NULL

      for (tx in nss.taxa) {
        ss = nss.db( "nss.by.set", nss.taxa=tx, nss.type=nss.type, nss.base=nss.base ) 
       
        ss0 = as.matrix(ss)
        offset = min(ss0[which(ss0>0)], na.rm=T) / 100
        rm (ss0); gc()
        ss = log( ss+offset, base=10 ) ## convert to base 10 for stats and plotting
        ss$id = rownames(ss)
        wm = merge ( ss, set, by="id", sort=F, all.x=T, ally=F)
        rm (ss); gc()
      for (va in variables) {  # size classes

      for (ti in plottimes) {
        td = recode.time( wm, ti, vector=T )
        yrs = sort( unique( td$yr ) )

      for (y in yrs) {
        i.y = which( td$yr==y )

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


