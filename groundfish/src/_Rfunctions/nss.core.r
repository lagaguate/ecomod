
  nss.core = function(id=NULL, sm, nss.base, nss.distances, nss.stimes, nss.taxa, nss.type, nss.bins, do.parallel, init.files=init.files ) {
    
    if (!is.null( init.files)) for (i in init.files) source( i )

    # the first index is a list that is passed from the calling prog: in this case "ssplt" (if parallel)
    if (do.parallel) id = as.numeric(id)
    if (!do.parallel) id = c(1:nrow(sm))

    coords = c("lon", "lat")
    ids = sort( unique(as.character( sm$id ) ) )
    nss = NULL

    for (ii in 1:length(id)) {
      i = id[ii]
      if (!do.parallel) print(i)
      sm0 = sm[ which(sm$id==ids[i]),][1,]

      for (di in nss.distances) {
        smdi = geodist.test( point=sm0[coords], locations=sm[,coords], method="great.circle", threshold=di, type="le")
        smd  = sm[smdi,]

        for (ti in nss.stimes) {
          smti = time.test( x0=sm0$chron, x1=smd$chron, threshold=ti, type="le")
          good.id = smd$id[ sort( smti) ]

          for (tx in nss.taxa) {

            for (vname in nss.type) {

              midpoints=nss.bins$mids

              ss = nss.db( "nss.by.set", nss.taxa=tx, nss.type=vname, nss.base=nss.base )
              
              ### add an offset (1% of min nonzero value) and then log transform it
              ss0 = as.matrix(ss)
              offset = min(ss0[which(ss0>0)], na.rm=T) / 100
              ss = log( ss+offset, base=nss.base ) ## convert to same base as X-values
              
              variables = colnames(ss)
              vtest = colMeans(ss[, variables], na.rm=T)
              vmode = which.max( vtest )
              vmin = min(which( vtest > log(offset, base=nss.base )), na.rm=T )
              vmax = max(which( vtest > log(offset, base=nss.base )), na.rm=T )
              vgood = c(vmin:vmax)
              vi = c(vmode:vmax)

              ss$id = rownames(ss)
              ss.i = sort( which( ss$id %in% good.id ) )
              if (length(ss.i)==0) next
              ss = merge( ss[ss.i,], smd, by="id", all.x=T, all.y=F, sort=F )

              # take geometric weighted ( by strata ) means
              v = NULL
              v = data.frame(
                logbaseN = colMeans( ss[,variables] * ss$area, na.rm=T ) / sum(ss$area, na.rm=T) ,
                sc = as.numeric(as.character(variables)) )
              v$sizeclass = midpoints[v$sc]
              v$size = nss.base^v$sizeclass  # return to grams
              v$N = nss.base^v$logbaseN - offset
              v$N = zapsmall( v$N)
              v$N[ which(!is.finite(v$N)) ] = 0
              if (sum(v$N) ==0) next()

              si = shannon.diversity( x=array( v$N[vgood]+offset, dim=c(1,length(vgood))), base=2, getid=F )
              vi = intersect( vi, which(is.finite(v$logbaseN)) )
              lm.r = NULL
              lm.r = try( lm( v$sizeclass[vi] ~ v$logbaseN[vi]  ) )
              if (is.null(lm.r) | class(lm.r)=="try-error" | !is.finite(sum(coef(lm.r))) ) next()
              lm.s = summary( lm.r )
              out = NULL
              out = cbind( ids[i], vname, tx, ti, di, lm.s$r.squared, lm.s$df[2], lm.s$coefficients[1], lm.s$coefficients[2], si )
              
              nss = rbind( nss, out )
              gc()
    }}}}}

    return(nss)
  }




