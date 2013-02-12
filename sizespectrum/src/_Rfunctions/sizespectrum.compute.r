
  sizespectrum.compute = function( ip=NULL, p=NULL, sm=NULL ) {
        
    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
   
    require(chron)
    require( bigmemory )
    
    coords = c("lon", "lat")
    
    nss <- attach.big.matrix( p$bigmem.desc )

    id = 1:p$nsets
    ids = sort( unique(as.character( sm$id ) ) )
    for (ii in 1:length(id)) {
      i = id[ii]
      sm0 = sm[ which(sm$id==ids[i]),][1,]
        smdi = geodist.test( point=sm0[coords], locations=sm[,coords], method="great.circle", threshold=p$nss.distances, type="le")
        smd  = sm[smdi,]

        tdiff = abs( as.numeric(sm0$chron  ) - as.numeric(smd$chron))
        smti = which(tdiff <= p$nss.stimes)

        good.id = smd$id[ sort( smti) ]

        midpoints=p$nss.bins$mids

        ss = sizespectrum.db( DS="sizespectrum.by.set", p=p )
        
        ### add an offset (1% of min nonzero value) and then log transform it
        ss0 = as.matrix(ss)
        offset = min(ss0[which(ss0>0)], na.rm=T) / 100
        ss = log( ss+offset, base=p$nss.base ) ## convert to same base as X-values
        
        variables = colnames(ss)
        vtest = colMeans(ss[, variables], na.rm=T)
        vmode = which.max( vtest )
        vmin = min(which( vtest > log(offset, base=p$nss.base )), na.rm=T )
        vmax = max(which( vtest > log(offset, base=p$nss.base )), na.rm=T )
        vgood = c(vmin:vmax)
        vi = c(vmode:vmax)

        ss$id = rownames(ss)
        ss.i = sort( which( ss$id %in% good.id ) )
        if (length(ss.i)==0) next
        ss = merge( ss[ss.i,], smd, by="id", all.x=T, all.y=F, sort=F )

        # take geometric weighted ( by strata ) means
        v = NULL
        v = data.frame(
          logbaseN = colMeans( ss[,variables] * ss$area, na.rm=T ) / sum(ss$area, na.rm=T),
          sc = as.numeric(as.character(variables))
        )
        
        v$sizeclass = midpoints[v$sc]
        v$size = p$nss.base^v$sizeclass  # return to grams
        v$N = p$nss.base^v$logbaseN - offset
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
        out = cbind( i, lm.s$r.squared, lm.s$df[2], lm.s$coefficients[1], lm.s$coefficients[2], si )
        
        nss[i,] = out 
        gc()
      }
     
    return ( p$bigmem.desc )
  }


