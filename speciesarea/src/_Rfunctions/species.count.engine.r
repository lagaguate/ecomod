
   species.count.engine = function( ip=NULL, p, set, sc ) {
        # define species counting mechanism
      
      if (exists( "init.files", p)) LoadFiles( p$init.files ) 
      if (exists( "libs", p)) RLibrary( p$libs ) 
      if ( is.null(ip) ) ip = 1:p$nruns 
     
      sar <- attach.big.matrix( p$bigmem.desc )
      sar.ny <- attach.big.matrix( p$bigmem.ny.desc )

      coords = c("lon", "lat")
      c0 = 0  # counter
      for ( i in ip ) {
        seti = set[i,]
        t0 = seti$yr
        dist = geosphere::distCosine( seti[, coords], set[,coords] ) / 1000
        cnter = round(i/length(ip) *100) 
        if ( ( cnter-c0 ) > 1 ) {
          print ( paste( cnter, "% complete" ))
          c0 = cnter
        }
        for (k in 1:p$nlengthscale) {
          qid = which(dist <= p$lengthscale[k])  # sets and taxa within target distance
          for (l in 1:p$ntimescale) {
            m = which ( set$yr[qid] %in% (t0 + c(-p$timescale[l] : p$timescale[l])) ) 
            u = l + (k-1)*p$ntimescale
            sar[i, u] = length( unique( sc$spec[ which( sc$id %in% set$id[qid][m] ) ] ) ) # no. species  
            sar.ny[i, u] = length( unique( set$yr[qid][m] ) ) # no. years entering into the count   
          }
        }
      }
      return(  p$bigmem.desc  )
    }


