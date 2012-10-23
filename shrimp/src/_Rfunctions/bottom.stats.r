

# -----------------------------------------------------------------------
# calculate mean bottom temperatures and depths ..
# two methods : 1. look for maximum depth and associated temperature
#               2. average temperature once a maximum depth is attained

  bottom.stats = function(x, threshold=1) {
   
   # threshold is the sd to use to detect movement off the bottom

   # x = factor2number( x, c("depth", "temperature")) 
   # x$chron = string2chron( x$chron )
   x$chron = x$jul

    error = ifelse( is.na( x$chron[1]), "Ambiguous time format", "" )
    if ( length( which( is.finite( x$temperature+x$depth))) < 10 ) {
      error = paste(error, "temp and/or depth missing" )
    }

    out = NULL
    Tmin = min(x$temperature, na.rm=T)
    nD = length(which(is.finite( x$depth ) ) )
    
    out = data.frame( cbind( z=NA, t=Tmin, zsd=NA, tsd=NA, n=nD, t0=NA, t1=NA, z0=NA, z1=NA) )
    if (nD < 10) {
      out = data.frame( cbind( z=NA, t=Tmin, zsd=NA, tsd=NA, n=nD, t0=x$t0[1], t1=NA, z0=xdepth[1], z1=NA) )
     return(out) 
    }

    bottom = which.max(x$depth)
    Zx = x$depth[bottom]
      
      # identify a core area where trawling is occurring for certain
        x = x[x$depth >  round(Zx*0.8) ,]  # shallower that 80% of max depth
        x = x[(Zx - x$depth) < 50 ,] # choose the deepest points (within 50m)
        ndata = nrow(x)
        midpoint = round(ndata/2)
        range = round(ndata/8)  # number of datapoints to examine
        x$tinc = c(1:ndata)
        q = x[c(max(1,(midpoint-range)):min(ndata,(midpoint+range))),]
        datalen = dim(q)[1]
      
      if (datalen < 7 ) { 
      out = data.frame( cbind( z=NA, t=Tmin, zsd=NA, tsd=NA, n=nD, t0=x$t0[1], t1=NA, z0=xdepth[1], z1=NA) )
	return(out)
        }

        # determine linear trend and then remove the trend
        lm.q = lm(depth ~ tinc, data=q)
        x$pred = coefficients(lm.q)[2] * x$tinc + coefficients(lm.q)[1]
        x$dx = abs(x$pred - x$depth)
          
        # test two directions for increase in sd as test of leaving bottom
          bindex = NULL
          for (i in c((midpoint-1):1)) {
            m = sd(x$dx[midpoint:i])
            if (!is.null(m) & (m < threshold)) bindex = c(bindex,i)
          }
          for (i in c((midpoint+1):ndata)) {
            m = sd(x$dx[(midpoint):i])
            if (!is.null(m) & m < threshold) bindex = c(bindex,i)
          }
          if (length(bindex) > 3) {
            bindex = sort(bindex)
            y = x[bindex,]
            t0 = as.character(y$chron[1])
            t1 = as.character(y$chron[length(bindex)])
            z0 = as.character(y$depth[1])
            z1 = as.character(y$depth[length(bindex)])
            z = mean( y$depth, trim=0.05)
            zsd = sd(y$depth)
            t = mean( y$temperature, trim=0.05)
            tsd = sd( y$temperature)
            n = length(y$depth)
            out =data.frame( cbind(z=z, t=t, zsd=zsd, tsd=tsd, n=n, t0=t0, t1=t1, z0=z0, z1=z1) )
          } else {
#out=bottom.stats(x,threshold=5)
        }
    
    return (out)
  }


