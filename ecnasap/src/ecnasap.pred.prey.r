  
  # -------------------------
  # biplots of pelagic vs groundfish
  
    source(file.path(ecnasapdir, "ecnasap.functions.r"))

    set = ecnasap.catches(source="file")

    set$btemp[which(set$btemp > 40)] = NA
    
    set$prey = round(set$prey)
    set$pred2 = round(set$pred2)
  
#    set$pred2[which(set$pred2 > 3000)] = NA
#   set$prey[ which(set$prey > 80) ] = NA
     
    areas = c("nafo.2j3kl", "nafo.3no", "nafo.3p", "nafo.4rs", "nafo.4t", "nafo.4vw", 
              "nafo.4x", "nafo.5y", "nafo.5zew" )
    regions = c("2J3KL", "3NO", "3P", "4RS", "4T", "4VW", "4X", "5Y", "5Zew" )
    variables = c( "allcaught", "grd", "pel", "shark", "pd_anom", "pred1", "pred2", "prey" )
 
    # overwrite the standard list with one tailored for the above
    variable.list.to.transform = function() {
      log.transform = c("allcaught", "grd", "pel", "shark", "pred1", "pred2", "prey")
      return(log.transform)
    }

         
    P = get.time.series (set, areas, v="prey", trim=0.025)
    P$variable = NULL
    
    G = get.time.series (set, areas, v="pred2", trim=0.025)
    G$variable = NULL
    
    Te = get.time.series (set, areas, v="btemp", trim=0.025)
    Te$variable = NULL

    Ti = get.time.series (set, areas, v="yr", trim=0.025)
    Ti$variable = NULL
    
    Sh = get.time.series (set, areas, v="shark", trim=0.025)
    Sh$variable = NULL
  
    All = get.time.series (set, areas, v="allcaught", trim=0.025)
    All$variable = NULL

    Gr = get.time.series (set, areas, v="grd", trim=0.025)
    Gr$variable = NULL
  
    Pel = get.time.series (set, areas, v="pel", trim=0.025)
    Pel$variable = NULL

    M0 = merge(P,   G, by=c("region", "year"), suffixes=c(".p", ".g") )
    M1 = merge(Te, Ti, by=c("region", "year"), suffixes=c(".q", ".yr") )
    M2 = merge(Sh, All, by=c("region", "year"), suffixes=c(".sh", ".all") )
    M3 = merge(Gr, Pel, by=c("region", "year"), suffixes=c(".gr", ".pel") )

    A = NULL
    A = merge(M0, M1, by=c("region", "year") )
    A = merge(A,  M2, by=c("region", "year") )
    A = merge(A,  M3, by=c("region", "year") )
   
#   outfile = "nafo.pred.prey.rdata" # trim=0.1
#   outfile = "nafo.pred.prey.notrim.rdata" # trim=0
    outfile = "nafo.pred.prey.trim025.rdata" # trim=0.025

#    save(A, file=outfile, compress=T)
#    load(outfile)
#    write.table(A, file="temp.csv", sep=";")

    plot(subset(A, select=c("mean.g", "mean.p")))
    points(subset(A, subset=region=="nafo.4t", select=c("mean.g", "mean.p")), col="red", pch=2)
     
    #  iremove1 = which( A$mean.g<1 | A$mean.g > 3 )
    #  iremove2 = which( A$region=="nafo.4rs" &  A$year %in% c(1970, 1981) )
    #  iremove = unique( c(iremove1, iremove2))
    #  A = A[-iremove,]

    
    A = A[ A$mean.g < 3 &  A$mean.g > 1 ,]  # points beyond this range are obvious outliers
    out = NULL
    
    for (ri in 1:length(areas)) {
      r = areas[ri]
      reg_id = regions[ri]
      
      u = A[A$region==r ,]
      u = u[ is.finite(rowSums(u[, c("mean.g", "mean.p")] ) ) ,]

      pred2 = mean(u$mean.g, na.rm=T)
      prey = mean(u$mean.p, na.rm=T)
      btemp = mean(u$mean.q, na.rm=T)
      years = mean(u$mean.yr, na.rm=T)
      shark = mean(u$mean.sh, na.rm=T)
      all = mean(u$mean.all, na.rm=T)
      grd = mean(u$mean.gr, na.rm=T)
      pel = mean(u$mean.pel, na.rm=T)

      y = u$mean.p 
      x = u$mean.g
      w = u$n.p

      tot0 = sum(w)
      reweight.p = 0
      
      # redistribute weights via z-transform
      tolerance = 0.01  # 0.01 % of initial sum of standardised weights 
      for (oo in 1:10) {
       print (oo)
        tot = sum(w)
        v = lm( y ~ x, weights=w )
        res = rstandard (v)
        reweight = dnorm(res) / dnorm(0)  #(z-score scaled from 0 to 1)
        if (oo==1) tolerance = sum(reweight) * tolerance
        converge = abs( sum(reweight) - sum(reweight.p) )
        w = floor(reweight * w)
        reweight.p = reweight
        w = floor( w * tot0/sum(w) )  # constrain total sample size to be constant
        if (converge < tolerance) break
      }
      
      # final model ...
     
      print(w)
      bad = which(w < 1)
      if (length(bad)>0) {
        y = y[-bad]
        x = x[-bad]
        w = w[-bad]
      }
      ce = coef(v)
      slope = ce[2]
      intercept=ce[1]
      su = summary(v)
      p.value =  round(su$coefficients[2,4],3) 
      rP = round(cor(x,y, use="pairwise.complete.obs", method="pearson"), 3)
      rS = round(cor(x,y, use="pairwise.complete.obs", method="spearman"), 3)
         
      plot( x, y, type="p", xlab="Predators", ylab="Prey", col="dark red", pch=20, cex=1.2 ) 
    
      # lowess fit:
#      z = data.frame( cbind(x=x, y=y))
#      oo = order(z$x)
#      z = z[oo,]
#      lines(lowess(x=z$x, y=z$y, f=3/4 ), lty=2, col="red")
      
      xr = range(x)
      
      yr = range(y)
      lines( xr, intercept + slope*xr, col="blue" )
      otext = paste( "NAFO Area", toupper(reg_id), "\n", 
                     "Pearson =", signif(rP,3), "\n", 
                     "P =", signif(p.value,3) 
                   )
      text (xr[1], yr[2], otext, adj=c(0, 1), cex=1.2 )
          
      outfile = paste(r, "pred2.prey", sep=".")
      Pr(dev="pdfpng", dname="pred2.prey", fname=outfile, width=6, height=4)
    
      data = cbind( area=toupper(reg_id), intercept, slope, p.value, rS, rP,
                    btemp, pel, grd, pred2, prey, shark, all, years )
      out = rbind(out, data)
    }
    rownames(out) = NULL
    colnames(out) = c("area", "b", "m", "p", "rS", "rP",
                      "btemp", "pel", "grd", "pred2", "prey", 
                      "shark", "all", "years" )
    out = as.data.frame(out)
    for (i in 2:dim(out)[2]) out[,i]= as.numeric(as.character(out[,i]))
    out$area = toupper(as.character( out$area ))
    
    nafo = read.table("nafo.dat", header=T )
    nafo$area = toupper(as.character( nafo$area ))

    nafo = merge(nafo, out, by="area")

    over.ride.temp.petrie = T
    if (over.ride.temp.petrie) {
      nafo$btemp = c( 1.17, 2.37, 2.96, 3.18, 1.88, 4.85, 7.12, 6.55, 8.75)
    }
    
    # x=(nafo$chla.mg.m3)
    x=(nafo$btemp)
    
    y=nafo$rP
    
    xr = range(x, na.rm=T)
    xr = c(xr[1]-xr[1]*.1, xr[2]+xr[2]*.1)
  
    yr = range(y, na.rm=T)
    yr = c(yr[1]+yr[1]*.1, yr[2]+yr[2]*.1)
  
    plot( x, y, xlim=xr, ylim=yr, 
          type="n", pch=20, col="gray",
          xlab="Temperature (C)", ylab="Correlation coefficient (groundfish vs pelagic)"
        )
    
    insig.p = which(nafo$p>0.05)
    sig.p = which(nafo$p<0.05)
    neg = which(nafo$rP<0)
    pos = which(nafo$rP>0)

    bu = intersect(pos, sig.p) 
    td = intersect(neg, sig.p)
    
    text( x, y, nafo$area, pos=4, col="gray")
    text( x[bu], y[bu], nafo$area[bu], pos=4, col="green")
    text( x[td], y[td], nafo$area[td], pos=4, col="red")
    
    mmod = lm (y~x, w=(1-nafo$p))
    slope = coef(mmod)[2]
    intercept=coef(mmod)[1]
    su = summary(mmod)
    p.value =  round(su$coefficients[2,4],3) 
    r.square = su$r.squared
    lines( x, intercept + slope*x, col="blue" )
    
    otext = paste( " R-square =", signif(r.square,3), "\n", 
                   "P =", signif(p.value,3) 
                   )
    text (xr[1], yr[2], otext, adj=c(0, 1), cex=1.2 )
          
    outfile = paste(r, "pred2.prey.temp", sep=".")
      Pr(dev="pdfpng", dname="final", fname=outfile, width=6, height=6)
    
      
    # from Worm and Myers (2004) paper

    area = c( "Labrador", "N Newfoundland", "Flemish Cap", "N Gulf of St. Lawrence", "E Scotian Shelf", "Gulf of Maine", "Iceland", "Barrents Sea", "Skagerrak")
    area2 = c( "cs-Lab", "cs-N.Nfld", "cs-Flem.C", "cs-NGSL", "cs-ESS", "cs-GM", "cs-Ice", "cs-Bar.S", "cs-Skag")
    lat = c( 55, 52.5, 47.5, 49.83,44.83, 43.5, 66.5, 74, 57.67 )
    lon = c( -58, -53, -45.67, -64, -60, -70, -23, 25, 7.33 )
    temp = c( 0.4, 1.29, 3.07, 4.68, 2.91, 8.97, 3.15, 3.92, 6.5 ) 
    temp.se = c( 0.14, 0.1, 0.12, 0.07, 0.14, 0.22, 0.18, 0.09, 0.1 )
    rP = c( -0.746, -0.911, -0.526, -0.708, -0.856, -0.131, -0.459, -0.412, 0.788  )
    rn = c( 23, 13, 12, 19, 21, 31, 33, 18, 11  ) 
    p  = c( 0, 0, 0.073, 0, 0, 0.485, 0.006, 0.087, 0.002) 
    
    WM = data.frame( (cbind(area, area2, lat, lon, temp, temp.se, rP, rn, p )) )
    WM$area = as.character( WM$area )
    WM$area2 = as.character( WM$area2 )
  
    for (i in 3:dim(WM)[2]) WM[,i]= as.numeric(as.character(WM[,i]))
  
    correct.temps  = T
    if (correct.temps) {
      WM$temp[WM$area2=="cs-NGSL"] = 1.88
      WM$temp[WM$area2=="cs-GM"] = 6.55
    }
  
    x = c( WM$temp, nafo$btemp )
    y = c( WM$rP, nafo$rP )
    z = c( WM$area2, nafo$area )
    w = 1 - c( WM$p, nafo$p )
    
    xr = range(x, na.rm=T)
    xr = c(xr[1]-xr[1]*.1, xr[2]+xr[2]*.1)
  
    yr = range(y, na.rm=T)
    yr = c(yr[1]+yr[1]*.1, yr[2]+yr[2]*.1)
  
    plot( x, y, xlim=xr, ylim=yr, 
          type="n", pch=20, col="gray",
          xlab="Temperature (C)", ylab="Correlation coefficient (pred vs prey)"
        )
 
    
    mmod = lm (y~x, w=w)
    slope = coef(mmod)[2]
    intercept=coef(mmod)[1]
    su = summary(mmod)
    p.value =  round(su$coefficients[2,4],3) 
    r.square = su$r.squared
    lines( x, intercept + slope*x, col="gray", lty=2, lwd=2 )
    
    otext = paste( " R-square =", signif(r.square,3), "\n", 
                   "P =", signif(p.value,3) 
                   )
    text (xr[1], yr[2], otext, adj=c(0, 1), cex=1.2 )
    text( x, y, z, pos=4, col="blue", cex=0.8)
          
    outfile = paste(r, "pred2.prey.temp2", sep=".")
      Pr(dev="pdfpng", dname="final", fname=outfile, width=6, height=4)
    
  
    temp = c( 0.4, 1.29, 3.07, 4.68, 2.91, 8.97, 3.15, 3.92, 6.5 ) 
    area2 = c( "cs-Lab", "cs-N.Nfld", "cs-Flem.C", "cs-NGSL", "cs-ESS", "cs-GM", "cs-Ice", "cs-Bar.S", "cs-Skag")

