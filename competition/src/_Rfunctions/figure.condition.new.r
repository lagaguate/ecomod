 

    # ---------------------
    # Alternate figures:
    # ---------------------
    
    
    # ---------------------
    # 1. size vs condition

  

figure.condition.new = function(id=NULL, xdet, bins, taxa, regions, plottimes, variables, do.parallel,  init.files ) {
 
  data.location = file.path( competitiondir )
  taxa = c( "all", "elasmobranchs", "demersal", "large.demersal", "small.demersal",
      "pelagic", "small.pelagic", "flatfish", "commercial", "noncommercial", "gadoid", "cod" )
  base = 2
  fname="sizecondition"

    outdir = file.path( competitiondir, "data", "condition.by.size.annual")
    plottimes = "annual"
    layout=c(5,8)
    levels=c(1970:gs.datayear)
    labels=c(1970:gs.datayear)
    
     
    fname = paste( fname, "length", "rdata", sep="." )
    outdir =  file.path( competitiondir, "data", "condition.by.size.annual" )
 regions=c("4vw", "4x")
  season="summer"

 
    variables="residual"
    plottimes="annual"
    season="summer"
    base=2


    # gs.datayear loaded by initialisation scripts

    X = groundfish.db( "det" )
    X = X[ X$settype %in% c(1,2,5) ,] # remove bad sets

      #  1=stratified random, 2=regular survey, 3=unrepresentative(net damage)
      #  4=representative sp recorded(but only part of total catch),
      #  5=comparative fishing experiment, 6=tagging, 7=mesh/gear studies,
      #  8=exploratory fishing, 9=hydrography
    X = X[ , c("id", "spec", "len", "mass", "cf","residual", "mat" )]
    X$mat = as.factor( X$mat)


    # remove extremes from the data 
        ql = quantile( X$len, probs=c(0.01, 0.99), na.rm=T )
        qm = quantile( X$mass, probs=c(0.01, 0.99), na.rm=T )
        

        X = X[ which(X$len> ql[1] & X$len< ql[2] & X$mass> qm[1] & X$mass< qm[2] ) ,]
    
        X = X[ which(X$spec<=1000) , ] # remove inverts
     
    X = filter.taxa( X, method="maxresolved" )
     
    sm = groundfish.db( "sm.base" )
    sm = sm[, c("id","strat","yr", "julian","lon", "lat", "temp", "sdepth", "sal", "oxysat" )]
    sm$region = NA
    for (r in regions ){
      i = filter.region.polygon(x=sm[, c("lon", "lat")], region=r, planar=F )
      sm$region[i] = r
    }
      
 
    gsstratum = groundfish.db( DS="gsstratum" )
    gsstratum = gsstratum[, c("strat", "area", "name")]
    gsstratum$area = as.numeric(gsstratum$area)
    sm = merge(sm, gsstratum, by="strat", all.x=T, all.y=F, sort=F)
    
    X = merge( X, sm , all.x=T, all.y=F, sort=F)
    X = X[ filter.season( X$julian, period=season, index=T ) , ]
    X = X[ which( !is.na( X$strat )) ,]

    X = X[ , c("id", "strat", "area", "yr", "julian", "lon", "lat", "spec", "len", "mass", "cf", "residual", "temp", "sdepth", "sal", "oxysat", "region", "name", "mat" )]
    
    strat = data.frame( strat = unique( X$strat) )
    strat = merge( strat, gsstratum, by="strat", all.x=T, all.y=F, sort=F)
    SS.area = sum(strat$area, na.rm=T )  # scale factor .. ~ total SA asscoaited with the annual survey
    

    rm (sm, gsstratum)

    X$CF = X$cf * ( X$area/SS.area ) 
        
      qcf = quantile( X$CF, probs=c(0.99), na.rm=T )
      X = X[ which(X$CF < qcf[1] ) ,]
    
    X$decade = floor(X$yr / 10) * 10
    X$month = floor(X$julian / 365 * 12) + 1

    X$llen  = log ( X$len )
    X$lmass = log ( X$mass )



    gc()

    X = X[ is.finite(X$residual), ]
    X = X[ which(X$region %in% c("4vw", "4x")) , ]
    X$spec = as.factor( X$spec )

    require( mgcv )
 #   oo = gam( residual~s(yr)+s(len), data=X, weights=CF )
 #   oo = gam( residual~s(yr,len), data=X, weights=CF )
    
 #   oo = gam( residual~s(yr)+s(mass), data=X, weights=CF )
 #   oo = gam( residual ~ s(yr) + s(len)  , data=X )

     oo = gam( residual~ s(yr, llen), data=X , weights=CF )
     summary(oo)
     res = gam2matrix (oo )   # function found at he end of this file
     require( rgl )
     persp3d( x=res$x, y=res$y, z=res$fit, col="green" )



     oo = gam( residual~ s(yr, llen) + s(lon,lat), data=X , weights=CF )
     summary(oo)
     res = gam2matrix (oo )   # function found at he end of this file
     require( rgl )
     persp3d( x=res$x, y=res$y, z=res$fit, col="green" )


     oo = gam( residual~ s(yr, llen ) + s(lon,lat) + s(sdepth) + s(temp), data=X , weights=CF )
     summary(oo)
     res = gam2matrix (oo )   # function found at he end of this file
     require( rgl )
     persp3d( x=res$x, y=res$y, z=res$fit, col="green" )





    
    
		sps = filter.taxa( X,  method="small.pelagic" )

    vw = which(X$region=="4vw" ) 
    x = which(X$region=="4x" ) 

    sset1 = sort( intersect( sps, vw ) )
    sset2 = sort( intersect( sps, x ) )

    oo = gam( residual~ s(yr, mass) , data=X, weights=CF, subset=sset2 ) 
    summary(oo)
    
    dir.create ( outdir )
    fn = file.path( outdir, paste("figure", "small.pelagic", "4VW", "3D", "svg", sep=".") )
    fn = file.path( outdir, paste("figure", "small.pelagic", "4X", "3D", "svg", sep=".") )

    Cairo( file= fn,  type="svg",  dpi=100, width=4, height=4, units="in" )
      plot(oo, pers=T)
    dev.off()
  

  res = gam2matrix (oo )   # function found at he end of this file
  require( rgl )
  persp3d( x=res$x, y=res$y, z=res$fit, col="green" )





  for (isc in 1:length(id)) {
    sc = id[isc]
    print(sc)
    i.xsc = which(xdet$sizeclass==sc)

    for (tx in taxa) {
      i.smtxi = filter.taxa( x=xdet$spec, method=tx )
      i.x = sort(intersect(i.xsc, i.smtxi))
      if (length(i.x) < 30) next
      xdet.tx = xdet[i.x,]
      for (re in regions) {
        uu = NULL
        uu = xdet.tx[ filter.region.polygon(x=xdet.tx[ , c("lon","lat")], region=re) , ]
        
        
    require( mgcv )
    oo = gam( residual~s(yr)+s(len), data=uu, weights=area)
    oo = gam( residual~s(yr)+s(mass), data=uu, weights=area)
     
               } #end year
      } # end plottimes
    } # end regions
  } # end taxa
  } # end size class

  return(byyear)
}


