

  project.gully = project.directory( "gully" )

  workdir = file.path( project.gully, "R" )
  setwd( workdir )
  
	gully.data = file.path( project.gully, "data" )
	gully.data.file = file.path(project.gully, "R", "gully.rdata")
 

	loadfunctions( c( "common", "snowcrab", "gully" ) )

  redo.data.load = F
  if ( redo.data.load ) {
    # gully = gully.db( "biochem.redo" )
    gully = gully.db( "biochem" )
    # pca
    M = gully.db( "biochem.pca.metals", gully )
    #  Pr("png", workdir, "metals")
    O = gully.db( "biochem.pca.organics", gully )
    #  Pr("png", workdir, "organics")
    chem = merge( O$pca, M$pca, by="tripset" )
    gully$G = merge( gully$G, chem, by="tripset", all.x=T, all.y=F )
    save( gully, file=gully.data.file, compress=T ) 
  }

  load( gully.data.file )
  
  # mapping
  map.gullydata( gully )

  
  # Variance components estimation 
  # 1. NLME
  require( nlme )
  require(ape)

  for ( v in c( gully$metals, gully$organics,"PC1.organics", "PC1.metals", "PC2.organics", "PC2.metals" ) ) {
    
    ii = which( is.finite( gully$G[, v] ) )
    iin = length(which(duplicated(gully$G[ii,"individual"] )) )

    if (v %in% c("PC1.organics", "PC1.metals", "PC2.organics", "PC2.metals" ) ){
      ff = as.formula(  paste(  eval(v), " ~ t + z ") )
    } else{
      ff = as.formula(  paste( "log10(",  eval(v), ") ~ t + z ") )
    }
   
    if ( iin > 0 ) {
      fr = as.formula( " ~ 1|cluster/station/individual" )
    } else { 
      fr = as.formula( " ~ 1|cluster/station" )
    }
    
    m = NULL
    m <- try( lme( ff, random =fr, data=gully$G[ii,]  ), silent=T )
    if (class(m) == "try-error" ) next()
    m.varcomp <- varcomp(m, scale=F, cum=F)
    fn = file.path( workdir, paste( v, "png", sep=".") )
    png( fn  )
      print(fn)
      p = plot(m.varcomp, ylab="Proportion", sub=paste( "Total variance of", deparse(ff), "::", signif( m$sigma,3 ) ) ) 
      print(p)
    dev.off()
  }

  # 2. LME4
  require(lme4)
  require(arm)

  for ( v in c( gully$metals, gully$organics,"PC1.organics", "PC1.metals", "PC2.organics", "PC2.metals" ) ) {
   
    ii = which( is.finite( gully$G[, v] ) )
    iin = length(which(duplicated(gully$G[ii,"individual"] )) )

    if (iin > 0) {
      if (v %in% c("PC1.organics", "PC1.metals", "PC2.organics", "PC2.metals" ) ){
        ff = as.formula(  paste(  eval(v), " ~ 1 + t + z + (1|cluster) + (1|station) + (1|individual) ") )
      } else{
        ff = as.formula(  paste( "log10(",  eval(v), ") ~ 1 + t + z + (1|cluster) + (1|station) + (1|individual)") )
      }
    } else { 
      if (v %in% c("PC1.organics", "PC1.metals", "PC2.organics", "PC2.metals" ) ){
        ff = as.formula(  paste(  eval(v), " ~ 1 + t + z + (1|cluster) + (1|station) ") )
      } else{
        ff = as.formula(  paste( "log10(",  eval(v), ") ~ 1 + t + z + (1|cluster) + (1|station)") )
      }
    }

    m = NULL
    m <- try( lmer( ff , data=gully$G[ii,]  ), silent=T )
    VarCorr(m)
    
    kk= mcmcsamp(m, n=1000)
    display(m)

    if (class(m) == "try-error" ) next()
    ss = sigma.hat(m)$sigma
    m.varcomp <-  as.vector(unlist(ss ))
    m.varcomp = (m.varcomp)^2
    names( m.varcomp ) = names(ss)
    
    fn = file.path( workdir, paste( v, "png", sep=".") )
    png( fn  )
      print(fn)
      p = barplot(m.varcomp, ylab="Variance", type="b" ) 
      print(p)
    dev.off()
  }

# variances for log10(Hg) (data==within station , ie. between individual, no replicates for Hg):
#     data   station   cluster 
#0.0493953 0.0341888 0.0528172 

>   display(m)
lmer(formula = ff, data = gully$G[ii, ])
            coef.est coef.se
(Intercept)  5.58     2.69  
t            0.15     0.16  
z           -0.76     0.59  

Error terms:
 Groups   Name        Std.Dev.
 station  (Intercept) 0.18    
 cluster  (Intercept) 0.23    
 Residual             0.22    
---
number of obs: 27, groups: station, 10; cluster, 3
AIC = 22.3, DIC = 3.6
deviance = 6.9 
          coef.est coef.se
(Intercept)  6.50     2.99  
t            0.16     0.17  
z           -0.96     0.66  

>   m.varcomp 
     data   station   cluster 
0.0493953 0.0341888 0.0528172 


  # 3. JAGS
   
  require(rjags)
  require(coda)
    
  loadfunctions( c("common", "bayesian"))

  v = "hepato.mercury.ng.g"  
  ii = which( is.finite( gully$G[, v] ) )
  gdat = list(
    Y = log10( gully$G[ ii, v] ) ,
    C = as.numeric(as.factor( gully$G[ ii, "cluster" ] ) ) , 
    S = as.numeric(as.factor( gully$G[ ii, "station" ] ) ) , 
    I = as.numeric(as.factor( gully$G[ ii, "individual" ] ) ) ,
    R = as.numeric(as.factor( gully$G[ ii, "replicate" ] ) ) ,
    temp =  gully$G[ ii, "t" ] ,
    z =  gully$G[ ii, "z" ],
    N = length( ii )
  )
  gdat$NC = length( unique( gdat$C ) )
  gdat$NS = length( unique( gdat$S ) )
  gdat$NI = length( unique( gdat$I ) )
  gdat$NR = length( unique( gdat$R ) )


  # define bugs/jags model
  # define bugs/jags model
  gmodel.with.replicates = "
model {
  for( i in 1:N ) {
    Y[i] ~ dnorm( Y.hat[i], Y.tau )
    Y.hat[i] <- b0 + bC[ C[i] ] + bS[ S[i] ] + bI[ I[i] ] + bR[ R[i]] + bT*temp[ i ] + bZ*z[ i ] 
  }
  
  for(c in 1:NC){
    bC[c] ~ dnorm( 0, bC.tau )
  }

  for(s in 1:NS){
    bS[s] ~ dnorm( 0, bS.tau )
  }
  
  for(j in 1:NI){
    bI[j] ~ dnorm( 0, bI.tau )
  }
      
  for(r in 1:NR){
    bR[r] ~ dnorm( 0, bR.tau )
  }

  Y.tau  <- pow( Y.sigma, -2)
  Y.sigma ~ dunif(0, 1)
  Y.var <- pow( Y.sigma, 2 ) 
  
  bC.tau <- pow( bC.sigma, -2 )
  bC.sigma ~ dunif(0,1)
  bC.var <- pow( bC.sigma, 2 ) 

  bS.tau <- pow( bS.sigma, -2 )
  bS.sigma ~ dunif( 0,1)
  bS.var <- pow( bS.sigma, 2 ) 
  
  bI.tau <- pow( bI.sigma, -2 )
  bI.sigma ~ dunif( 0,1 )
  bI.var <- pow( bI.sigma, 2 ) 
  
  bR.tau <- pow( bR.sigma, -2 )
  bR.sigma ~ dunif( 0,1 )
  bR.var <- pow( bR.sigma, 2 ) 

  bT ~ dnorm(0, 0.0001)
  bZ ~ dnorm(0, 0.0001)
  b0 ~ dnorm(0, 0.0001)
}
"


  gmodel.without.replicates = "
model {
  for( i in 1:N ) {
    Y[i] ~ dnorm( Y.hat[i], Y.tau )
    Y.hat[i] <- b0 + bC[ C[i] ] + bS[ S[i] ] + bI[ I[i] ] + bT*temp[ i ] + bZ*z[ i ] 
  }
  for(c in 1:NC){
    bC[c] ~ dnorm( bC.mean[c], bC.tau )
    bC.mean[c] ~ dnorm( 0, 0.0001)
  }
  for(s in 1:NS){
    bS[s] ~ dnorm( bS.mean[s], bS.tau )
    bS.mean[s] ~ dnorm( 0, 0.0001)
  }
  for(j in 1:NI){
    bI[j] ~ dnorm( bI.mean[j], bI.tau )
    bI.mean[j] ~ dnorm( 0, 0.0001)
  }
  Y.tau  <- pow( Y.sigma, -2)
  Y.sigma ~ dunif(0, 1)
  Y.var <- pow( Y.sigma, 2 ) 
  bC.tau <- pow( bC.sigma, -2 )
  bC.sigma ~ dunif(0,1)
  bC.var <- pow( bC.sigma, 2 ) 
  bS.tau <- pow( bS.sigma, -2 )
  bS.sigma ~ dunif( 0,1)
  bS.var <- pow( bS.sigma, 2 ) 
  bI.tau <- pow( bI.sigma, -2 )
  bI.sigma ~ dunif( 0,1 )
  bI.var <- pow( bI.sigma, 2 )
 
  bT ~ dnorm(0, 0.0001)
  bZ ~ dnorm(0, 0.0001)
  b0 ~ dnorm(0, 0.0001)
}
"


  gmodel = ifelse(gdat$NR>1, gmodel.with.replicates, gmodel.without.replicates )
  
  nsims = 100000
  thin = 10000

  m = jags.model( file=jags.setup( gmodel ), data=gdat, n.chains=3, n.adapt=floor(nsims/4) )  #initiate and adapt steps
  coef(m)
  
  y = coda.samples(m, variable.names=c("b0",  "bC.sigma", "bS.sigma", "bI.sigma" , "bT", "bZ" ), n.iter=nsims, thin=thin ) # sample from posterior

  densityplot(y)
  jj =   summary( y )
  jj$statistics

  dic.jags(m, n.inter=nsims, thin=thin, type="pD")
  
  autocorr.plot(y)
  geweke.plot(y)
  gelman.plot(y)


# predctions

  X = as.matrix( expand.grid(b0=1, bC.sigma=1, bS.sigma=1, bI.sigma=1, bT=seq(2,6,by=0.1), bZ=seq(4,6,by=0.1) ) )
  Y = as.matrix( y) 

  out = NULL 
  for (i in 1:nrow(X) ) {
    Z = as.data.frame( t(X[i,]))
    Q = Y %*% X[i,] 
    out = rbind( out, cbind(Z,Q) )
  }

  plot( 10^Q~bT, out)
  plot( 10^Q~bZ, out)

