loadfunctions('bayesian')
loadfunctions('utility')
#sp unit I/II
yr = 1970:2011
svMar = c(66738,76915,25489,25008,36059,19989,42865,47986,60970,38186,18588,16299,31970,14153,22715,49090,46724,42515,59583,100141,25687,29214,11410,37127,11919,45977,41180,41049,23111,25553,13739,30485,14931,21611,18350,19714,26652,9338,7970,36761,8811,47053)/1000
yrG = 1990:2011
svG = c(267.3,188.6,208.9,108.9,71,11.3,10.2,26.3,48,13.3,19,21.6,13.5,71.9,14.2,24.4,37.7,24.1,52.8,18.7,58.4,27.8)

l = c(62.4,	63.7,	56.9,	71.3,	44.9,	48.4,	30.3,	22,	20,	16.5,	15.3,	20.3,	19.7,	17.1,	18.7,	17.4,	20.3,	25.2,	27.6,	31,	34.3,	41.53,	41.76,	35.37,	20.46,	6.34,	4.87,	5.13,	5.64,	9.69,	5.77,	4.84,	3.87,	4.31,	3.55,	3.89,	3.84,	2.11,	2.27,	3.18,	3.77,	1.25)

N=1:length(yr)

   
    require(rjags)
    rjags::load.module("dic")
    rjags::load.module("glm")

    sb = list( N= length(N), C = l, I1 = svMar, I2 = svG,sb$yr = yr   )




			Ku<-max(c(sb$I1,sb$I2))
			k <- findMoments(lo=1/Ku,up=1/(Ku*6),l.perc=0.05,u.perc=0.95,dist='lnorm')
			K <- findMoments(lo=Ku,up=(Ku*6),l.perc=0.05,u.perc=0.95,dist='lnorm')
			sb$r.a<- 0.145 ; sb$r.b <- 1/0.069 
			sb$k.a<-k[[1]] ; sb$k.b <- k[[2]]
			sb$q.a<-0.05 ; sb$q.b <- 2
			sb$P0.a <- 0.0001; sb$P0.b <- 2		





#jags

    n.adapt = 200 # burn-in  .. 4000 is enough for the full model but in case ...
    n.iter = 15000 
    n.chains = 3
    n.thin = 100 # use of uniform distributions causes high autocorrelations ? 
    n.iter.final = n.iter * n.thin
    fnres = file.path( project.datadirectory("redfish.unitII"), paste( "surplus.prod.mcmc", 2015,"rdata", sep=".") )
   
    m = jags.model( file=file.path(project.codedirectory('redfish'),'src','bugs','sp3twoI.bug'), data=sb, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all observed CVs


		tomonitor <- c('sd.p','r','K','sd.o','sd.o2','q1', 'q2','B','Imean','P.res','P0')

    tomonitor = intersect( variable.names (m), tomonitor )
    coef(m)
    

    # ----------------

    dic.samples(m, n.iter=n.iter ) # pDIC

    
    # ----------------
    dir.output = file.path(project.datadirectory('redfish.unitII'))
  y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

save(y, file.path(dir.output, 'analysis','spmodel2I.rdata'))

load(file.path(dir.output, 'analysis','spmodel2I.rdata'))
  
  figure.bugs( type="timeseries", vname="biomass", y=y, sb=sb, fn=file.path(dir.output, "biomass.timeseries.png" ) ,save.plot=T) 
  
  figure.bugs( type="timeseries", vname="fishingmortality", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.png" ),save.plot=T ) 
   
  graphics.off() ; x11()
  layout( matrix(c(1,2,3), 3, 1 )); par(mar = c(5, 4, 0, 2))
  for( i in 1:3) hist(y$cv.r[i,,], "fd")
           
