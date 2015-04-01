
# --------------------------------------------------------
# synthetic analysis for the state of the ecosystem report
# --------------------------------------------------------



# data analysis for the ser report

	loadfunctions( "commom" )
	loadfunctions( "ser" )




# --------------------------------------------------------
# synthetic analysis for the state of the ecosystem report
# --------------------------------------------------------

  # read data and reformat into logical groups

    datadir= file.path( project.datadirectory( "ser"), "data" )
    mapdir = file.path( project.datadirectory( "ser"), "maps" )

    bottomT.integral = read.table( file="~/ecomod/ser/data/BtmT4VW_NAO.txt", sep=";", header=T, as.is=T, strip.white=T)
    
    load ( "~/ecomod/speciescomposition/R/4VW_log" )  # species composition of fish -- already log-transformed
    rv4vw = t(VW)
    rv4vw = as.data.frame( cbind( rv4vw, yr=c(1970:2002)) )

    abun = read.table(paste(datadir, "abundance.csv", sep=""), sep=",", header=T)
    atmo = read.table(paste(datadir, "atmospheric.csv", sep=""), sep=",", header=T)
    ecos = read.table(paste(datadir, "ecosystem.csv", sep=""), sep=",", header=T)
    huma = read.table(paste(datadir, "human.csv", sep=""), sep=",", header=T)
    humo = read.table(paste(datadir, "human_other.csv", sep=""), sep=",", header=T)
    ocea = read.table(paste(datadir, "oceanic.csv", sep=""), sep=",", header=T)
    ns.pop = read.table(paste(datadir, "ns.pop.dat", sep="") , header=F)
    humpop = data.frame(yr=c(1970:2002), ns.pop = log10(ns.pop$V1))
    
    seal = read.table(paste(datadir, "seals.csv", sep=""), sep=",", header=T)
    seal = seal[seal$yr <= 2002 ,]  # some numbers are projections

    mixedlayer = read.table(paste(datadir, "mixedlayer.csv", sep=""), sep=",", header=T)
    mixed = mixedlayer[c(1, 2, 6, 10, 14)]  # ignore the normalised and time averages

# merge CA of species associations
#ca = read.table(file="/home/jae/ecomod/speciescomposition/R/years.ca.dat", header=T, sep=",")
 
#ca = ca[,c("CA1", "CA2")]
#    ca$yr = c(1970:2002)

  # merge SA estimates of poor condition
  
  
  # merge stratifed means of various high-level indicators 
    tss = read.table(file=paste(mapdir, "ts.csv", sep=""), header=T, sep=";", as.is=T)

    vars = c( "mmean", "pmean", "mrT", "smrT", "mr", "smr", "ca1", "ca2", 
              "b0", "b1", "rsquared", "mrPvalueT", "mrPvalue", "speccradial", "speczradial", "specpradial")

    timeseries = tss[tss$region=="vw" & tss$taxa=="all" &  tss$variable %in% vars ,]
    k = 10^6
    tssout = xtabs(as.integer(mean*k) ~ as.factor(yr) + as.factor(variable), data=timeseries) / k
    tssout2 = cbind(yr=as.numeric(as.character(row.names(tssout))), tssout )

  # merge invertebrate abundances
    s = read.table(file="/home/jae/ecomod/ser/R/inverts.csv", header=T, sep=",")
    s$snowcrab.kg.h = scale (log10(s$snowcrab.kg.h), center=T, scale=T)
    s$shrimp.gulf.cpue  = scale (log10(s$shrimp.gulf.cpue), center=T, scale=T)
    invert = data.frame(yr=s$yr, invert.cpue=rowMeans(s[,2:3], na.rm=T))

  # merge recruitment estimates
    recruits = read.csv("/home/jae/ecomod/ser/data/recruitment.csv", header=T)
    t0 = 1970
    t1 = 1998
    recruits = recruits[ recruits$yr>=t0 & recruits$yr <=t1,]
    vars = c("cod", "haddock", "silverhake", "pollock")
    recruits[,vars] = log10(recruits[,vars])
    recruits[,vars] = scale(recruits[,vars], center=T, scale=T)
    r1 = rowMeans(recruits[,vars], na.rm=T)
    rec = data.frame(yr=c(t0:t1), grd.recruits=r1)

  # merge potential fecundity
    load("~/data/groundfish/R/potentialfecundity")
    fec = NULL
    fec$fecundity = fmean
    fec$yr = c(1970:2002)

  # landings
  # inflation corrected total landed values (grdfish, pel, invert)
    lndgs = read.table("/home/jae/ecomod/hysteresis/greed.csv", header=T, sep=";")
    

  # merge all data
    vw = NULL
    vw = merge (x=abun, y=atmo, by="yr", all=T)
    vw = merge (x=vw, y=ecos, by="yr", all=T)
    vw = merge (x=vw, y=huma, by="yr", all=T)
    vw = merge (x=vw, y=humo, by="yr", all=T)
    vw = merge (x=vw, y=ocea, by="yr", all=T)
    vw = merge (x=vw, y=seal, by="yr", all=T)
    vw = merge (x=vw, y=mixed, by="yr", all=T)
    vw = merge (x=vw, y=invert, by="yr", all=T)
    vw = merge (x=vw, y=rec, by="yr", all=T)
    vw = merge (x=vw, y=fec, by="yr", all=T)
    vw = merge (x=vw, y=lndgs, by="yr", all=T)
    vw = merge (x=vw, y=humpop, by="yr", all=T)
    
    vw = merge (x=vw, y=tssout2, by="yr", all=T)
    
    vw = merge (x=vw, y=bottomT.integral, by="yr", all=T)
   
    names(vw) = gsub("_", ".", names(vw))

    
  # data transformations
  #   in abiotic data, only storms shows significant skew ..
  #   a log transform does little to help .. left alone
    transf = c("rv.biom.pel", "rv.biom.grd", "rv.num.pel", "rv.num.grd", "greyseal.pups",
                  "cpr.diatoms", "cpr.dino", "cpr.cf1.4", "cpr.cf5.6", "cpr.para.pseudo",
                  "seals.total", "landings.grd", "landings.pel", "landings.inv", "seismic.2D",
                  "seismic.3D" )

    landings =c( "landings.grd", "landings.pel", "landings.inv")
    vw$total.landings = log10(rowSums(vw[,landings], na.rm=T))
    vw[,transf] = log10(vw[,transf])

    vw$cpr.ch = log10(vw$cpr.ch*1000+1)
    vw$Nwells.drilled = log10( vw$Nwells.drilled + 1 )
    vw$cpr.cf = log10 ( 10^vw$cpr.cf1.4 + 10^vw$cpr.cf5.6 )
    vw$area.T.bottom.gt.3C = 1 - vw$area.T.bottom.lt.3C
    vw$area.T.bottom.lt.3C = NULL
    vw$strat.anomaly50.0m = - vw$strat.anomaly0.50m
#    vw$strat.anomaly0.50m = NULL

  # rename a few variables
    names(vw)[which(names(vw)=="relF.grd")] = "relativeF"
    names(vw)[which(names(vw)=="cpr.para.pseudo")] = "cpr.pp"

  # various data groupings
    biotic.abund = c( "rv.biom.grd", "rv.biom.pel", "rv.num.grd", "rv.num.pel",
                      "greyseal.pups", "seals.total")
    biotic.cpr =  c( "cpr.colour", "cpr.ch", "cpr.diatoms", "cpr.dino", "cpr.cf", "cpr.pp")
    biotic.eco = c( "cpr.ratio", "pel.dem.wgt", "pel.dem.num", "Margalef.diversity",
                    "Shannon.diversity", "Bray.Curtis.sim", "dem.meanwgt", "len.at.age.hadd6",
                    "len.at.age.cod6", "len.at.age.poll6", "len.at.age.silvhake6", "condition.Ken",
                    "condition.Jae.grd", "prop.area.pos.cond", "mmean", "pmean", "mrT", "smrT",
                    "ca1", "ca2", "b0", "b1", "rsquared", "mrPvalueT", "speccradial", "speczradial",
                    "specpradial")
    abiotic.mixed =  c(  "Z.ml","T.ml","S.ml","Sigt.ml", "strat.anomaly50.0m")
    abiotic.temp  =  c( "ice.coverage","T.bottom.emerald","T.bottom.misaine","area.T.bottom.gt.3C",
                        "T.sable.annual","SST.halifax","sst.anomaly.satellite", "bottomT", "bottomT.6yr.6", "NAOanomaly.6yr.6", "bottomTanomaly.6yr.6")
    abiotic.currents = c( "sl.anomaly", "rivsum", "vol.source.cil", "gulf.stream.front.dkm",
                          "shelf.front.dkm", "oxygen", "nitrate" )
    abiotic.atmospheric = c( "nao", "storms","stress.total.sable",
                             "tau.x.sable", "tau.y.sable", "tau.a.sable" )
    human.fishing =  c( "landings.grd", "landings.pel", "landings.inv", "relativeF",
                        "area.trawled", "landval.grd", "landval.pel", "landval.inv" )
    human.other =  c( "Nwells.drilled", "seismic.2D", "seismic.3D", "PCB.sealblubber" )

    biotic = c( biotic.abund, biotic.cpr, biotic.eco )
    abiotic = c( abiotic.mixed, abiotic.temp, abiotic.currents, abiotic.atmospheric )
    human = c( human.fishing, human.other)
    vw.high = eco.indices = biotic.eco
    vw.primary = c( biotic.abund, biotic.cpr, abiotic.mixed, abiotic.temp, abiotic.currents,
                    abiotic.atmospheric, human.fishing, human.other )

  # overwrite categories with the actual data
    biotic.abund        = vw[, c("yr", biotic.abund)]
    biotic.cpr          = vw[, c("yr", biotic.cpr)]
    biotic.eco          = vw[, c("yr", biotic.eco)]
    abiotic.mixed       = vw[, c("yr", abiotic.mixed)]
    abiotic.temp        = vw[, c("yr", abiotic.temp)]
    abiotic.currents    = vw[, c("yr", abiotic.currents)]
    abiotic.atmospheric = vw[, c("yr", abiotic.atmospheric)]
    human.fishing       = vw[, c("yr", human.fishing)]
    human.other         = vw[, c("yr", human.other)]

 

# -------------------------------------------------
# plot time-series of all data

t0 = 1960
t0 = 1970
t1 = 2002

b=vw[ vw$yr>= t0 ,]

for (i in 1:dim(b)[2]) {
  plot(c(t0:t1),b[,i],type="b", xlab="year", ylab=colnames(b)[i])
  # dev.print(width=9, height=7)
}


# -------------------------------------------------
# visualize data matrices (correlation matrices and ordinations)

t0 = 1960
t1 = 2002

# all data ..
  bio.view.t( bio.form(vw, t0, t1), title="4VW -- All variables", print=T, t0, t1)
  bio.view( bio.form(rv4vw, t0, t1), title="Groundfish species composition", print=T, t0, t1)

# logical groups
  bio.runall( biotic.abund, title="Abundance of organisms", print=T, t0, t1)
  bio.runall( biotic.cpr, title="CPR data", print=T, t0, t1)
  bio.runall( eco.indices, title="Ecosystem indices", print=T, t0, t1)
  bio.runall( abiotic.mixed, title="Mixed layer characteristics", print=T, t0, t1)
  bio.runall( abiotic.temp, title="Temperatures", print=T, t0, t1)
  bio.runall( abiotic.currents, title="Ocean currents", print=T, t0, t1)
  bio.runall( abiotic.atmospheric, title="Atmospheric characteristics", print=T, t0, t1)
  bio.runall( human.fishing, title="Fishing patterns", print=T, t0, t1)
  bio.runall( human.other, title="Drilling and pollution", print=T, t0, t1)
  bio.runall( vw, title="All indicators in 4VW", print=T, t0, t1)

  biotic = cbind(biotic.abund, biotic.cpr )
  biotic=biotic[,-8]

  human = cbind(human.fishing, human.other)
  human = human[, c(-10)]

  abiotic = cbind(abiotic.temp, abiotic.mixed, abiotic.currents, abiotic.atmospheric)
  abiotic = abiotic[,c(-9, -15, -23)]


# -------------------------------------------------
# mantel-type analyses

t0 = 1960
t1 = 2002

nvars=10
  f = matrix(data=NA, ncol=nvars, nrow=length (bio.flatten( rv4vw, t0, t1 )) )
  f[,1] = bio.flatten( rv4vw, t0, t1 )
  f[,2] = bio.flatten( biotic.abund, t0, t1)
  f[,3] = bio.flatten( biotic.cpr, t0, t1)
  f[,4] = bio.flatten( eco.indices, t0, t1)
  f[,5] = bio.flatten( abiotic.mixed, t0, t1)
  f[,6] = bio.flatten( abiotic.temp, t0, t1)
  f[,7] = bio.flatten( abiotic.currents, t0, t1)
  f[,8] = bio.flatten( abiotic.atmospheric, t0, t1)
  f[,9] = bio.flatten( human.fishing, t0, t1)
  f[,10] = bio.flatten( human.other, t0, t1)

nvars = 3
  f = matrix(data=NA, ncol=nvars, nrow=length (bio.flatten( rv4vw, t0, t1 )) )
  f[,1] = bio.flatten( rv4vw, t0, t1 )
  f[,2] = bio.flatten( biotic.abund, t0, t1)
  f[,3] = bio.flatten( biotic.cpr, t0, t1)

  g = f[!is.na(rowSums(f)),]
  q.lm = lm ( g[,1] ~ g[,c(2:10)] )
  q.step = step(q.lm)
  summary(q.step)


# -------------------------------------------------
# form residual variations figure for various data groups

t0=1940
t1=2002

  bio.image( vw, title="4VW--All Indices", print=T, t0, t1)
  bio.image( vw.primary, title="4VW--Primary Indices", print=T, t0, t1)
  bio.image( vw.high, title="4VW--High-level Indices", print=T, t0, t1)


# ---------------------------------------------------------------------
# extract ordination scores and plot as time-series

t0=1960
t1=2002

# b = bio.form( biotic, t0, t1)
# 
# 
  
  names(b) = rename.variables(oldnames=names(b), lookupnames=make.ser.lookupnames())
  
  b = bio.form( vw, t0, t1 )
#   analyse(b)
  
  tonull = c("smr", "mr", "mrPvalue", "grd.recruits", "cpr.cf1.4", "cpr.cf5.6", "Metabolic discordance", "Fish community condition index (K)", "Fish community condition index (J)", "T.sable.winter", "condition.Jae.pel")
  
  for (i in tonull) b[,i] = NULL
  
  analyse(b,  colscheme="redgreen", addscores=F, sortsequence=T )


# ---------------------------------------------------------------------
# various time-series

  ygrd = 10^vw$landings.grd/10^6
  ypel = 10^vw$landings.pel/10^6
  yinv = 10^vw$landings.inv/10^6

  yall = ygrd+ypel+yinv
  yrs = vw$yr
  lwd = 3

  x11()
  plot( yrs, yall,
        xlim=c(1959,2003), ylim=c(0, max(yall, na.rm=T)),
        cex.axis=1.5, cex.lab=1.5,
        type="l", col="orange", lwd=lwd+1,
        xlab="Year", ylab="Biomass (10^9 kg)"
       )
  lines( yrs, ypel,  col="blue1", lwd=lwd )
  lines( yrs, ygrd, col="cyan2", lwd=lwd  )
  lines( yrs, yinv, col="green1", lwd=lwd  )


  i = which(!is.na(yall))
  csum = cumsum( yall[i])
  csyrs = yrs[i]
  x11()
  plot ( csyrs, csum,
         xlim=c(1959,2003), ylim=c(0, max(csum, na.rm=T)),
         cex.axis=1.5, cex.lab=1.5,
         type="l", col="orange", lwd=lwd+1,
         xlab="Year", ylab="Biomass (10^9 kg)"
  )

  
  def.par <- par(no.readonly = TRUE)# save default, for resetting...

  ## divide the device into two rows and two columns
  ## allocate figure 1 all of row 1
  ## allocate figure 2 the intersection of column 2 and row 2
  
  ncol = 2
  nvars = dim(q)[2]
  nelem = ceiling(nvars/2) * ncol
  m =  matrix( data=c(1:nelem), nrow=ceiling(nvars/ncol), ncol=ncol, byrow = F)
  m1 = layout(m, respect=F)
  layout.show(m1)
  
  for (i in names(q)) {
      x = q$yr
      y = q[,i]
      plot(x, y, type="p", 
         xlim=c(1969.5,2002.5), ylim=range(y),
         cex.axis=1.5, cex.lab=1.5,
         col="red", 
         xlab="Year", 
         ylab="" )
      lines(lowess (x, y, f=0.2 ), col="blue")

 




