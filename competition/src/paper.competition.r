

# analyses for the inter-specific competition paper
# primary datasets are derived from the groundfish data set: /home/jae/projects/src/groundfish.r

# ------------------  Common initialisation for groundfish 
# ------------------
   
  competitiondir = file.path( projects, "competition" )

  dir.create( competitiondir,  showWarnings=F, recursive =T )
  dir.create( file.path( competitiondir, "data" ),  showWarnings=F, recursive =T )
  
  require(Cairo)
 
  init.files = loadfunctions( c( 
		"common", "bathymetry", "taxonomy", "groundfish", "snowcrab", "temperature", "sizespectrum", "competition" 
	))
  


  # ----------------------- GAM analysis --- see end: "condition.new"


	data.summary.by.year( redo=T, competitiondir=competitiondir, init.files=init.files )


# ------------------
# ------------------




  season = "summer"
  sm = groundfish.db("sm.complete")
  sm = sm[ filter.season( sm$julian, period=season, index=T ) , ]


# Nancy Shackell's extraction
# vars.nancy = c("yr", "lon", "lat", "rmean.allfish" )
# sm = sm[, vars.nancy]
# write.table(sm, file="condition.shackell.csv", sep=";" )

  # --------------------------
  # Condition vs size plots broken down by decade

  data.location = file.path(gs.datadir)
  taxa = c( "all", "forage.fish", "elasmobranchs", "demersal", "large.demersal", "small.demersal",
      "pelagic", "small.pelagic", "flatfish", "commercial", "noncommercial", "gadoid", "cod" )
  base = 2
  time.type = "decadal"
  type = "mass"
  type = "len"

 
 

  if (time.type=="annual") {
    outdir =  file.path( competitiondir, "data","condition.by.size.annual")
    plottimes = "annual"
    layout=c(5,8)
    levels=c(1970:gs.datayear)
    labels=c(1970:gs.datayear)
  } else if ( time.type=="decadal" ) {
    outdir =  file.path( competitiondir, "data", "condition.by.size.decadal" )
    plottimes = "decadal"
    layout=c(1,4)
    levels=c(1970, 1980, 1990, 2000)
    labels=c(1970, 1980, 1990, 2000)
  }
   
    fname = paste( "sizecondition", time.type, type, "rdata", sep="." ) 
    bins =  bins.df( paste("gf", type, sep="."), base , length.out=30)
  
  # ~ 10 min on io for a single pass of taxa="all"
  if (redo.byyear.serial) {
      byyear = condition.by.size.region.year( loc=data.location, taxa=taxa, from.file=F,
        fname=fname, base=base, vname=type, bins=bins,
        variables="residual", plottimes=plottimes, regions=c("4vw", "4x"), season="summer",do.parallel=F )
    } else if (redo.byyear.parallel) {
      clusters=rep("kaos",16 )
      byyear = condition.by.size.region.year( loc=data.location, taxa=taxa, from.file=F,
        fname=fname, base=base, vname=type, bins=bins,
        variables="residual", plottimes=plottimes, regions=c("4vw", "4x"), season="summer", do.parallel=T, clusters=clusters )
  } 
  
  
  byyear = condition.by.size.region.year(loc=data.location, fname=fname, from.file=T)
  byyear$sc = byyear$sizeclass
  byyear$sizeclass = base^bins$mids[byyear$sc]  # convert to kg
#  byyear = byyear[ which(byyear$sizeclass <= 10) ,]# screen out very large

  byyear$sizeclass = log( byyear$sizeclass, base=10 )  # return to base 10

  byyear$mean[byyear$nind < 10 ] = NA

  byyear$se = sqrt( byyear$variance/byyear$nind )

  byyear$ub = byyear$mean + 2*byyear$se  
  byyear$lb = byyear$mean - 2*byyear$se  
#  byyear$ub = byyear$mean + sqrt(byyear$variance)   
#  byyear$lb = byyear$mean - sqrt(byyear$variance)   


  # fix order in lattice plots
  byyear$region = factor( byyear$region, levels=c( "4vw","4x" ), labels=c( "4VW", "4X")  )
  byyear$yr = factor(byyear$yr, levels=levels, labels=labels)


  # some options for the plots (lattice and printing)
    cex.main = 2.5
    cex.lab = 2.5
    cex.axis = 2.5

  require(lattice)
  setup.lattice.options()

  for (tx in taxa ) {
  for (pe in plottimes) {
  for (va in as.character(unique(byyear$variable))) {
  for( re in as.character(unique(byyear$region))) {

    yy.data = which( byyear$variable==va & byyear$period==pe & byyear$region==re & byyear$taxa==tx)
    if (length (yy.data) ==0) next
    yy = byyear[ yy.data, ]

    yy.data.finite = which( is.finite(yy$mean) & is.finite(yy$variance) )
    if (length( yy.data.finite) ==0) next
    yy = yy[ yy.data.finite, ]
    yy.min.nonzero = min(yy$se[which(yy$se>0)], na.rm=T) # an offset to make all data weight nonzero
    yy.max = max(yy$se, na.rm=T)
    yy$weight = (yy.max - yy$se + yy.min.nonzero) / (yy.max+yy.min.nonzero) # max value is 1

    globalmean = mean(yy$mean,na.rm=T)

    blank = yy[1,]
    blank[c("mean", "ub", "lb")] = globalmean
    blank$sizeclass = min(byyear$sizeclass)
    blank$weight = 1/1000

    yys = sqrt(yy$variance/yy$nind)
   
    # fill data with blanks where there is no data to make graphics output format consistent
    for (yrs in as.character(sort(unique(byyear$yr)))) {
      if (length( which(yy$yr==yrs)) == 0) {
        blank$yr = yrs
        yy = rbind(yy, blank)
      }
    }


    yrange = c( -1.5,1.3 )
    bylength = T
    if (bylength) {
     xlab = "Body length (log10; cm)"
     xrange = range( byyear$sizeclass , na.rm=T )
    } else {
     xlab = "Body mass (log10; kg)"
     xrange = log10( c( 0.02, 15 )) #kg
     # xrange = range( byyear$sizeclass , na.rm=T )
    }

    main = paste(va, pe, re, tx)
    lattice.plot.condition.year( yy, outdir, layout=layout, xrange, yrange, xlabel=xrange )  # see below for this function

  }}}}





# ------------ bottom temperatures

	


	figure.condition.new(...) # incomplete

	figure.temperatures()
	figure.size.abundance.log()
	figure.oxygen ()

	species.list()



