
	figure.size.abundance.log = function() {

# ------------ plot size-abundance on log scale

nss.base =2
nss.type = "mass"
nss.taxa = "all"
regions = c("nafo.4vw", "nafo.4x")
plottimes = "decadal"
  levels=c(1970, 1980, 1990, 2000)
    labels=c(1970, 1980, 1990, 2000)
 
if ( nss.type=="mass") bins = bins.df( "gf.mass", nss.base )
if ( nss.type=="len")  bins = bins.df( "gf.len", nss.base )


   # nss.db( DS="nss.collapse.yr.redo", plottimes=plottimes, regions=regions, nss.taxa=nss.taxa, nss.type=nss.type, nss.distances=1 )
    byyear0 = byyear = nss.db( "nss.collapse.yr", plottimes=plottimes, regions=regions, nss.taxa=nss.taxa, nss.type=nss.type, nss.distances=1 )

  byyear$sc = as.numeric(byyear$variable)

  byyear$sizeclass = nss.base^bins$mids[byyear$sc] # convert to kg
  byyear$sizeclass = log( byyear$sizeclass, base=10 )

  byyear$mean[byyear$nset < 5 ] = NA
  byyear$mean[byyear$mean == min(byyear$mean,na.rm=T)  ] = NA  # min-value is the recoded value representing  zero values

  byyear$se = sqrt( byyear$variance/byyear$nsets )

  tmp.sd = sqrt( byyear$variance )
  byyear$ub = byyear$mean + tmp.sd  
  byyear$lb = byyear$mean - tmp.sd  


  # fix order in lattice plots
  byyear$region = factor( byyear$region, levels=regions, labels=c( "4VW", "4X")  )
  byyear$yr = factor(byyear$yr, levels=levels, labels=labels)

  outdir = file.path( competitiondir, "data", "number.by.size.decadal" )

  # some options for the plots (lattice and printing)
    cex.main = 1.4
    cex.lab = 1.3
    cex.axis = 1.3

  require(lattice)
  setup.lattice.options()


  for (tx in nss.taxa ) {
  for (pe in plottimes) {
  for( re in as.character(unique(byyear$region))) {

    yy.data = which( byyear$period==pe & byyear$region==re & byyear$taxa==tx)
    if (length (yy.data) ==0) next
    yy = byyear[ yy.data, ]

    yy.data.finite = which( is.finite(yy$mean) )
    if (length( yy.data.finite) ==0) next
    yy = yy[ yy.data.finite, ]
    yy.max = max(yy$mean, na.rm=T)
    yy$weight = yy$nsets # max value is 1

    globalmean = mean(yy$mean,na.rm=T)

    blank = yy[1,]
    blank[c("mean", "ub", "lb")] = globalmean
    blank$sizeclass = min(byyear$sizeclass)
    blank$weight = 1/1000

#    yys = sqrt(yy$variance/yy$mean)
    yrange = range( c(yy$lb, yy$ub) , na.rm=T )
    xrange = log10( c( 0.05, 15 )) #kg
    # xrange = range( yy$sizeclass )
    
#    yy = yy[ which(yy$mean <=yrange[2] & yy$mean>= yrange[1] ) ,]
#    yy = yy[ which(yy$sizeclass <=xrange[2] & yy$sizeclass>= xrange[1] ) ,]

    # fill data with blanks where there is no data to make graphics output format consistent
    for (yrs in as.character(sort(unique(byyear$yr)))) {
      if (length( which(yy$yr==yrs)) == 0) {
        blank$yr = yrs
        yy = rbind(yy, blank)
      }
    }

    main = paste( pe, re, tx)
    lattice.plot.number.year( yy, outdir, layout=c(1,4), xrange, yrange, xlabel="Body mass (log10; kg)" )

  }}}

	}


