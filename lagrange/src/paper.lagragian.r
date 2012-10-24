
# Routines specific to the paper

# ------------------------------------------------
# create data -- annual summariesbroken down by CA 1 and 2 categories
	
	loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
	
  data.location = file.path( project.directory("groundfish"), "data", "2006")

  season = "summer"
  taxa = "maxresolved"

  if (make.new.sm) {
    x = groundfish.db( "set" )
    
    # x$spec = taxa.specid.correct( x$spec, method=taxa )
    x = filter.taxa( x, method=taxa )
    
    x = x[ filter.season( x$julian, period=season, index=T ) , ]

    sm = groundfish.db( "sm.complete" )
    sm = sm[ filter.season( sm$julian, period=season, index=T ) , ]
    # redo ordination to remove non-summer data
    sm = sm[sm$sdepth > 50,]  # some strange depth results

    sm = sm.speciescomposition( sm, x, loc=data.location, from.file=F, type="number", threshold=0.05, fname="summer")

    load( file.path(data.location,"summer.ordination.all.rdata") )
    print( ord$CA$eig/sum(ord$CA$eig)*100 ); plot(ord)
    # load( file.path( data.location, "ordination.all.rdata")  ) # returns "ord"
    # CA1 CA2 CA3   CA4 ...
    # 12.9307779914 11.2711218044  6.3610181500  6.1046173734  5.4339599441
    sm$ca1 = sm$ca1.y
    sm$ca2 = sm$ca2.y  # the originals become *.x ..leave alone

# determine qunatiles
    quant1 = unname( quantile(sm$ca1,  probs=c(0, 1/3, 2/3, 1), na.rm=T ) )
    quant2 = unname( quantile(sm$ca2,  probs=c(0 ,1/3, 2/3, 1), na.rm=T ) )

print(quant1)
print(quant2)
#    quant1:  -2.678767 -0.508786  0.385723  2.624901
#    quant2:  -2.914372 -0.492228  0.475391  2.891280

    sm$ca1.quant = cut(sm$ca1, breaks=quant1,labels=F )
    sm$ca2.quant = cut(sm$ca2, breaks=quant1,labels=F )

    sm = sm[ is.finite(sm$ca1.quant + sm$ca2.quant) ,]
    sm$ca.quant = paste( sm$ca1.quant, sm$ca2.quant, sep="." )
    save (sm, file=file.path(data.location,"sm.summer.rdata"))

  } else {

    load (file.path(data.location,"sm.summer.rdata") )

  }

#-------------------------------------------
# prepare the "byyear" data summary:

  variables = c( get.variables("all"), "area")
  plottimes = c("annual")
  regions = assemblages = sort( unique( sm$ca.quant  ) )

  if (redo.byyear) {
    byyear = ts.getdata(sm=sm, from.file=F, variables, plottimes, regions, do.parallel=T, fname="summer", custom="ca", clusters=c("tethys", "tethys", "tethys", "io", "io", "io", "io" ) )
  } else {
    byyear = ts.getdata(fname="summer", custom="ca")
  }

  byyear$se = sqrt( byyear$variance/byyear$nsets )
  byyear$ub = byyear$mean + byyear$se  # 1 SE
  byyear$lb = byyear$mean - byyear$se  # 1 SE

  # fix order in lattice plots
  reordered = c( "1.1", "2.1", "3.1", "1.2", "2.2",  "3.2", "1.3", "2.3", "3.3")
  relabels = c( "Deep-cold", "Deep-cool", "Deep-warm", "Intermediate-cold", "Intermediate-cool", "Intermediate-warm", "Shallow-cold", "Shallow-cool", "Shallow-warm" )
  byyear$region = factor( byyear$region, levels=reordered, labels=relabels )

  # some options for the plots (lattice and printing)
    cex.main = 1.4
    cex.lab = 1.3
    cex.axis = 1.3
    figure.height=6
    figure.width=6

  require(lattice)
  setup.lattice.options()

  for( log.scale in c(T,F)){
  for (pe in plottimes) {
  for (va in variables) {
    yy.data = which( byyear$variable==va & byyear$period==pe)
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
    blank$yr = 1993.5

    if (log.scale) {  # plots with log transformation

      outdir = "timeseries.geometric.ca"
      blank[c("mean", "ub", "lb")] = globalmean
      yys = sqrt(yy$variance/yy$nsets)
      yrange = range( c( yy$mean+yys, yy$mean-yys ))

    } else {  # untransformed plots

      outdir = "timeseries.ca"
      blank[c("mean", "ub", "lb")] = decode.variable(globalmean, va)
      yys = sqrt(yy$variance/yy$nsets)

      yy$ub = decode.variable(yy$ub, va)
      yy$lb = decode.variable(yy$lb, va)
      yy$mean = decode.variable(yy$mean, va)
      yy$weight = decode.variable(yy$weight, va)

      yy.data = c(yy$mean)
      yy.trimmed.mean = mean(yy.data, na.rm=T, trim=0.2)
      yy.data.sd = sd(yy.data, na.rm=T)
      yy.data.clean = which( yy.data > (yy.trimmed.mean-5*yy.data.sd) &
                             yy.data < (yy.trimmed.mean+5*yy.data.sd) )
      yrange = range( yy.data[yy.data.clean] )
    }

    # fill data with blanks where there is no data to make graphics output format consistent
      for (reg in relabels) {
        if (length( which(yy$region==reg)) == 0) {
          bl = blank
          bl$region = reg
          yy = rbind(yy, bl)
        }
      }

    lattice.plot.region( yy, outdir, fname )  # see below for this function

  }}}




  # ---------------
  # figure -- xy-plot of ca1 and depth
	
	loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
	
  data.location = file.path(gs.datadir, "2006")
  season = "summer"

  load( file.path(data.location,"sm.summer.rdata") )
  sm = sm[sm$sdepth > 50,]
  plot( log10(sm$sdepth), sm$ca1, pch=20, ylab="CA1 (13%)", xlab="Depth (log10(m))" )
    ca1.lm = lm(  sm$ca1 ~ log10(sm$sdepth))
    R2 = paste( "R-square =", round( summary(ca1.lm)$r.squared, 3) )
    text(2.65, 2, R2)
    Pr (dev="png", dname="figures", fname="CA1.depth", trim=T)

  # ---------------
  # figure -- xy-plot of ca2 vs temp

  load( file.path(data.location,"sm.summer.rdata") )
  sm = sm[sm$sdepth > 50,]
  plot( sm$temp, sm$ca2, pch=20, ylab="CA2 (11%)", xlab="Temperature (Celcius)" )
    ca2.lm = lm(  sm$ca2 ~ sm$temp)
    R2 = paste( "R-square =", round( summary(ca2.lm)$r.squared, 3) )
    text(14, -2.2, R2)
    Pr (dev="png", dname="figures", fname="CA2.temperature", trim=T)

  # ---------------
  # figure -- CA ordination

  # load CA results
	
	loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
	
  data.location = file.path(gs.datadir, "2006")

  load( file.path(data.location,"summer.ordination.all.rdata") )
  gstaxa =taxa.db( "gstaxa.rdata")

  names = NULL
  names$spec = as.numeric( names(ord$colsum) )
  names = merge(x=names, y=gstaxa, by="spec", all.x=T, all.y=F, sort=F)
  lnames = as.character(names$namecom)

  plot(ord, type="n", scaling=2, xlim=c(-0.45,0.45), ylim=c(-0,0.2) )
  text(ord, display="sp", labels=lnames, cex=0.8 )  # see ?plot.cca
  Pr (dev="png", dname="figures", fname="CA.species", trim=T, width =10, height =10 )


# --------------------------
# test figures of histograms in lattice format

  load( file.path(data.location,"sm.summer.rdata") )
  sm = sm[sm$sdepth > 50 ,]
  sm = sm[is.finite(sm$smrT) ,]
  xrange = c(0,0.008)
  nint = 60

  y = histogram( ~ smrT | as.factor(ca1.quant)*as.factor(ca2.quant), data=sm,
        xlab="Metabolic intensity",
        type="density", layout=c(3,3), xlim=xrange, nint=nint,
        panel = function(x, ...) {
          panel.histogram(x,  ...)
          panel.mathdensity(dmath=dnorm, col="black", args=list(mean=mean(x),sd=sd(x)))
          panel.abline(v=mean(x), col="gray", ...)
        })
  plot(y)


  load( file.path(data.location,"sm.summer.rdata") )
  sm = sm[sm$sdepth > 50 ,]
  sm = sm[is.finite(sm$mrT) ,]
  xrange = c(10,90)
  nint = 60

  y = histogram( ~ Npred | as.factor(ca1.quant)*as.factor(ca2.quant), data=sm,
        xlab="Metabolic intensity",
        type="density", layout=c(3,3), xlim=xrange, nint=nint,
        panel = function(x, ...) {
          panel.histogram(x,  ...)
          panel.mathdensity(dmath=dnorm, col="black", args=list(mean=mean(x),sd=sd(x)))
          panel.abline(v=mean(x), col="gray", ...)
        })
  plot(y)


