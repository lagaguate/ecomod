
#-------------------------------------------
# prepare the "byyear" data summary (this is the same as in timeseries.r, but with fewer regions):


data.summary.by.year = function(redo=F, competitiondir="", init.files="" ) {

  variables = c( get.variables("all"), "temp")
  plottimes = c("annual")
  regions = c("4vw", "4x")
  fname="byear.4vw.4x"

  if (redo) {
    # clusters= c("io", "io" ) #
    # clusters= c("tethys", "tethys" ) # ~ 15 min
    # only 2 processors needed as there are 2 regions ... 
    # might fix this to use all processors .. just need to redefine "id"
    byyear = ts.getdata(sm=sm, fname=fname, from.file=F, variables=variables, plottimes=plottimes, regions=regions, do.parallel=F, clusters=clusters, init.files=init.files) 
  }
  
  byyear = ts.getdata(fname=fname)
  byyear = byyear[ which(byyear$region %in% regions) , ]
  # fix order in lattice plots
  byyear$region = factor( byyear$region, levels=c( "4vw","4x" ), labels=c( "4VW", "4X")  )

  # some options for the plots (lattice and printing)
    cex.main = 1.5
    cex.lab = 1.5
    cex.axis = 1.5
    figure.height=8
    figure.width=6

  bad.size.variables = c("rmean.small.pelagic", "rmean.pelagic", "rmean.large.pelagic", "rmean.noncommercial", "pmean.small.pelagic", "pmean.pelagic", "pmean.large.pelagic", "pmean.noncommercial", "mmean.small.pelagic", "mmean.pelagic", "mmean.large.pelagic", "mmean.noncommercial")
  bad.size.data = which( byyear$variable %in% bad.size.variables &  byyear$yr %in% c(1986:1994 ) )

  byyear$mean[ bad.size.data ] = NA

  
  byyear$se = sqrt( byyear$variance/byyear$nsets )
  byyear$ub = byyear$mean + byyear$se  # 1 SE
  byyear$lb = byyear$mean - byyear$se  # 1 SE


  require(lattice)
  setup.lattice.options()

  for( log.scale in c(T,F)){
  for (pe in plottimes) {
  for (va in variables) {
    yy.data = which( byyear$variable==va & byyear$period==pe)
    if (length (yy.data) ==0) next()
    yy = byyear[ yy.data, ]
    
    yy.zeros = which( yy$mean == 0 )
    if (length(yy.zeros) > 0 ) {
      yy$mean[ yy.zeros ] = NA 
    }

    yy.data.finite = which( is.finite(yy$mean) & is.finite(yy$variance) )
    if (length( yy.data.finite) ==0) next()
    yy = yy[ yy.data.finite, ]
    
    
    yy.min.nonzero = min(yy$se[which(yy$se>0)], na.rm=T) # an offset to make all data weight nonzero
    yy.max = max(yy$se, na.rm=T)
    yy$weight = (yy.max - yy$se + yy.min.nonzero) / (yy.max+yy.min.nonzero) # max value is 1
    
    blank = yy[1,]
    blank$yr = 1993.5

    if (log.scale) {  # plots with log transformation

      outdir = file.path( competitiondir, "data", "timeseries.4x.4vw.geometric" )
      globalmean = weighted.mean(yy$mean, yy$weight, na.rm=T)
      blank[c("mean", "ub", "lb")] = globalmean
      yys = sqrt(yy$variance/yy$nsets)
      yrange = range( c( yy$mean+yys, yy$mean-yys ))

    } else {  # untransformed plots

      outdir = file.path( competitiondir, "data", "timeseries.4x.4vw.backtransformed" )
      blank[c("mean", "ub", "lb")] = variable.recode(globalmean, va, direction="backward", db="groundfish")
      yys = sqrt(yy$variance/yy$nsets)

      yy$ub = variable.recode(yy$ub, va, direction="backward", db="groundfish")
      yy$lb = variable.recode(yy$lb, va, direction="backward", db="groundfish")
      yy$mean = variable.recode(yy$mean, va, direction="backward", db="groundfish")
      yy$weight = variable.recode(yy$weight, va, direction="backward", db="groundfish")
      globalmean = weighted.mean(yy$mean, yy$weight, na.rm=T)

      yy.data = c(yy$mean)
      yy.trimmed.mean = mean(yy.data, na.rm=T, trim=0.2)
      yy.data.sd = sd(yy.data, na.rm=T)
      yy.data.clean = which( yy.data > (yy.trimmed.mean-5*yy.data.sd) &
                             yy.data < (yy.trimmed.mean+5*yy.data.sd) )
      yrange = range( yy.data[yy.data.clean] )
    }

    # fill data with blanks where there is no data to make graphics output format consistent
      for (reg in as.character(unique(byyear$region))) {
        if (length( which(yy$region==reg)) == 0) {
          bl = blank
          bl$region = reg
          yy = rbind(yy, bl)
        }
      }

    lattice.plot.region( yy, outdir, layout=c(1,2), sdy=2.5, log=log.scale )  # see below for this function

  }}}

}



