

  source(paste(srcdir, "snowcrab.functions.r", sep=""))

  snowcrab = 2526
  
  res = "med"
  vars = c("depth", "number")
  times = c("annual")
  period = "ALL"
  taxa = "spec"
  id = snowcrab
  
  region = c("vwx", "vw", "v", "w", "x")

  xsm = "sets"  # name of database table to store set based means
  
  # flags
    make.all = T
    make.base = T
    make.catches = T
    make.condition = T

  # make base data tables ... first pass data analysis of the whole shelf ... only need to do once
   
    lookupregion = make.lookup.strat(db="lookupregion")  # strata vs region lookup table


  if (make.all) {
    for (t in 1:length(taxa)) {
      db = paste(xsm, period, region="all", taxa[t], id[t], sep="")
      print(db)
      params.sm = list(db=db, period=period, region="all", taxa=taxa[t], id=id[t], lookupregion=lookupregion)
      if (make.base) crab.make.setmeansbase(params.sm)
      if (make.catches) crab.merge.sm.catch(params.sm)
      if (make.condition) crab.merge.sm.resid (params.sm, lcrit=log10(1))
    }
  }


  # ------------------------------------------------------------------------------
  # extract and make region specific stats
  
  if (redodata) {
    for (r in 1:length(region)) {
    for (t in 1:length(taxa)) {
      db = paste(xsm, period, region[r], taxa[t], id[t], sep="")
      print(db)
      params.sm = list(db=db, period=period, region=region[r], taxa=taxa[t], id=id[t], lookupregion=lookupregion)
      outdir=paste(region[r], taxa[t], id[t], sep=".")
      subselect.sm(params.sm, dball=paste(xsm, period, region="all", taxa[t], id[t], sep="") )
    }}
  }


  # ------------------------------------------------------------------------------
  # generate time series data and plots
  
  if (make.timeseries) {
    out0 = out1 = NULL
    for (t in 1:length(taxa)) {
    for (r in 1:length(region)) {
      db = paste(xsm, period, region[r], taxa[t], id[t], sep="")
      print(db)
      params.sm = list(db=db, period=period, region=region[r], taxa=taxa[t], id=id[t], lookupregion=lookupregion)
      outdir=paste(region[r], taxa[t], id[t], sep=".")
      delta=1
      outvars=vars  
      if (make.spatialcorrelations)  outvars = c(outvars, lsvars) 
      strat = ts.data( outvars, times="annual", db=db, delta=delta )
      ts = ts.collapse(strat, outvars)
      # par(ask=T)
      ts.plot(ts, outvars, outdir)
      strat$region = region[r]
      strat$taxa = taxa[t]
      ts$region = region[r]
      ts$taxa = taxa[t]
      out0 = rbind(out0, strat)
      out1 = rbind(out1, ts)
    }}
    write.table(out0, file="strat.csv", quote=F, sep=";")
    write.table(out1, file="ts.csv", quote=F, sep=";")
  }

  
  if (make.maps) {
    for (r in 1:length(region)) {
    for (t in 1:length(taxa)) {
      db = paste(xsm, period, region[r], taxa[t], id[t], sep="")
      print(db)
      params.sm = list(db=db, period=period, region=region[r], taxa=taxa[t], id=id[t], lookupregion=lookupregion)
      outdir=paste(region[r], taxa[t], id[t], sep=".")
      params.base = gmt.define.region( locale=region[r], resolution=res, projection="-JM6.5i")
      params.gmt = list(gmtcol="seis", maskres="-S25k", interpres="-S25k")
      params.plot = list(haddockbox=F, sa=get.sa, convert=T, view=F, delta=1)
      gmt.isobaths( params.base, justthemap=0 )  # make a basemap -- needs to be run only once
      idata = map.data( use=gs, vars, times, db=db, outdir=outdir, params.base, params.gmt, params.plot)
      if (make.spatialcorrelations) {
        merge.sm.fractal (params.sm, lengthscale=seq(100, 1000, 50), variables=vars)  
        lsvars = paste(vars, c( "lsD",  "lsD2" ), sep="")
        idata = map.data( use=gs, lsvars, times, db=db, outdir=outdir, params.base, params.gmt, params.plot)
      }
    }}
  }


  if (make.movies.frompng) {
    for (r in 1:length(region)) {
    for (t in 1:length(taxa)) {
      outdir=paste(region[r], taxa[t], id[t], sep=".")
      make.movies(vars, times, outdir, delay=100)
    }}
  }


  if (pstopng) {
    for (r in 1:length(region)) {
    for (t in 1:length(taxa)) {
      outdir=paste(region[r], taxa[t], id[t], sep=".")
      ccf.convert(vars, times, outdir)
    }}
  }

  if (clean.ps) cmd('find . -iname "*.ps" | xargs rm -f')


