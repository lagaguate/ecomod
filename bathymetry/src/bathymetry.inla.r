
 
## ----- Adaptive estimation method (test) :
# processing bathymetry data with RINLA  

  p=list()
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry" ) )
  p$libs = RLibrary( "rgdal", "lattice", "parallel", "INLA", "geosphere", "sp", "raster" )
  
 
  p$project.name = "bathymetry"
  p$interpolation.function = interpolate.local.2d.depth  # found in spacetime/src/_Rfunctions/
  p$inla.alpha = 2 # 2 = exponential autocorrelation

  p = spatial.parameters( type="canada.east", p=p )
  p$dist.max = p$pres * 10 # length scale (km) of local analysis .. for acceptance into the local analysis
  p$dist.mwin = p$pres * 10 # size of the moving window (km) that aggregates the statisitics
  
  B = bathymetry.db ( p, DS="z.lonlat.rawdata" ) # larger
  B = lonlat2planar( B, proj.type=p$internal.projection )

  # levelplot( z~plon+plat, W, xlab='', ylab='', col.regions=colorRampPalette(c( "white", "darkblue", "black"), space = "Lab")(n), scale=list(draw=FALSE), aspect="iso", cuts=n )

  p$inputdata.filename = project.datadirectory( p$project.name, "tmp" ) 
  p$backingfile.W = "input.bigmatrix.tmp"
  p$descriptorfile.W = "input.bigmatrix.desc"
 
  # input data
  W = filebacked.big.matrix( nrow=nrow(B), ncol=ncol(B), type="double", init=B, dimnames=NULL, separated=FALSE, 
    backingpath=p$tmp.datadir, backingfile=p$backingfile.P, descriptorfile=p$descriptorfile.P ) 

  rm( B ); gc()

  p$outfilename.P = project.datadirectory( p$project.name, "data", "predictions.rdata" ) 
  p$outfilename.S = project.datadirectory( p$project.name, "data", "statistics.rdata" ) 

  interpolate.2d( p, method="inla.local", ... )


  p$backingfile.W = "input.bigmatrix.tmp"
  p$descriptorfile.W = "input.bigmatrix.desc"
  W = filebacked.big.matrix( nrow=nrow(B), ncol=ncol(B), type="double", init=B, dimnames=NULL, separated=FALSE, 
    backingpath=p$tmp.datadir, backingfile=p$backingfile.P, descriptorfile=p$descriptorfile.P ) 

  rm( B ); gc()

  p = interpolate.local.2d.depth( p ) 
  
  P = attach.big.matrix(p$descriptorfile.P)  # predictions
  S = attach.big.matrix(p$descriptorfile.S)  # statistical outputs

  S = load( p$


