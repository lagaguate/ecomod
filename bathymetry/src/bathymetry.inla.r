
 
## ----- Adaptive estimation method (test) :
# processing bathymetry data with RINLA  

  p=list()
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry" ) )
  p$libs = RLibrary( "rgdal", "lattice", "parallel", "INLA", "geosphere", "sp", "raster",  "bigmemory" )
  
 
  p$project.name = "bathymetry"
  p$project.root = project.datadirectory( p$project.name )

  p$inla.alpha = 2 # 2 = exponential autocorrelation
  p$inla.nsamples = 1000 # no. samples to ude for posterior predictions

  p = spatial.parameters( type="canada.east.highres", p=p ) ## highres = 1/4 km discretization
  
  p$n.min = 50
  p$dist.max = 25 # length scale (km) of local analysis .. for acceptance into the local analysis/model
  p$dist.mwin = 1 # size of the moving window (km) that aggregates the ** statistics **
  p$dist.pred = 10 # size of the moving window (km) where **predictions** are retained
  
  B = bathymetry.db ( p, DS="z.lonlat.rawdata" ) # larger
  B = lonlat2planar( B, proj.type=p$internal.projection )

  # or to debug:
  # B = bathymetry.db ( p, DS="baseline" ) # smaller
 
  # levelplot( z~plon+plat, W, xlab='', ylab='', col.regions=colorRampPalette(c( "white", "darkblue", "black"), space = "Lab")(n), scale=list(draw=FALSE), aspect="iso", cuts=n )

  p$tmp.datadir = file.path( p$project.root, "tmp" )
  if( !file.exists(p$tmp.datadir)) dir.create( p$tmp.datadir, recursive=TRUE, showWarnings=FALSE )

  # input data stored as a bigmatrix to permit operations with min memory usage
  p$backingfile.W = "input.bigmatrix.tmp"
  p$descriptorfile.W = "input.bigmatrix.desc"
  W = filebacked.big.matrix( nrow=nrow(B), ncol=ncol(B), type="double", dimnames=NULL, separated=FALSE, 
    backingpath=p$tmp.datadir, backingfile=p$backingfile.W, descriptorfile=p$descriptorfile.W ) 
  W[] = as.matrix(B)

  rm( B ); gc()

  p$outfilename.P =  file.path( p$project.root, "data", "predictions.rdata" ) 
  p$outfilename.S =  file.path( p$project.root, "data", "statistics.rdata" ) 

  p$clusters = c(rep( "nyx", 12 ), rep ("tartarus", 12), rep("kaos", 12 ))  # do not use all CPU's as INLA itself is partially run in parllel

  p = interpolate.local.2d.depth( p )  # found in spacetime/src/_Rfunctions/
  
  P = load( p$outfilename.P )
  S = load( p$outfilename.S )



