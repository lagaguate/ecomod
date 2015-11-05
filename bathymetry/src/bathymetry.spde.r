  # Bathymetry data 
  # processing bathymetry data with RINLA  .. no GMT dependency 

  # initialize bigmemory data objects
  p=list()
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry" ) )
  p$libs = RLibrary( "rgdal", "lattice", "parallel", "INLA", "geosphere", "sp", "raster", "colorspace" ,
    "bigmemory.sri", "synchronicity", "bigmemory", "biganalytics", "bigtabulate", "bigalgebra", "splancs")
  
  p$project.name = "bathymetry"
  p$project.root = project.datadirectory( p$project.name )
  
  p = spatial.parameters( type="canada.east.highres", p=p ) ## highres = 0.5 km discretization
  
  redo.bathymetry.rawdata = FALSE
  if ( redo.bathymetry.rawdata ) { 
    bathymetry.db ( p=spatial.parameters( type="canada.east", p=p ), DS="z.lonlat.rawdata.redo", additional.data=c("snowcrab", "groundfish") )
  }

  p = spacetime.parameters(p)  # load spde defaults

  p$dist.max = 75 # length scale (km) of local analysis .. for acceptance into the local analysis/model
  p$dist.mwin = 5 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
  p$dist.pred = 0.95 # % of dist.max where **predictions** are retained (to remove edge effects)
 
  ## this changes with resolution: at p$pres=0.25 and a p$dist.max=25: the max count expected is 40000
  p$n.min = 30
  p$n.max = 6000 # numerical time/memory constraint

  p$expected.range = 50 # km , with dependent var on log scale
  p$expected.sigma = 1e-1  # spatial standard deviation (partial sill) .. on log scale

  p$sbbox = spacetime.db( p=p, DS="statistics.box" ) # bounding box and resoltuoin of output statistics defaults to 1 km X 1 km

  p$debug.file = file.path( ecomod.workdirectory, "inla.debug.out" )


  p$modelformula = formula( ydata ~ -1 + intercept + f( spatial.field, model=SPDE ) ) # SPDE is the spatial covariance model .. defined in spacetime.interpolate.inla (below)

  p$spatial.field.name = "spatial.field"  # name used in formula to index the spatal random field

  p$spacetime.link = function( X ) { log(X + 1000) }  ## data range is from -383 to 5467 m .. 1000 shifts all to positive valued as this will operate on the logs
  p$spacetime.invlink = function( X ) { exp(X) - 1000 }

  p$spacetime.family = "gaussian"
  
  # if not in one go, then the value must be reconstructed from the correct elements:  
  p$spacetime.posterior.extract = function(s, rnm) { 
    # rnm are the rownames that will contain info about the indices ..
    # optimally the grep search should only be done once but doing so would 
    # make it difficult to implement in a simple structure/manner ... 
    # the overhead is minimal relative to the speed of modelling and posterior sampling
    i_intercept = grep("intercept", rnm, fixed=TRUE ) # matching the model index "intercept" above .. etc
    i_spatial.field = grep("spatial.field", rnm, fixed=TRUE )
    return(  s$latent[i_intercept,1] + s$latent[ i_spatial.field,1] )
  }
 

  reset.input = FALSE
  if (reset.input) {
    # faster if you do this step on kaos (the fileserver)
    bathymetry.db ( p, DS="bathymetry.spacetime.input.redo" )  # Warning: req ~ 15 min, 40 GB RAM (2015, Jae)
    spacetime.db( p=p, DS="bigmemory.inla.reset.input", B=bathymetry.db( p=p, DS="bathymetry.spacetime.input" ) )
  }

  
  reset.output = FALSE
  if (reset.output) {
    spacetime.db( p=p, DS="bigmemory.inla.reset.output" ) # create/reset bigmemory output data objects  
    cat( paste( Sys.time(), Sys.info()["nodename"], p$project.name, p$project.root, p$spatial.domain, "\n" ), file=p$debug.file, append=FALSE ) # init
  }

  # cluster definition
  # do not use all CPU's as INLA itself is partially run in parallel
  # RAM reqiurements are a function of data density and mesh density .. currently ~ 12 GB / run
  # p$clusters = "localhost"  # if serial run, send a single cluster host
  # p$clusters = c( "hyperion",  "nyx", "tartarus", "kaos", "tethys" ) 
  p$clusters = rep( "localhost", 8 )
  # p$clusters = c( rep( "hyperion", 6 ), rep( "nyx", 24 ), rep ("tartarus", 24), rep("kaos", 24 ), rep("tethys", 6 ) )
  nS = spacetime.db( p, DS="statistics.bigmemory.size" )
    
  p = make.list( list( jj=sample( 1:nS ) ), Y=p ) # random order helps use all cpus 
  
  # spacetime.interpolate.inla( p=p, debugrun=TRUE ) 
  parallel.run( spacetime.interpolate.inla, p=p ) # no more GMT dependency! :)  
 


  if (0) {
    # low level check of results
    p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
    
    # predictions
    S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir ) 
 
    # problematic and/or no data (i.e., land) and skipped
    i = which( is.nan( S[,3] ) )
    length(i)
     
    # not yet completed
    j = which( is.na( S[,3] ) ) 
    length(j)

    # completed 
    k = which( is.finite (S[,3])  ) # not yet done
    length(k)

  

  # world coastline data base
  require(maps)
  require(mapdata)
  require(maptools)
  require(rgdal)
  require(sp)
  library(latticeExtra)
     
  
  # generate landmask
    if (index.redo) {
      V = data.frame( cbind(plon=S[,1], plat=S[,2]) )
      V = SpatialPoints( planar2lonlat( V, proj.type=p$internal.crs )[, c("lon", "lat" )], CRS("+proj=longlat +datum=WGS84") ) 
      landmask( lonlat=V, db="worldHires",regions=c("Canada", "US"), ylim=c(36,53), xlim=c(-72,-45),
      tag="statistics" )
    }
    oc = landmask( db="worldHires", regions=c("Canada", "US"), return.value="not.land", tag="statistics" )


  x11()
    datarange = log( c( 5, 800 ))
    dr = seq( datarange[1], datarange[2], length.out=150)
    levelplot( log(S[oc,3])  ~ S[oc,1] + S[oc,2] , aspect="iso", at=dr, col.regions=color.code( "seis", dr) ,
      contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE), cex=2, 
      panel = function(x, y, subscripts, ...) {
        panel.levelplot (x, y, subscripts, aspect="iso", rez=c(5,5), ...)
        #coastline
        cl = landmask( return.value="coast.lonlat",  ylim=c(36,53), xlim=c(-72,-45) )
        cl = lonlat2planar( data.frame( cbind(lon=cl$x, lat=cl$y)), proj.type=p$internal.crs )
        panel.xyplot( cl$plon, cl$plat, col = "black", type="l", lwd=1 )
      }
    ) 


    #predictions 
    P = attach.big.matrix(p$descriptorfile.P , path=p$tmp.datadir )
    pps  =  expand.grid( plons=p$plons, plats=p$plats)
   
    # generate landmask
    if (index.redo) {
      V = SpatialPoints( planar2lonlat( pps, proj.type=p$internal.crs )[, c("lon", "lat" )], CRS("+proj=longlat +datum=WGS84") ) 
      landmask( lonlat=V, db="worldHires",regions=c("Canada", "US"), ylim=c(36,53), xlim=c(-72,-45),
      tag="predictions" )
    }
    oc = landmask( db="worldHires",regions=c("Canada", "US"), return.value="not.land", tag="predictions" )


  x11()
    datarange = log( c( 1, 5000 ))
    dr = seq( datarange[1], datarange[2], length.out=150)
    levelplot( log( P[oc,2] ) ~ plons + plats, pps[oc,], aspect="iso", main="mean", at=dr, col.regions=rev(color.code( "seis", dr)) ,
      contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE),
        panel = function(x, y, subscripts, ...) {
        panel.levelplot (x, y, subscripts, aspect="iso", rez=c(1,1), ...)
        #coastline
        cl = landmask( return.value="coast.lonlat",  ylim=c(36,53), xlim=c(-72,-45)  )
        cl = lonlat2planar( data.frame( cbind(lon=cl$x, lat=cl$y)), proj.type=p$internal.crs )
        panel.xyplot( cl$plon, cl$plat, col = "black", type="l", lwd=1 )
      }
    )
 
    x11()
     datarange = ( c( 0.1, 50 ))
     dr = seq( datarange[1], datarange[2], length.out=150)
     levelplot(  P[oc,3]  ~ plons + plats, pps[oc,], aspect="iso", main="mean", at=dr, col.regions=rev(color.code( "seis", dr)) , contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )

    # redo incomplete
    p = make.list( list( jj=sample( j ) ), Y=p ) 
    p = parallel.run( spacetime.interpolate.inla, p=p ) # no more GMT dependency! :)  

  }

  spacetime.plot( p=p, "predictions.mean.bigmemory" ) # directly from bigmatrix objects
  
  # save to file and clean up 
  spacetime.db( p=p, DS="predictions.redo" )  
  spacetime.db( p=p, DS="statistics.redo" )  # this also rescales results to the full domain
  spacetime.db( p=p, DS="bigmemory.inla.cleanup" )

  # compute slope and curvature and then assimilate results in bathymerty
  bathymetry.db( p=p, DS="bathymetry.spacetime.finalize.redo" )  

  # as the interpolation process is so expensive, regrid based off the above run
  bathymetry.db( p=p, DS="finalized", regrid=c( "SSE", "snowcrab", "mpa" ) ) # if you want more, will need to add to the list and modify the selection criteria




  # world bathymetry data base
  # request at: https://www.bodc.ac.uk/data/online_delivery/gebco/ [ jae.choi@dfo ] ... / gate.gate
  # etopo1_bedrock.xyz ---> 1 min resolution at http://maps.ngdc.noaa.gov/viewers/wcs-client/
 WSEN: -72,36,-45.,53


###required packages
library(RNetCDF)
library(maps)
library(mapdata)
library(marmap)


###Data
#data locations
bathy_fname <- "galapagos_gebco_08_-92_-2_-88_2.nc" # from https://www.bodc.ac.uk/data/online_delivery/gebco/gebco_08_grid/
coast_fname <- "galapagos_18563.dat" # from
 
#load bathy data
nc <- open.nc(bathy_fname)
print.nc(nc)
tmp <- read.nc(nc)
z <- array(tmp$z, dim=tmp$dim)


#z[which(z > 0)] <- NaN
z <- z[,rev(seq(ncol(z)))]
xran <- tmp$x_range
yran <- tmp$y_range
zran <- tmp$z_range
lon <- seq(tmp$x[1], tmp$x[2], tmp$spac[1])
lat <- seq(tmp$y[1], tmp$y[2], tmp$spac[1])
rm(tmp)
close.nc(nc)
 
#load coast data
coast <- read.table(coast_fname)
names(coast) <- c("lon", "lat")
 
###Plot
#make palette
ocean.pal <- colorRampPalette(
 c("#000000", "#000413", "#000728", "#002650", "#005E8C",
 "#0096C8", "#45BCBB", "#8AE2AE", "#BCF8B9", "#DBFBDC")
)
 
land.pal <- colorRampPalette(
 c("#467832", "#887438", "#B19D48", "#DBC758", "#FAE769",
 "#FAEB7E", "#FCED93", "#FCF1A7", "#FCF6C1", "#FDFAE0")
)
 
zbreaks <- seq(-8000, 8000, by=10)
cols <-c(ocean.pal(sum(zbreaks<=0)-1), land.pal(sum(zbreaks>0)))
 
#compare coastlines to package 'mapdata'
png("coastline_compare.png", width=7.5, height=6, units="in", res=400)
#quartz(width=7.5, height=6)
layout(matrix(1:2, 1,2), widths=c(6,1.5), heights=c(6))
 
par(mar=c(2,2,1,1), ps=10)
image(lon, lat, z=z, col=cols, breaks=zbreaks, useRaster=TRUE, ylim=c(-1.5,0.5), xlim=c(-92,-90))
lines(coast, col=1)
map("world", col=2, ylim=c(-2,2), xlim=c(-93,-88), add=TRUE)
map("worldHires", col=3, ylim=c(-2,2), xlim=c(-93,-88), add=TRUE)
legend("bottomleft", legend=c("World Vector Shoreline", "maps: world", "mapdata: worldHires"), lty=1, col=c(1:3), bg="white") 
 
par(mar=c(2,0,1,5))
image(x=1, y=zbreaks, z=matrix(zbreaks, 1, length(zbreaks)), col=cols, breaks=zbreaks, useRaster=TRUE, xlab="", ylab="", axes=FALSE)
axis(4, at=seq(-8000, 8000, 1000), las=2)
mtext("[meters]", side=4, line=3)
box()
 
dev.off()
