# -------------------------------------------------------------------------------------
# Bathymetry data
  
  
  p=list()
  p$init.files = loadfunctions( c( "common", "bathymetry" ) )
  p$libs = loadlibraries( c("chron", "rgdal", "lattice", "parallel" ) )
 
  p$isobaths = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 250, 300, 350, 400, 450, 500, 600, 800, 1000 )
	

	if ( bathymetry.rawdata.redo ) { 
		# glue all data sources (spherical coords) 
    # ... right now this is about 17 GB in size when expanded .... SLOW .... 
    # and it takes about 52+ GB RAM (due to addition of Greenlaw's DEM )
    # run on servers only unless your machine can handle it
		p = spatial.parameters( type="canada.east", p=p )
		bathymetry.db ( p, DS="z.lonlat.rawdata.redo", additional.data=c("snowcrab", "groundfish") )
	}

	# begin interpolations using GMT 
 
  for ( j in c( "canada.east", "SSE" ) ) {
		p = spatial.parameters( type=j, p=p )
		bathymetry.db ( p, DS="prepare.intermediate.files.for.dZ.ddZ" )  # uses GMT...
		bathymetry.db ( p, DS="Z.redo" )
		bathymetry.db ( p, DS="dZ.redo" )
		bathymetry.db ( p, DS="ddZ.redo" )
    bathymetry.db ( p, DS="baseline.redo" ) # additional filtering of areas and or depth to reduce file size
    bathymetry.db ( p, DS="complete.redo" ) # glue all together 
		p = make.list( list( depths=p$isobaths ), Y=p )
    p$clusters = rep( "localhost", 2 )  # too many clusters will overload the system ... data files are large ~(11GB RAM required to block) and can be deleted in the temporary drives 
    parallel.run( isobath.db,  p=p, DS="redo" ) 	
		# isobath.db( p=p, depths=depths, DS="redo" ) 
	}

 
  # "snowcrab" subsets do exist but are simple subsets of SSE 
  # so only the lookuptable below is all that is important as far as bathymetry is concerned
  # both share the same initial domains + resolutions
 	p = spatial.parameters( type="snowcrab", p=p )
  bathymetry.db ( p, DS="baseline.redo" ) # additional filtering of areas and or depth to reduce file size
  bathymetry.db( DS="lookuptable.sse.snowcrab.redo" ) 
 



## a few maps:
 
  p = spatial.parameters( type="SSE" )
  x = bathymetry.db ( p, DS="baseline" )
  
	snowcrab.area=F
	if (snowcrab.area) {
		# this is used below
		sc = intersect( 
				which( x$plon< 990 & x$plon > 220  & x$plat< 5270 & x$plat > 4675 ) ,
				filter.region.polygon( x[, c("plon", "plat") ], "cfaall", planar=T) 
		)
		x = x[sc,]
	}
	
	x$z =log( x$z )
  
  dr = quantile( x$z, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "depth"
  annot = "ln ( Depth; m )"
  map( xyz=x[,c("plon", "plat", "z")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=project.directory("bathymetry", "maps"), at=datarange , col.regions=cols )
  

  
  x = bathymetry.db ( p, DS="dZ.planar" )
	if (snowcrab.area) x = x[sc,]
  dr = quantile( x$dZ, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "slope"
  annot = "ln ( Slope; m/m )"
  map( xyz=x[ ,c("plon", "plat", "dZ")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=project.directory("bathymetry","maps"), at=datarange , col.regions=cols )

  
 
  x = bathymetry.db ( p, DS="ddZ.planar" )
	if (snowcrab.area) x = x[sc,]
  dr = quantile( x$ddZ, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "curvature"
  annot = "ln ( Curvature; m/m/m )"
  map( xyz=x[,c("plon", "plat", "ddZ")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=project.directory("bathymetry", "maps"), at=datarange , col.regions=cols )

  

