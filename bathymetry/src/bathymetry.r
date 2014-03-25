# -------------------------------------------------------------------------------------
# Bathymetry data
  
  
  p=list()
  p$init.files = loadfunctions( c( "common", "bathymetry" ) )
  p$libs = loadlibraries( c("chron", "rgdal", "lattice" ) )
  p$depthrange = c(-3000, 100) # permissible depth ranges for all following analyses ... contrainted to limit file sizes .. keep above sea level for coastal areas 

	if ( bathymetry.rawdata.redo ) { 
		# glue all data sources (spherical coords) 
    # ... right now this is about 17 GB in size when expanded .... SLOW .... 
    # and it takes about 50+ GB RAM (due to addition of Greenlaw's DEM )
    # run on servers only unless your machine can handle it
		p = spatial.parameters( type="canada.east" )
		bathymetry.db ( p, DS="z.lonlat.rawdata.redo", additional.data=c("snowcrab", "groundfish") )
	}

	# begin interpolations using GMT  
	for ( j in c( "canada.east", "SSE" ) ) {
		p = spatial.parameters( type=j )
		bathymetry.db ( p, DS="prepare.intermediate.files.for.dZ.ddZ" )  # uses GMT...
		bathymetry.db ( p, DS="Z.redo" )
		bathymetry.db ( p, DS="dZ.redo" )
		bathymetry.db ( p, DS="ddZ.redo" )
  }


	for ( j in c( "canada.east", "SSE", "snowcrab" ) ) {
		p = spatial.parameters( type=j )
    bathymetry.db ( p, DS="baseline.redo" ) # additional filtering of areas and or depth to reduce file size
    bathymetry.db ( p, DS="complete.redo" ) # glue all together 

		depths = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 600, 800 )
		p$clusters = rep( "kaos", length(depths) )
		p = make.list( list( depths = 1:length(depths) ), Y=p )
    parallel.run( isobath.db,  p=p, depths=depths, DS="redo" ) 	
		# isobath.db( p=p, depths=depths, DS="redo" ) 
	}

  # create a lookuptable for SSE -> snowvrab domains
  # both share the same initial domains + resolutions
  bathymetry.db( DS="lookuptable.sse.snowcrab.redo" ) 
 



## a few maps:
  
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
  
  datarange = seq(-1, 8, length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "depth"
  annot = "ln ( Depth; m )"
  map( xyz=x[,c("plon", "plat", "z")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=project.directory("bathymetry"), at=datarange , col.regions=cols )
  

  
  x = bathymetry.db ( p, DS="dZ.planar" )
	if (snowcrab.area) x = x[sc,]
  datarange = seq(-12, -1, length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "slope"
  annot = "ln ( Slope; m/m )"
  map( xyz=x[ ,c("plon", "plat", "dZ")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=project.directory("bathymetry"), at=datarange , col.regions=cols )

  
 
  x = bathymetry.db ( p, DS="ddZ.planar" )
	if (snowcrab.area) x = x[sc,]
  datarange = seq(6, 14, length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "curvature"
  annot = "ln ( Curvature; m/m/m )"
  map( xyz=x[,c("plon", "plat", "ddZ")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=project.directory("bathymetry"), at=datarange , col.regions=cols )

  

