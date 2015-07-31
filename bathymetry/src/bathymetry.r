# -------------------------------------------------------------------------------------
# Bathymetry data --- warning this depends upon GMT (Generic Mapping Tools) 
  
  p=list()
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry" ) )
  p$libs = RLibrary( "chron", "rgdal", "lattice", "parallel" )

  
  # ------------------
  # glue all data sources (spherical coords) 
  # ... right now this is about 17 GB in size when expanded .... SLOW .... 
  # and it takes about 52+ GB RAM (due to addition of Greenlaw's DEM )
  # run on servers only unless your machine can handle it
  redo.bathymetry.rawdata = FALSE
  if ( redo.bathymetry.rawdata ) { 
		p = spatial.parameters( type="canada.east", p=p )
    p = gmt.parameters(p)  # interpolation parameters ... currently using GMT to interpolate bathymetry
    bathymetry.db ( p, DS="z.lonlat.rawdata.redo", additional.data=c("snowcrab", "groundfish") )
    if ( !file.exists( p$bathymetry.bin )) {
      # a GMT binary file of bathymetry .. currently, only the "canada.east" domain is all that is required/available
        cmd( "gmtconvert -bo", p$bathymetry.xyz, ">", p$bathymetry.bin )
    }
  }


  # ------------------
  # too many clusters will overload the system as data files are large ~(11GB RAM required to block) 
  # for the high resolution maps .. the temporary files can be created deleted/overwritten files 
  # in the temporary drives 
  redo.isobaths = FALSE
  if (redo.isobaths) {
    area = c( "snowcrab", "SSE", "ecnasap", "canada.east" ) 
    for (sp in area) {
      p$spatial.domain = sp
      p = spatial.parameters( p=p )
      p = gmt.parameters(p)  # interpolation parameters ... currently using GMT to interpolate bathymetry
      # override defaults in gmt.parameters as additional ones are used by other systems including lattice
      p$isobaths = c( 0, seq(50, 450, by=50), seq( 500, 1000, by=100 )  ) #override defaults 
      p = make.list( list( depths=p$isobaths ), Y=p )
      p$clusters = rep( "localhost", 1 )  
      #isobath.db( p=p, DS="redo" ) 
      parallel.run( isobath.db,  p=p, DS="redo" ) 	
    }
  }


  # ------------------
  # intermediary base maps with location definitions, annotations and isobaths ... to speed up PS map production .. only for GMT maps
  redo.basemap.gmt = FALSE  
  if ( redo.basemap.gmt ) {
    area = c( "snowcrab", "SSE", "ecnasap", "canada.east" ) 
    for (sp in area) {
      p$spatial.domain = sp
      p = spatial.parameters( p=p )
      p = gmt.parameters(p)  # interpolation parameters ... currently using GMT's isobaths whcih are specified in gmt.parameters
      # or if you want to override the isobaths plotted define them here (but make sure they were created in the previous step)
      # p$isobaths = c( seq(50, 450, by=100)  )
      gmt.basemap(p)
    }
  }

  
  # ------------------
  # prepare finalised bathymetry data for use in ecomod
  complete.bathymetry.db = FALSE
  areas = c( "canada.east", "SSE" ) # only two are currently used  
  for ( sp in areas ) {
    p = spatial.parameters( type=sp, p=p )
    p = gmt.parameters(p)
    bathymetry.db ( p, DS="prepare.intermediate.files.for.dZ.ddZ" )  # uses GMT's math functions ...
		bathymetry.db ( p, DS="Z.redo" )
		bathymetry.db ( p, DS="dZ.redo" )
		bathymetry.db ( p, DS="ddZ.redo" )
    bathymetry.db ( p, DS="baseline.redo" ) # additional filtering of areas and or depth to reduce file size
    bathymetry.db ( p, DS="complete.redo" ) # glue all together 
	
  }

 
  # ------------------
  # "snowcrab" subsets do exist but are simple subsets of SSE 
  # so only the lookuptable below is all that is important as far as bathymetry is concerned
  # both share the same initial domains + resolutions
 	p = spatial.parameters( type="snowcrab", p=p )
  bathymetry.db ( p, DS="baseline.redo" ) # additional filtering of areas and or depth to reduce file size
  bathymetry.db( DS="lookuptable.sse.snowcrab.redo" ) 
 



  # ------------------
  ## a few lattice-based maps: for SSE only right now
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
  
  outdir = file.path(project.datadirectory("bathymetry","maps"), p$spatial.domain) 

  dr = quantile( x$z, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "depth"
  annot = "ln ( Depth; m )"
  map( xyz=x[,c("plon", "plat", "z")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=outdir, at=datarange , col.regions=cols, spatial.domain=p$spatial.domain )
  
  
  x = bathymetry.db ( p, DS="dZ.planar" )
	if (snowcrab.area) x = x[sc,]
  dr = quantile( x$dZ, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "slope"
  annot = "ln ( Slope; m/m )"
  map( xyz=x[ ,c("plon", "plat", "dZ")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=outdir, at=datarange , col.regions=cols , spatial.domain=p$spatial.domain )

 
  x = bathymetry.db ( p, DS="ddZ.planar" )
	if (snowcrab.area) x = x[sc,]
  dr = quantile( x$ddZ, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "curvature"
  annot = "ln ( Curvature; m/m/m )"
  map( xyz=x[,c("plon", "plat", "ddZ")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=outdir, at=datarange , col.regions=cols, spatial.domain=p$spatial.domain )



