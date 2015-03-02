


  RLibrary( "maptools" , "rgdal" )

	loadfunctions( c("spacetime", "utility", "substrate", "bathymetry" ) ) 

	# --------------------------------------
  # create the main database
  # some require upto 1.2 GB RAM, ~ 5 min
  # no need to run again unless the substrate data file is updated ... 

  if (make.substrate.db) {
    substrate.db ( DS="substrate.initial.redo" ) # stored as a SpatialGridDataFrame
		substrate.db ( p=spatial.parameters( type="SSE" ), DS="lonlat.highres.redo" )
		for ( j in c( "SSE", "canada.east" ) ) {  # sse and snowcrab have same domains
			p = spatial.parameters( type=j )
			substrate.db ( p, DS="lonlat.interpolated.redo" )
			substrate.db ( p, DS="lonlat.redo" )
			substrate.db ( p, DS="planar.redo" )
		}
  }


  # --------------------------------------
  # substrate = substrate.db( p, DS="lonlat.highres" ) # or lonlat to refresh, planar or planar.saved 
  # library(lattice)
  # levelplot( log(grainsize) ~ lon + lat, substrate, main = "ln( grainsize; mm )", aspect="iso")
  
  # load the imported data in a data.frame format in a snow crab-consistent coordinates framework
  p = spatial.parameters( type="SSE" )
		
  substrate = substrate.db( p, DS="planar" ) # or lonlat to refresh, planar or planar.saved 
  i = which( substrate$plon< 990 &  substrate$plon > 220  &
             substrate$plat< 5270 &  substrate$plat > 4675 
  )
  substrate = substrate[ i, ]
  substrate$grainsize =log( substrate$grainsize )
  inside = filter.region.polygon( substrate[, c("plon", "plat") ], "cfaall", planar=T)
  datacols = c("plon", "plat", "grainsize")
  datarange = seq(-5,3, length.out=50)
  cols = color.code( "blue.black", datarange )
  outfn = "substrate.grainsize"
  annot = "ln ( Grain size; mm )"
  map( xyz=substrate[inside,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=file.path( project.directory("substrate"), "R"), at=datarange , col.regions=cols )

  

