
  gmt.parameters = function(p=NULL) {
    print( "Parameterisations for GMT ver 5.X.X " )
    if( is.null(p)) p=list()

    # default gmt-related parameters for Maritimes region of Canada
    p$gmtcol       = "seis"  # "red2green" is an alternate
    p$maskres      = "-S25k"
    p$interpres    = "-n40k"
    p$tension      = "-T0.4"  # 0.35+ for steep; 0.25 for smooth .. tension interpolation parameter
    
    p$bathy.tension = "-T0.75"  # tension parameter for GMT splines-in-tension (1=harmonic surface ... max,min not exceeded)
    p$bathy.maskres  = "-S40k"        # resolution
    p$bathy.zrange = "-Sa-1/NaN -Sb-501/NaN" # show 0 to 800 m depth range
    p$bathy.contour= "-C100 -S4 -W0.25p" # contour lines every 100m with lines of various thinknesses

    p$block = T  # do a block ?
    p$blocktype = "mean"  # do a block mean ?

    p$polygon.options = "-W0.8p"
    p$coast.options = "-Df -G220 -W0.5p"  # -Df = full resolution and colour/thickness
    p$scale.location = "-D4.75i/0.75i/2.6i/0.16ih" # alternate: "-D4.5i/0.8i/2.5i/.25ih"
    p$fnt = "18p"
    p$psresolution = "300"
    p$delta  = 1
    p$overlay = ""
    
    p$annot.lon0 = -63.7
    p$annot.lat0 = 47.25
    p$annot.fontsize = 32 # points
    p$annot.fontno = 0 # try  pstext  -L for possibles .. 0 = helvetica
    p$annot.fontcol = 0 # try  pstext  -L for possibles .. 0 = helvetica
    p$annot.angle = 0
    p$annot.justify = "LT"  # justification
    p$annot.text = ""

    
    p$delete.postscript = T
    p$redo.basemap = F

    basemap.location = project.directory("bathymetry", "maps" )
    p$basemap = file.path(basemap.location, "basemap.2min.ps" )  # default

 
    if ( exists( "spatial.domain", p) ) {
      
      if ( p$spatial.domain == "snowcrab" ) {
        # 2min grids
        p$res = "-I2m"  
        p$res.isobaths = "-I30s"  # slightly higher resolution for bathymetry to make it look nice
        p$basemap = file.path(basemap.location, "basemap.2min.ps")
        p$overlay = c( "cfanorth", "cfasouth", "cfa4x") # polygons of fishing areas 
        p$gmt.projection.long = "Lambert.conformal.conic.crab"
      }
      
      if ( p$spatial.domain %in% c("SSE", "4vw", "4vwx" ) {
        # 2min grids
        p$res = "-I2m" 
        p$res.isobaths = "-I30s"
        p$basemap = file.path(basemap.location, "basemap.2min.ps")
        p$gmt.projection.long = "Lambert.conformal.conic" 
      }
      
      if ( p$spatial.domain == "4vwx" ) {
        # 2min grids
        p$res = "-I2m"  
        p$res.isobaths = "-I30s"
        p$basemap = file.path(basemap.location, "basemap.2min.ps")
        
        p$gmt.projection.long = "Lambert.conformal.conic" 
      }
 
      if ( p$spatial.domain == "canada.east" ) {
        # 15 arc sec ... high res for 
        p$res = "-I10m"
        p$res.bathy = "-I15s"  # for bathymetry ... high res preferred
        p$res.isobaths = "-I15s"  # isobaths to match bathy
        p$basemap = file.path(basemap.location, "basemap.10min.ps")
        p$gmt.projection.long = "lambert.conic.canada.east" 
      }

    } else {
      stop( " 'spatial.domain' not found ... this needs to be defined for GMT to work ")
    }
    
    p = gmt.projection(p)  # req. "gmt.projection.long" -> "gmtproj" (-J params)
    p = gmt.region( p ) # req. spatial.domain -> region (-R params)
		p = gmt.annot ( p ) # req. spatial.domain -> annot (-B params)
	
    return(p)
  }
 
