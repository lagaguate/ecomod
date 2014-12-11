

spatial.parameters = function( p=NULL, type="SSE" ) {

  if (is.null(p)) p=list()

  if (type=="gmt") {
    p = c( p, params.gmt() )  # default core GMT setting here
    p$overlay = c( "cfanorth", "cfasouth", "cfa4x")
		p$gmt.projection.long = "Lambert.conformal.conic.crab"  # for gmt maps
		p$mapres = "15sec"  # "15sec" is currently the highest resolution
    p$tension = "-T0.4"  # 0.35+ for steep; 0.25 for smooth
    p$maskres = "-S25k"
    p$interpres = "-n40k"
    p$T.interp.method = "tps"  # tps is thin splate spline with GMT (alt: inverse distance using gstat)
    p$delete.postscript = T
    p$redo.basemap = F
    p = gmt.resolution(p)
    p = gmt.projection(p)
    p = gmt.defineregion(p)
  }


  if (type=="snowcrab") {
		p = spatial.parameters(p, type="SSE")
		p$spatial.domain = "snowcrab"
    p = gmt.region( p )  
    # this is the only difference between snowcrab and SSE --  map parameters for GMT !!
    # that is, all data streams and bounds/parameters are identical
    # However --- there is depth filtering and area 4X removed in bathymetry.db ...
  }


  if (type=="SSE") {
    p$spatial.domain = "SSE"
		p = gmt.region( p ) # this is the only difference between snowcrab and SSE -- map parameters for GMT

		# source raw data for bathymetry:
		p$bathymetry.xyz = file.path( project.directory("bathymetry"), "data", "bathymetry.canada.east.xyz" )
		p$bathymetry.bin = file.path( project.directory("bathymetry"), "data", "bathymetry.canada.east.bin" )
    
		# resolution and region
		p$internal.projection = "utm20"
    p$dres = 1/60/4  # this is the 15 second grid from CHS  .. default use highest resolution
    p$pres = 1
    p$lon0=-68
    p$lon1=-56
    p$lat0=41
    p$lat1=48
    p$lons = seq(p$lon0, p$lon1, by=p$dres)
    p$lats = seq(p$lat0, p$lat1, by=p$dres)
    p$nlons = length(p$lons)
    p$nlats = length(p$lats)
    p$corners = data.frame(lon=c(p$lon0,p$lon1), lat=c(p$lat0,p$lat1))
    p$corners = lonlat2planar( p$corners, proj.type=p$internal.projection )
    p$plons = seq(min(p$corners$plon), max(p$corners$plon), by=p$pres)
    p$plats = seq(min(p$corners$plat), max(p$corners$plat), by=p$pres)
    p$nplons = length(p$plons)
    p$nplats = length(p$plats)
  }


  if (type=="canada.east") {
    p$spatial.domain = "canada.east"
		p = gmt.region( p )
	
		# source raw data for bathymetry:
		p$bathymetry.xyz = file.path( project.directory("bathymetry"), "data", "bathymetry.canada.east.xyz" )
		p$bathymetry.bin = file.path( project.directory("bathymetry"), "data", "bathymetry.canada.east.bin" )
    
		# resolution and region
		p$internal.projection = "lambert.conic.canada.east"
    p$dres = 1/60/4  # this is the 15 second grid from CHS  .. default use highest resolution
    p$pres = 1
    p$lon0=-72
    p$lon1=-52
    p$lat0=40
    p$lat1=50
    p$lons = seq(p$lon0, p$lon1, by=p$dres)
    p$lats = seq(p$lat0, p$lat1, by=p$dres)
    p$nlons = length(p$lons)
    p$nlats = length(p$lats)
    p$corners = data.frame(lon=c(p$lon0,p$lon1), lat=c(p$lat0,p$lat1))
    p$corners = lonlat2planar( p$corners, proj.type=p$internal.projection )
    p$plons = seq(min(p$corners$plon), max(p$corners$plon), by=p$pres)
    p$plats = seq(min(p$corners$plat), max(p$corners$plat), by=p$pres)
    p$nplons = length(p$plons)
    p$nplats = length(p$plats)
  }


  return(p)
}


