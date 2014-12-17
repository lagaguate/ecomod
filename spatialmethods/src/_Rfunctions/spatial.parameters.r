

spatial.parameters = function( p=NULL, type="SSE" ) {

  if (is.null(p)) p=list()
  
  if ( ! exists("spatial.domain", p) ) p$spatial.domain = type
   
  if ( p$spatial.domain %in% c("SSE", "snowcrab") ) {
    # source raw data for bathymetry:
		p$bathymetry.xyz = file.path( project.directory("bathymetry"), "data", "bathymetry.canada.east.xyz" )  # ascii
		p$bathymetry.bin = file.path( project.directory("bathymetry"), "data", "bathymetry.canada.east.bin" )  # GMT binary
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
  }


  if ( p$spatial.domain=="canada.east") {
		# source raw data for bathymetry:
		p$bathymetry.xyz = file.path( project.directory("bathymetry"), "data", "bathymetry.canada.east.xyz" )  # ascii
		p$bathymetry.bin = file.path( project.directory("bathymetry"), "data", "bathymetry.canada.east.bin" )  # GMT binary
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


