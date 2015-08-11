

spatial.parameters = function( p=NULL, type="SSE" ) {

  if (is.null(p)) p=list()
  
  if ( ! exists("spatial.domain", p) ) p$spatial.domain = type
   
  if ( p$spatial.domain %in% c("SSE", "snowcrab") ) {
    # source raw data for bathymetry:
		p$bathymetry.xyz = file.path( project.datadirectory("bathymetry"), "data", "bathymetry.canada.east.xyz" )  # ascii
		p$bathymetry.bin = file.path( project.datadirectory("bathymetry"), "data", "bathymetry.canada.east.bin" )  # GMT binary
		# resolution and region
		p$internal.projection = "utm20"
    p$internal.crs =  "+proj=utm +ellps=WGS84 +zone=20"
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
		p$bathymetry.xyz = file.path( project.datadirectory("bathymetry"), "data", "bathymetry.canada.east.xyz" )  # ascii
		p$bathymetry.bin = file.path( project.datadirectory("bathymetry"), "data", "bathymetry.canada.east.bin" )  # GMT binary
		# resolution and region
		p$internal.projection = "lambert.conic.canada.east"
    p$internal.crs = "+proj=lcc +ellps=WGS84  +lon_0=62W +lat_0=45N +lat_1=43N +lat_2=47N "
    p$dres = 1/60/4  # this is the 15 second grid from CHS  .. ~ 0.25 km
    p$pres = 1  # km
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

  if ( p$spatial.domain=="canada.east.highres") {
		# source raw data for bathymetry:
		p$bathymetry.xyz = file.path( project.datadirectory("bathymetry"), "data", "bathymetry.canada.east.xyz" )  # ascii
		p$bathymetry.bin = file.path( project.datadirectory("bathymetry"), "data", "bathymetry.canada.east.bin" )  # GMT binary
		# resolution and region
		p$internal.projection = "lambert.conic.canada.east"
    p$internal.crs = "+proj=lcc +ellps=WGS84  +lon_0=62W +lat_0=45N +lat_1=43N +lat_2=47N "
    p$dres = 1/60/4/2  # CHS is 15 arc second ~ 0.5km/2 
    p$pres = 0.5/2  # discretize to 0.5 km/2 resolution
    p$lon0=-72
    p$lon1=-52
    p$lat0=40
    p$lat1=50
    p$lons = seq(p$lon0, p$lon1, by=p$dres)
    p$lats = seq(p$lat0, p$lat1, by=p$dres)
    p$nlons = length(p$lons)
    p$nlats = length(p$lats)
    p$corners = data.frame(lon=c(p$lon0,p$lon1), lat=c(p$lat0,p$lat1))
    p$corners = lonlat2planar( p$corners, proj.type=p$internal.projection, ndigits=6 )  # ndigits=2 is default .. as pres is being divided by 2 add another digit
    p$plons = seq(min(p$corners$plon), max(p$corners$plon), by=p$pres)
    p$plats = seq(min(p$corners$plat), max(p$corners$plat), by=p$pres)
    p$nplons = length(p$plons)
    p$nplats = length(p$plats)
  }

  return(p)
}


