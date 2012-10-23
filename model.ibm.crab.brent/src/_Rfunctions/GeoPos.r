setConstructorS3("GeoPos", function(lon=0, lat=0) {
	extend(Object(), "GeoPos",
		.lon = lon,
		.lat = lat
	)
})
setMethodS3("getpos", "GeoPos", function(this) {
	c(this$.lon, this$.lat);
})
setMethodS3("setpos", "GeoPos", function(this, lon, lat) {
	this$.lon = lon;
	this$.lat = lat;
})