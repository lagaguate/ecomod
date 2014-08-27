
  gmt.resolution = function(params) {
		params = gmt.mapres( params )
		params = gmt.determine.basemap( params )
    return(params)
  }


