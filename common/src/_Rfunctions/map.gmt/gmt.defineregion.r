  gmt.defineregion = function(params) {
    
		# params$inp =  params$bathymetry.xyz
    params = gmt.region( params )
		params = gmt.annot ( params )
			
    return (params)
  }
