	
	compute.calibration.estimates.between.surveys = function() { 
		
		#### TODO

		fn = file.path( project.directory("bio"), "data", "species.specific.calibration.estimates.rdata" )
    
		sq = NULL # trip/set loc information
	
    if ( is.null(DS) ) {
      if (file.exists( fn) ) load( fn)
      return ( sq )
    }

		OO = bio.db( DS="subset", p=p) 

		sset = OO$set
		sset = lonlat2planar( sset, proj.type=p$internal.projection )

		scat = OO$cat
		scat$qn = NULL
		scat$qm = NULL

		rm (OO); gc()


		

		# test plot

		bc = scat
	  tx = 10
		lookup.spec2taxa (tx)
		ww = bc[ bc$spec==tx ,]
		require(lattice)
		histogram( qn|data.source, bc )
		by( bc$qn, bc$data.source, summary)
		by( bc$totno, bc$data.source, summary)










		sps = sort( unique( scat$spec ) )
		sq = expand.grid( data.source=sort( unique(scat$data.source)) , spec=sps )
		

		}



