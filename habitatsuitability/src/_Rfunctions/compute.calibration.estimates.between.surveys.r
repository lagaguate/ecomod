	
	compute.calibration.estimates.between.surveys = function() { 
		
		#### TODO  --- compare zqn, zqm by survey ...


		fn = file.path( project.datadirectory("bio"), "data", "species.specific.calibration.estimates.rdata" )
    
		sq = NULL # trip/set loc information
	
    if ( is.null(DS) ) {
      if (file.exists( fn) ) load( fn)
      return ( sq )
    }

		OO = habitatsuitability.db( DS="subset", p=p) 

		sset = OO$set
		sset = lonlat2planar( sset, proj.type=p$internal.projection )

		scat = OO$cat
    
		rm (OO); gc()


		

		# test plot

		bc = scat
	  tx = 10
		taxonomy.recode ( from="spec", tolookup=tx )
		ww = bc[ bc$spec==tx ,]
		require(lattice)
		histogram( qn|data.source, bc )
		by( bc$qn, bc$data.source, summary)
		by( bc$totno, bc$data.source, summary)










		sps = sort( unique( scat$spec ) )
		sq = expand.grid( data.source=sort( unique(scat$data.source)) , spec=sps )
		

		}



