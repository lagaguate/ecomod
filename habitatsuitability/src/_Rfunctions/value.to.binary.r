
	value.to.binary = function( pr=0.05, x, y=NULL ) {
		# convert abundance (x) to binary using quantile information, 
		# need to be clever if there is more than 1 survey (y)

		# ---	bc = bio.db( DS="cat.with.zeros", p=p)

		# ---	bc$bn = value.to.binary( pr=p$quantile.threshold, x=bc$totno, y=bc$data.source ) 
	


		bc$qn = NA
		bc$qm = NA
		for ( s in surveys ) {
			si = which( bc$data.source==s  )
			for (sp in sps ){
				print (sp)
				spi = intersect( si, which( bc$spec == sp ) )
				if (length( spi) > 0 ) {
					# convert to quantiles, by species and survey
					bc$qn[spi] = quantile.estimate( bc$totno[spi] )
					bc$qm[spi] = quantile.estimate( bc$totmass[spi] )  
				}
		}}
	
		bc$zm = NA 
		bc$zn = NA 
		ii = which( is.finite( bc$qn ) )
		jj = which( is.finite( bc$qm ) )
		bc$zn[ii] = qnorm( bc$qn[ii] )
		bc$zm[jj] = qnorm( bc$qm[jj] )
	

	}

