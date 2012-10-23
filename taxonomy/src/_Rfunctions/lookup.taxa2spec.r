
  
  lookup.taxa2spec = function(tx, spi=taxa.db() ) {
		
		v = as.vector(unlist( strsplit(tx, "[[:space:]]+", fixed=FALSE, perl=FALSE, useBytes=FALSE) ))
		
		out = list()

		for (i in 1:length(v)) {
			w = paste("\\<", v[i], "\\>", sep="")

			sn = grep( w, spi$name.scientific, ignore.case=T )
			vn = grep( w, spi$name.common, ignore.case=T )
			vn2 = grep( w, spi$name.common.worktable, ignore.case=T )
			vn3 = grep( w, spi$vernacular, ignore.case=T )

			out[[i]] = sort( unique( c(sn, vn, vn2, vn3)  ))
		}
		
		if (length(out)==0) {
			sp = -1  # no match
		} else  {
			sp = spi$spec[out[[1]]]
		}
		
		sp2 = sp
		if (length(out) > 1) {
			for ( i in 2:length(v)) {
				sp2 = intersect( sp2, out[[i]] )
			}
		}

		if (length(sp2)==1) { 
			sp=sp2 
		} else { 
			sp = sort( unique( c( sp, sp2 ) ) )
		}

		return(sp)
	}



