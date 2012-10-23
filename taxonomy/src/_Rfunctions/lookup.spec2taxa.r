
  lookup.spec2taxa = function( spec, tx = taxa.db() ) { 
    out = data.frame( spec=spec)
    out$tx = NA
		out$vern = NA
		out$tsn = NA

    for ( i in 1:length(spec) ) {
      itx = which( tx$spec == spec[i] )
      itx_good = NULL

      if (length(itx) == 0 ) {
        next()
      } else {
        itx_good = itx[1]
      }

      out$tx[i] = tx$name.scientific[itx_good]
      out$vern[i] = tx$name.common[itx_good]
      out$tsn[i] = tx$itis.tsn[itx_good]
    }
    return (out) 
  }


