
  gmt.annot = function( p ) {
		if (p$spatial.domain %in% c("SSE", "vwx", "all", "4vwx", "vw", "crab", "snowcrab")) p$annot = "-B2neSW"
		if (p$spatial.domain %in% c("v", "w", "x")) p$annot = "-B2ne-5SW"
		if (p$spatial.domain %in% c( "canada.east", "ecnasap", "ecnasap2", "porbeagle") ) p$annot = "-B10neSW"
		if (p$spatial.domain == "canada") p$annot = "-B20neSW"
		return  ( p )
  }


