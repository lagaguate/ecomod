
	gmt.region = function (p) { 
		if (p$spatial.domain %in% c( "crab", "snowcrab") ) p$region = "-R-66.4/-57.2/42.2/47.4"
		if (p$spatial.domain == "porbeagle") p$region = "-R-68/-45/40/55"
		if (p$spatial.domain == "vw") p$region = "-R-65.5/-56.5/42.5/47.5"
		if (p$spatial.domain %in% c("SSE", "vwx", "all", "4vwx")) p$region = "-R-68/-56.5/41.5/47.5"
		if (p$spatial.domain %in% c("v", "w", "x")) p$region = "-R-68/-56.5/41.5/47.5"
		if (p$spatial.domain == "canada") p$region = "-R-140/-50/40/85"
		if (p$spatial.domain == "ecnasap") p$region = "-R-72/-40/36.5/67.5"
		if (p$spatial.domain == "ecnasap2") p$region = "-R-73/-43/39/61"
		if (p$spatial.domain == "canada.east") p$region = "-R-72/-52/40/50"
		return(p)
	}

