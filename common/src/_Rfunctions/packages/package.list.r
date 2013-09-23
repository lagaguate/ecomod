
	package.list = function( X="basic", default.repository="http://mirror.its.dal.ca/cran" ) {
	
    out = "Choose: basic, parallel, spatial, etc." 
    o0 = options("stringsAsFactors")
    options(stringsAsFactors = FALSE)  # in data.frames, make sure all variables are explicitly character 

		if (X=="basic") {
			pkgs = c( "abind", "acepack", "ade4",  "aplpack",
					"bigmemory", "boot", "car", "chron", "Cairo", "class" , "cluster", "setwidth",
					"date", "Design", "doBy", "eco", "effects", "filehash", "ff", "foreign", 
					"gee", "geepack", "gamm4", "graph",  "Hmisc", "igraph", "its", "jit", "lattice", "lme4", 
					"MASS", "maxLik", "mgcv", "mixdist", "mvtnorm", "nlme", "multcomp", "nlme", "nnet", "oz",
					"qcc", "quadprog", "randomForest","rCurl", "XML",
					"ppso", "Rcmdr", "Rcpp", "rgl", "Rcompression", "RColorBrewer",  "plotrix",
					"scatterplot3d",  "systemfit", "sm", "sampling",
					"SparseM", "splines", "stats", "stats4", "SuppDists", "TeachingDemos", "tools",
					"tcltk",  "tseries", "utils",  "vegan", "waveslim",  
					"xtable","XML", "zoo", "R.oo", "geosphere", "VGAM", "vimcom", "setwidth"  )	
			out = data.frame( cbind( pkgs=pkgs, repos=default.repository ) )
		}
		

		if (X=="parallel") {
			pkgs = c( "Rmpi", "rsprng", "snow", "snowfall", "doMC", "foreach", "multicore" )
			out = data.frame( cbind( pkgs=pkgs, repos=default.repository ) )
		}

		if (X=="spatial") {
			pkgs = c( "akima", "deldir", "fields", "gstat", "geoR", "geoRglm", "mapproj", "maps", 
          "maptools", "RArcInfo", "rgdal", "rgeos",
					"sgeostat", "sp", "spatial", "spatstat", "tripack", "PBSmapping" )	
			out = data.frame( cbind( pkgs=pkgs, repos=default.repository ) )
		}

		if (X=="epidemiology") {
			pkgs = c( "Epi", "epicalc", "epitools", "survrec", "survival" )
			out = data.frame( cbind( pkgs=pkgs, repos=default.repository ) )
		}


		if (X=="flr" ) {
			pkgs = c("FLCore", "FLAssess", "FLEDA", "FLICA", "FLICES", "FLPellaT", "SQLiteFL", 
					"FLXSA", "FLBRP",  "FLBocadillo", "FLBioDym", "FLash", "FSA" )
			out = data.frame( cbind( pkgs=pkgs, repos="http://R-Forge.R-project.org" ) )
		}

		if (X=="stock.assessment") {
			pkgs = c("fishmethods")
			out = data.frame( cbind( pkgs=pkgs, repos=default.repository ) )
		}

		if (X=="bayesian") {
			pkgs = c( "arm", "bayesm", "bayesSurv",
        "DPpackage",  "mcmc", "deal", "eco", 
        "lmm", "spBayes", "vabayelMix", "boa", "coda",
        "mcgibbsit", "rv", "bayesmix", "rbugs", "runjags", "rjags", "rv",
        "Umacs", "R2jags", "runjags", "gR", "MCMCpack" )
			Rforge.pkgs = c("BUGSExamples")
      out = data.frame( cbind( pkgs=pkgs, repos=default.repository ) )
		  out2 =  data.frame( cbind( pkgs=Rforge.pkgs, repos="http://R-Forge.R-project.org" ) )
      out= rbind(out, out2 )
    }

    if (X=="species") {
      pkgs= c("mda", "BIOMOD" )
      repos = c( default.repository, "http://r-forge.r-project.org/" )
      out = data.frame( cbind( pkgs=pkgs, repos=repos ) )
    }

		if (X=="simulation") {
			pkgs = c( "deSolve", "simecol",  "odesolve", "rootSolve", "ecolMod", "shapes", "diagram", "GillespieSAA", "sde", "pomp" )
			out = data.frame( cbind( pkgs=pkgs, repos=default.repository ) )
		}

		if (X=="network") {
			pkgs = c("igraph", "statnet", "sna" , "latentnet", "bipartite", 
							 "bionetdata" , "BoolNet", "LogitNet", "NetworkAnalysis",
							 "ggraph", "QuACN", "intergraph", "snort" )
			out = data.frame( cbind( pkgs=pkgs, repos=default.repository ) )
		}

		if (X=="database") {
			pkgs = c(  "RODBC", "DBI", "RSQLite" )
			out = data.frame( cbind( pkgs=pkgs, repos=default.repository ) )
		}


		if (X=="all") {
			out = NULL
			subsets = c("basic", "species", "database", "simulation", "bayesian", "stock.assessment", 
					"flr", "epidemiology", "spatial", "parallel", "omegahat", "eclipse" )
			for ( i in c(subsets) ) out = rbind( out, package.list( i ) )
		}

    options(stringsAsFactors = o0 )  # reset to initial state

		return (out)

	}


