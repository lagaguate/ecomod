  gmt.mapres = function(params) {

    params$res = "-I1m"

    if (params$mapres %in% c("10sec"))					 params$res = "-I10s" # 10 second grid
    if (params$mapres %in% c("15sec"))					 params$res = "-I15s" # 15 second grid
    if (params$mapres %in% c("20sec", "high"))   params$res = "-I20s" # 20 second grid
    if (params$mapres %in% c("30sec"))		       params$res = "-I30s" # 30 second grid
    if (params$mapres %in% c("1min", "med"))     params$res = "-I1m"  # 1 minute grid
    if (params$mapres %in% c("2min", "low"))     params$res = "-I2m"  # 2 minute grid
    if (params$mapres %in% c("4min", "low"))		 params$res = "-I4m"  # 4 minute grid
    if (params$mapres %in% c("5min"))		         params$res = "-I5m"  # 5 minute grid
    if (params$mapres %in% c("6min"))				     params$res = "-I6m"  # 6 minute grid
    if (params$mapres == "8min")					       params$res = "-I8m"  # 8 minute grid
    if (params$mapres == "10min")						     params$res = "-I10m"  # 10 minute grid
    if (params$mapres == "canada")						   params$res = "-I10m"
    
    return(params)
  }


