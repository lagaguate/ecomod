
  gmt.determine.basemap = function(params) {
    
    # make the basemap with bathimetry; "basemap" is defined in the P list
    basemap.location = project.directory("bathymetry", "maps" )
    params$basemap = file.path(basemap.location, "basemap.1min.ps" )

    if (params$mapres %in% c("10sec"))  params$basemap = file.path(basemap.location, "basemap.10sec.ps" )
    if (params$mapres %in% c("15sec"))  params$basemap = file.path(basemap.location,"basemap.15sec.ps")
    if (params$mapres %in% c("20sec", "high"))  params$basemap = file.path(basemap.location,"basemap.20sec.ps")
    if (params$mapres %in% c("30sec"))     params$basemap = file.path(basemap.location,"basemap.30sec.ps")
    if (params$mapres %in% c("1min", "med")) params$basemap = file.path(basemap.location,"basemap.1min.ps")
    if (params$mapres %in% c("2min", "low")) params$basemap = file.path(basemap.location,"basemap.2min.ps")
    if (params$mapres %in% c("4min", "low")) params$basemap = file.path(basemap.location,"basemap.4min.ps")
    if (params$mapres %in% c("5min"))     params$basemap = file.path(basemap.location,"basemap.5min.ps")
    if (params$mapres %in% c("6min"))     params$basemap = file.path(basemap.location,"basemap.6min.ps")
    if (params$mapres == "8min")       params$basemap = file.path(basemap.location,"basemap.8min.ps" )
    if (params$mapres == "10min")      params$basemap = file.path(basemap.location,"basemap.10min.ps")
    if (params$mapres == "canada")     params$basemap = file.path(basemap.location,"basemap.10min.ps")
    return(params)
  }


