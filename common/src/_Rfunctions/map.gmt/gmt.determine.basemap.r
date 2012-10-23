
  gmt.determine.basemap = function(params) {

    # default
      params$basemap = "basemap.1min.ps"

    if (params$mapres %in% c("10sec"))  params$basemap = "basemap.10sec.ps"
    if (params$mapres %in% c("15sec"))  params$basemap = "basemap.15sec.ps"
    if (params$mapres %in% c("20sec", "high"))  params$basemap = "basemap.20sec.ps"
    if (params$mapres %in% c("30sec"))     params$basemap = "basemap.30sec.ps"
    if (params$mapres %in% c("1min", "med")) params$basemap = "basemap.1min.ps"
    if (params$mapres %in% c("2min", "low")) params$basemap = "basemap.2min.ps"
    if (params$mapres %in% c("4min", "low")) params$basemap = "basemap.4min.ps"
    if (params$mapres %in% c("5min"))     params$basemap = "basemap.5min.ps"
    if (params$mapres %in% c("6min"))     params$basemap = "basemap.6min.ps"
    if (params$mapres == "8min")       params$basemap = "basemap.8min.ps"
    if (params$mapres == "10min")      params$basemap = "basemap.10min.ps"
    if (params$mapres == "canada")     params$basemap = "basemap.10min.ps"
    
    return(params)
  }


