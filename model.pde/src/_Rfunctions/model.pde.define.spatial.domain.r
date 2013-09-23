
  model.pde.define.spatial.domain = function(p) {


    if (p$spatial.domain == "snowcrab") {

      p$taxa = "maxresolved"
      p$season = "allseasons"

      # resolution and region
      p$internal.projection = "utm20"
      p$pres = 1

      p$lon0=-66.5
      p$lon1=-56.5

      p$lat0=42.5
      p$lat1=47.5

      p$corners = data.frame(lon=c(p$lon0,p$lon1), lat=c(p$lat0,p$lat1))
      p$corners = lonlat2planar( p$corners, proj.type=p$internal.projection )

      p$plons = seq(min(p$corners$plon), max(p$corners$plon), by=p$pres)
      p$plats = seq(min(p$corners$plat), max(p$corners$plat), by=p$pres)
      p$nplats = length(p$plats)
      p$nplons = length(p$plons)
 
      # R, deSolve, image use rows as X-direction --- longitude =  rows
      p$nr = p$nplons   # no. cells in x direction (cols)
      p$nc = p$nplats   # no. cells in y direction (rows)
      p$nrc = p$nr * p$nc
      
      p$dr = p$pres # distance between nodes in x-direction (cols) -- km
      p$dc = p$pres # distance between nodes in y-direction (rows) -- km

    }

    return(p)

  }

