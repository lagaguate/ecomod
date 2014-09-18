	
  convert.degmin2degdec = function (x, y=NULL) {
    if (is.null(y)) {
      x$lat = trunc(x$lat) + round((x$lat - trunc(x$lat)) /60 * 100, 6)
      x$lon = trunc(x$lon) + round((x$lon - trunc(x$lon)) /60 * 100, 6)
    } else {
      if (y=="lat") x = floor(x /100)+(x - 100*floor(x /100))/60
      if (y=="lon") x = - (floor(x/100)+(x-100*floor(x/100))/60)
    }
    return (x)
  }


