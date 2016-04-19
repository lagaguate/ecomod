get.ocmd.areas <- function(requested=c("All"), get.detailed=F)
{
  library(sp)
  if ("All" %in% requested | "Musquash" %in% requested){
    #Musquash is so huge that it makes the code load more slowly, and I don't 
    #want to see it if I don't have to
   loadfunctions(projectname="mpa", functionname="get.Musquash")
    }
    
  get.Haddock_Closed_Area = function(detailed = get.detailed){
    this=SpatialPolygons(list(Polygons(list(
      Polygon(matrix(c(-63.333333,43.35, -63.333333,43.016667, 
                       -62.5,43.06667, -62,43.066667, 
                       -61.3,43.316667, -61.3,44.033333, 
                       -61.7,44.033333, -61.7,44.033333, 
                       -62.733333,43.7, -63.333333,43.35), 
                     ncol = 2, byrow=TRUE))),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
    return(this)
  }
  get.Vazella_Emerald = function(detailed = get.detailed){
    this=SpatialPolygons(list(Polygons(list(
        Polygon(matrix(c(-62.66666666666666,44.33333333333334, 
                         -62.59166666666667,44.33333333333334, 
                         -62.53333333333333,44.25, 
                         -62.53333333333333,44.15, 
                         -62.66666666666666,44.15, 
                         -62.66666666666666,44.33333333333334), 
                       ncol = 2, byrow=TRUE))),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    return(this)
  }
  get.Vazella_Sambro = function(detailed = get.detailed){
    this=SpatialPolygons(list(Polygons(list(
        Polygon(matrix(c(-63.11666666666667,43.93333333333333, 
                         -63.05,43.93333333333333, 
                         -62.99999999999999,43.9, 
                         -63.11666666666667,43.83333333333334, 
                         -63.11666666666667,43.93333333333333), 
                       ncol = 2, byrow=TRUE))),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    return(this)
  }
  get.Lophelia = function(detailed = get.detailed){
    this=SpatialPolygons(list(Polygons(list(
        Polygon(matrix(c(-57.20833333333334,44.49166666666667, 
                         -57.166667,44.49166666666667, 
                         -57.15,44.45833333333334, 
                         -57.2,44.45833333333334, 
                         -57.20833333333334,44.49166666666667), 
                       ncol = 2, byrow=TRUE))),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
    return(this)
  }
  get.Gully = function(detailed = get.detailed){
    if (detailed == F){
      this=SpatialPolygons(list(Polygons(list(
        Polygon(matrix(c(-59.09999999999917,44.21666699999535, 
                         -59.33333299999829,44.09999999999538, 
                         -59.13333299999912,43.91666699999538, 
                         -59.13333299999919,43.58333299999542, 
                         -58.58333299999983,43.5833329999954, 
                         -58.58333299999982,43.78333299999537, 
                         -59.09999999999917,44.21666699999535), 
                       ncol = 2, byrow=TRUE))),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
      return(this)
    }else{
      this=SpatialPolygons(list(Polygons(list(
        Polygon(matrix(c(-59.09999999999917,44.21666699999535, 
                         -59.33333299999829,44.09999999999538, 
                         -59.13333299999912,43.91666699999538, 
                         -59.13333299999919,43.58333299999542, 
                         -58.58333299999983,43.5833329999954, 
                         -58.58333299999982,43.78333299999537, 
                         -59.09999999999917,44.21666699999535), 
                       ncol = 2, byrow=TRUE)),
        Polygon(matrix(c(-59.03770799999938,43.6999519999954, 
                         -58.98667699999945,43.77222899999538, 
                         -59.01195599999941,43.77722899999539, 
                         -59.0186219999994,43.82778399999538, 
                         -58.99792699999944,43.83228399999538, 
                         -58.99667599999943,43.87222799999537, 
                         -59.03112099999937,43.89972799999537, 
                         -59.03612099999935,43.93056199999538, 
                         -59.02028599999939,43.96472799999536, 
                         -59.04445399999933,43.98222799999536, 
                         -59.00750799999939,43.98222799999536, 
                         -59.02611899999936,44.01056099999538, 
                         -59.05861999999928,44.03389399999534, 
                         -59.08389799999922,44.08083899999535, 
                         -59.13723199999907,44.10806099999536, 
                         -59.13723199999907,44.11833899999535, 
                         -59.07223099999926,44.09111599999535, 
                         -58.96500599999947,44.00500499999536, 
                         -58.95556299999949,43.93861599999536, 
                         -58.8822279999996,43.90361599999537, 
                         -58.86444999999961,43.88694899999535, 
                         -58.86417199999962,43.84778299999536, 
                         -58.84583799999962,43.86861599999536, 
                         -58.79991999999967,43.88194899999537, 
                         -58.79992299999968,43.69994999999538, 
                         -59.03770799999938,43.6999519999954), 
                       ncol = 2, byrow=TRUE)),
        Polygon(matrix(c(-58.97333299999944,44.11194399999535, 
                         -59.01111099999937,44.09249999999537,
                         -58.94389499999949,44.00055999999536, 
                         -58.92583899999952,43.94556099999537, 
                         -58.83278199999965,43.88889399999535, 
                         -58.7499999999997,43.92666699999535, 
                         -58.97333299999944,44.11194399999535), 
                       ncol = 2, byrow=TRUE)),
        Polygon(matrix(c(-59.09945399999919,44.04778399999537, 
                         -59.21326856675336,43.98978597309883, 
                         -59.13333299999912,43.91666699999538, 
                         -59.13333299999916,43.6947219999954, 
                         -59.08723699999928,43.71750799999541, 
                         -59.03390099999935,43.79389599999536, 
                         -59.04278899999935,43.82750699999538, 
                         -59.01556599999941,43.85722899999536, 
                         -59.05334399999932,43.89611799999538, 
                         -59.0533429999993,43.97611699999536, 
                         -59.09251099999922,43.99028399999536, 
                         -59.06417599999927,43.99333899999537, 
                         -59.08084299999924,44.03194999999536, 
                         -59.10195499999919,44.03722799999536, 
                         -59.09945399999919,44.04778399999537), 
                       ncol = 2, byrow=TRUE))
      ),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
      return(this)
    }
  }
  get.NE_Channel = function(detailed = get.detailed){
    if (detailed == F){
      this=SpatialPolygons(list(Polygons(list(
        Polygon(matrix(c(-65.63278709199994,42.11676329000005, 
                         -65.73279046699992,42.06676446400007, 
                         -65.74930092699992,41.99999730100006,
                         -65.6993257229999,41.95501439100008, 
                         -65.66612725099992,41.92444179100005, 
                         -65.56596301299993,41.83333190900004, 
                         -65.43262442899993,41.94999584300007, 
                         -65.63278709199994,42.11676329000005), 
                       ncol = 2, byrow=TRUE))),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
      return(this)
    }else{
      this=SpatialPolygons(list(Polygons(list(
        Polygon(matrix(c(-65.73279046699992,42.06676446400007, 
                         -65.74930092699992,41.99999730100006, 
                         -65.6993257229999,41.95501439100008, 
                         -65.6992992399999,42.00833092300007, 
                         -65.67429825999994,42.00833077600004, 
                         -65.73279046699992,42.06676446400007), 
                       ncol = 2, byrow=TRUE)),
        Polygon(matrix(c(-65.63278709199994,42.11676329000005, 
                         -65.73279046699992,42.06676446400007, 
                         -65.67429825999994,42.00833077600004, 
                         -65.6992992399999,42.00833092300007, 
                         -65.6993257229999,41.95501439100008, 
                         -65.66612725099992,41.92444179100005, 
                         -65.56596301299993,41.83333190900004, 
                         -65.43262442899993,41.94999584300007, 
                         -65.63278709199994,42.11676329000005), 
                       ncol = 2, byrow=TRUE))
      ),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
      return(this)
    }
  }
  get.St_Ann = function(detailed = get.detailed){
    if (detailed == F){
      this=SpatialPolygons(list(Polygons(list(
        Polygon(matrix(c(-59.5,46.1666666666667, 
                         -59.3333333333333,46.1666666666667, 
                         -59.3333333333333,46.2666666666667, 
                         -59,46.4166666666667, 
                         -58.6666666666667,46.4166666666667, 
                         -58.3666666666667,46.2333333333333, 
                         -58.5333333333333,46.0666666666667, 
                         -58.6666666666667,46.0666666666667, 
                         -58.6666666666667, 45.9608333333333, 
                         -58.6666666666667,45.9333333333333, 
                         -58.9166666666667,45.8960833333333, 
                         -59.0333333333333, 45.8784722222222,
                         -59.5,45.8068055555556, 
                         -59.65, 45.7833333333333,
                         -59.65, 46.1666666666667,
                         -59.5, 46.1666666666667), 
                       ncol = 2, byrow=TRUE))),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
      return(this)
    }else{
      this=SpatialPolygons(list(Polygons(list(
        Polygon(matrix(c(-59.5,46.1666666666667, 
                         -59.3333333333333,46.1666666666667, 
                         -59.3333333333333,46.2666666666667, 
                         -59,46.4166666666667, 
                         -58.6666666666667,46.4166666666667, 
                         -58.3666666666667,46.2333333333333, 
                         -58.5333333333333,46.0666666666667, 
                         -58.6666666666667,46.0666666666667, 
                         -58.85,46.2, 
                         -58.9333333333333,46.1333333333333, 
                         -58.6666666666667,45.9608333333333, 
                         -58.6666666666667,45.9333333333333, 
                         -58.9166666666667,45.8960833333333, 
                         -58.9166666666667,46, 
                         -59.0333333333333,46, 
                         -59.0333333333333,45.8784722222222, 
                         -59.5,45.8068055555556, 
                         -59.5,46.1666666666667), 
                       ncol = 2, byrow=TRUE)),
        Polygon(matrix(c(-59.65,46.1666666666667, 
                         -59.5,46.1666666666667, 
                         -59.5,45.8068055555556, 
                         -59.65,45.7833333333333, 
                         -59.65,46.16666666666), 
                      ncol = 2, byrow=TRUE)),
        Polygon(matrix(c(-59.0333333333333,45.8784722222222, 
                         -59.0333333333333,46, 
                         -58.9166666666667,46, 
                         -58.9166666666667,45.8960833333333, 
                         -59.0333333333333,45.8784722222222 ), 
                      ncol = 2, byrow=TRUE)),
        Polygon(matrix(c(-58.6666666666667,46.0666666666667, 
                         -58.6666666666667,45.9608333333333, 
                         -58.9333333333333,46.1333333333333, 
                         -58.85,46.2, 
                         -58.6666666666667,46.0666666666667), 
                      ncol = 2, byrow=TRUE))
      ),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
      return(this)
    }
  }
  get.FundyWhale = function(detailed = get.detailed){
    this=SpatialPolygons(list(Polygons(list(
      Polygon(matrix(c(-66.36669999999999,44.55, 
                       -66.61669999999999,44.43, 
                       -66.61669999999999,44.7, 
                       -66.45,44.81667, 
                       -66.2833,44.7833, 
                       -66.36669999999999,44.55), 
                     ncol = 2, byrow=TRUE))
    ),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
    return(this)
  }
  get.RosewayWhale = function(detailed = get.detailed){
    this=SpatialPolygons(list(Polygons(list(
      Polygon(matrix(c(-64.98333358764648,42.78333473205566, 
                       -65.51666641235352,42.64999961853027, 
                       -66.08333206176756,42.86666679382324, 
                       -64.91666603088379,43.26666831970215, 
                       -64.98333358764648,42.78333473205566), 
                     ncol = 2, byrow=TRUE))
    ),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
    return(this)
  }
  get.Haldimand = function(detailed = get.detailed){
    this=SpatialPolygons(list(Polygons(list(
      Polygon(matrix(c(-57.88472222000002,44.23611111000002, 
                       -57.88472222000002,44.08472222, 
                       -58.03472221999993,44.08472222, 
                       -58.03472221999993,44.25, 
                       -57.93888888999998,44.3027777800001, 
                       -57.88472222000002,44.23611111000002), 
                     ncol = 2, byrow=TRUE))
    ),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
    return(this)
  }
  get.Shortland = function(detailed = get.detailed){
    this=SpatialPolygons(list(Polygons(list(
      Polygon(matrix(c(-58.28611110999998,44.12222222000003, 
                       -58.28611110999998,43.96666667000005, 
                       -58.42916666999997,43.96666667000005, 
                       -58.42916666999997,44.19583333000003, 
                       -58.39027777999996,44.19583333000003, 
                       -58.28611110999998,44.12222222000003), 
                     ncol = 2, byrow=TRUE))
    ),1)),proj4string = CRS("+proj=longlat +datum=WGS84"))
    return(this)
  }
  
  areas=list()
  if ("All" %in% requested) {
    areas$Haddock_Closed_Area=get.Haddock_Closed_Area()
    areas$Vazella_Emerald=get.Vazella_Emerald()
    areas$Vazella_Sambro=get.Vazella_Sambro()
    areas$Lophelia=get.Lophelia()
    areas$Gully=get.Gully()
    areas$NE_Channel=get.NE_Channel()
    areas$St_Ann=get.St_Ann()
    areas$Musquash=get.Musquash()
    areas$FundyWhale=get.FundyWhale()
    areas$RosewayWhale=get.RosewayWhale()
    areas$Haldimand=get.Haldimand()
    areas$Shortland=get.Shortland()
  }else if ("MPA" %in% requested){
    areas$Gully=get.Gully()
    areas$Musquash=get.Musquash(get.detailed)
  }else if ("CCA" %in% requested){
    areas$Vazella_Emerald=get.Vazella_Emerald()
    areas$Vazella_Sambro=get.Vazella_Sambro()
    areas$Lophelia=get.Lophelia()
    areas$NE_Channel=get.NE_Channel()
    areas$Gully=get.Gully()
  }else{
    for (i in 1:length(requested)){
      if ("Haddock_Closed_Area" %in% requested[i])  areas$Haddock_Closed_Area=get.Haddock_Closed_Area()
      if ("Vazella_Emerald" %in% requested[i])  areas$Vazella_Emerald=get.Vazella_Emerald()
      if ("Vazella_Sambro" %in% requested[i]) areas$Vazella_Sambro=get.Vazella_Sambro()
      if ("Lophelia" %in% requested[i]) areas$Lophelia=get.Lophelia()
      if ("Gully" %in% requested[i])  areas$Gully=get.Gully()
      if ("NE_Channel" %in% requested[i]) areas$NE_Channel=get.NE_Channel()
      if ("St_Ann" %in% requested[i]) areas$St_Ann=get.St_Ann()
      if ("Musquash" %in% requested[i]) areas$Musquash=get.Musquash(get.detailed)
      if ("FundyWhale" %in% requested[i]) areas$FundyWhale=get.FundyWhale()
      if ("RosewayWhale" %in% requested[i]) areas$RosewayWhale=get.RosewayWhale()
      if ("Haldimand" %in% requested[i]) areas$Haldimand=get.Haldimand()
      if ("Shortland" %in% requested[i]) areas$Shortland=get.Shortland()
    }
  }
  return(areas)
}
