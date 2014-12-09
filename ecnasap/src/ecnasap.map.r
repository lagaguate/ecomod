
  # map the data

  source(file.path(ecnasapdir, "ecnasap.functions.r"))

  
    #   plottimes = c("ecnasap.contrast", "five", "nao", "globalaverage")
    #   conversions = c("ps2png", "ps2pdf")
    conversions = c("ps2png")
    
    params = NULL
    params = list()
    params = params.gmt() # default settting here
    # params$overlay = c("nafo.2j", "nafo.3k", "nafo.3l", "nafo.3n", "nafo.3o", "nafo.3p", 
    #                    "nafo.4r", "nafo.4s", "nafo.4t", "nafo.4v", "nafo.4w", "nafo.4x", 
    #                    "nafo.5y", "nafo.5ze", "nafo.5zw" )
    params$overlay = c("nafo.2j3kl", "nafo.3no", "nafo.3p", "nafo.4rs", "nafo.4t", 
                       "nafo.4vw", "nafo.4x", "nafo.5y", "nafo.5zew" )
    params$polygon.options = "-W0.4p" 
    params$mapres     = "2min"
    params$gmt.projection.long = "Lambert.conformal.conic.ecnasap2"
    params$spatial.domain    = "ecnasap2"
    params$maskres    = "-S30k"
    params$interpres  = "-n50k"
    params$tension    = "-T0.25"
    params$bathy.tension = "-T0.3"  # tension parameter for GMT splines-in-tension 
                                    # 1 = harmonic surface ... max,min not exceeded
                                    # 0.35 = sharp fields
                                    # 0.25 = ssmooth fields
    params$bathy.res    = "-I2m"    # resolution  
    params$bathy.zrange = "-Sa1001/NaN -Sb1/NaN" # show 0 to 800 m depth range
    # params$bathy.contour= "-C100 -W0.5p/80" # contour lines every 100m with lines of various t
    params$bathy.contour= "-C200 -S1m -W0.2p/140" 
    params$annot.lon0 = -55
    params$annot.lat0 =  60
      
    params$basedir = "maps"
# params$outfile = file.path(params$basedir, paste("survey.locations", "ps", sep=".") )
    
    if (redo.basemap) gmt.basemap (params)
    
    if (map="set") {
      set = ecnasap.catches(source="file")
      variables = c( "allcaught", "grd", "pel", "shark", "pd_anom", "pred1", "pred2", 
                     "prey", "ntaxa", "ngrd" )
      # variables = c( "amPlaice", "atSpinyLumpsucker", "loScuplin", "noSandlance", 
      #                "spDogfish", "thSkate", "wiFlounder", "yeFlounder", "cod" )
      make.maps ( set, params, variables, plottimes, params$basedir, conversions)
    }
    
    if (map="ntaxa") {
      load("ntaxa.globalaverage.rdata")
      variables = c( "ntaxa.globalaverage.50" )
      set[,variables] = log(set[ ,variables] )
      i = which(set[,variables] > 2)
      set = set[i,]
      params$maskres    = "-S10k"
      params$interpres  = "-n10k"
      params$tension    = "-T0.4"
      make.maps ( set, params, variables, plottimes, params$basedir, conversions )
    }

    if (clean.ps) {
      files.to.delete = list.files( path=params$basedir, pattern="[*.ps]$", all.files=T, full.names=T, recursive=T)
      remove.files ( files.to.delete) 
    }

