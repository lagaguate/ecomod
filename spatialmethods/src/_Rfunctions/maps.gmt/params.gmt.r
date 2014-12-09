
  params.gmt = function(params=NULL) {

    params = NULL

    # default gmt-related parameters

    params$gmtcol       = "seis"  # "red2green" is an alternate
    params$maskres      = "-S20k"
    params$interpres    = "-nn"
    params$tension      = "-T0.75"
    params$bathy.tension = "-T0.75"  # tension parameter for GMT splines-in-tension (1=harmonic surface ... max,min not exceeded)
    params$bathy.maskres  = "-S40k"        # resolution
    params$bathy.zrange = "-Sa-1/NaN -Sb-501/NaN" # show 0 to 800 m depth range
    params$bathy.contour= "-C100 -S4 -W0.25p/40" # contour lines every 100m with lines of various thinknesses

    params$block = T  # do a block ?
    params$blocktype = "mean"  # do a block mean ?
    params$polygon.options = "-W0.8p"
    params$coast.options = "-Df -G220 -W0.5p"  # resolution and colour
    params$scale.location = "-D4.75i/0.75i/2.6i/0.16ih" # alternate: "-D4.5i/0.8i/2.5i/.25ih"
    params$fnt = "18p"
    params$psresolution = "300"
    params$delta  = 1
    params$overlay = ""
    params$annot.lon0 = -63.7
    params$annot.lat0 = 47.25
    params$annot.fontsize = 32 # points
    params$annot.fontno = 0 # try  pstext  -L for possibles .. 0 = helvetica
    params$annot.fontcol = 0 # try  pstext  -L for possibles .. 0 = helvetica
    params$annot.angle = 0
    params$annot.justify = "LT"  # justification
    params$annot.text = ""

  return(params)
  }
 
