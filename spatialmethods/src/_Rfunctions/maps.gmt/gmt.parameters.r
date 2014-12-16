
  gmt.parameters = function(p=NULL) {

    if( is.null(p)) p=list()

    # default gmt-related parameters
    p$gmtcol       = "seis"  # "red2green" is an alternate
    p$maskres      = "-S20k"
    p$interpres    = "-nn"
    p$tension      = "-T0.75"
    p$bathy.tension = "-T0.75"  # tension parameter for GMT splines-in-tension (1=harmonic surface ... max,min not exceeded)
    p$bathy.maskres  = "-S40k"        # resolution
    p$bathy.zrange = "-Sa-1/NaN -Sb-501/NaN" # show 0 to 800 m depth range
    p$bathy.contour= "-C100 -S4 -W0.25p" # contour lines every 100m with lines of various thinknesses

    p$block = T  # do a block ?
    p$blocktype = "mean"  # do a block mean ?
    p$polygon.options = "-W0.8p"
    p$coast.options = "-Df -G220 -W0.5p"  # resolution and colour
    p$scale.location = "-D4.75i/0.75i/2.6i/0.16ih" # alternate: "-D4.5i/0.8i/2.5i/.25ih"
    p$fnt = "18p"
    p$psresolution = "300"
    p$delta  = 1
    p$overlay = ""
    
    p$annot.lon0 = -63.7
    p$annot.lat0 = 47.25
    p$annot.fontsize = 32 # points
    p$annot.fontno = 0 # try  pstext  -L for possibles .. 0 = helvetica
    p$annot.fontcol = 0 # try  pstext  -L for possibles .. 0 = helvetica
    p$annot.angle = 0
    p$annot.justify = "LT"  # justification
    p$annot.text = ""

    p$tension = "-T0.4"  # 0.35+ for steep; 0.25 for smooth
    p$maskres = "-S25k"
    p$interpres = "-n40k"
    p$T.interp.method = "tps"  # tps is thin splate spline with GMT (alt: inverse distance using gstat)
    p$delete.postscript = T
    p$redo.basemap = F

    if ( exists( "spatial.domain", p) ) {
      if ( p$spatial.domain == "snowcrab" ) {
        p$mapres = "2min" #internal resolution for snow crab  
        p$overlay = c( "cfanorth", "cfasouth", "cfa4x")
        p$gmt.projection.long = "Lambert.conformal.conic.crab"
      }
      if ( p$spatial.domain == "SSE" ) {
        p$mapres = "2min" #internal resolution for snow crab  
        p$gmt.projection.long = "Lambert.conformal.conic" 
      }
      if ( p$spatial.domain == "canada.east" ) {
        p$mapres = "15sec"  # "15sec" is currently the highest resolution
        p$gmt.projection.long = "lambert.conic.canada.east" 
      }
    }
    
    p = gmt.resolution(p)  # req. "mapres" -> "basemap"(ps), "res" (-I params)
    p = gmt.projection(p)  # req. "gmt.projection.long" -> "gmtproj" (-J params)
    p = gmt.defineregion(p) # req. spatial.domain -> region (-R params)

    return(p)
  }
 
