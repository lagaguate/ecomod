
  bottom.contact.parameters = function( bcp=list() ) {
    
    if (bcp$datasource =="snowcrab") {
      # snow crab specific defaults  
    
    }


    if (bcp$datasource =="groundfish") {
      # parameters tuned to different years of data for groundfish survey 
      bcp$depth.range = c(-65, 65)
    
    
      if ( bcp$nr < 3500 & bcp$nr >= 2000 ) {
        bcp$noisefilter.var.window = 30
        bcp$noisefilter.target.r2 = 0.8
        bcp$noisefilter.trim = 0.1
        bcp$noisefilter.quants = c(0.1, 0.9)
      } 

      if ( bcp$nr < 2000 & bcp$nr >= 1200 ) {
        bcp$noisefilter.var.window = 20
        bcp$noisefilter.target.r2 = 0.8
        bcp$noisefilter.trim = 0.1
        bcp$noisefilter.quants = c(0.1, 0.9)
      } 

      if ( bcp$nr < 1200 &  bcp$nr >= 400  ) {
        bcp$noisefilter.trim = 0.05
        bcp$noisefilter.quants = c(0.1, 0.9)
      } 
      
      if ( bcp$nr < 400 ) {  # tend to be very noisy
        bcp$noisefilter.trim = 0.05 
        bcp$noisefilter.quants = c(0.1, 0.9)
      } 
  

    }


    # basic defaults for any that have not yet been defined

    if ( !exists("id", bcp)) bcp$id="noid"
    if ( !exists("datasource", bcp)) bcp$datasource ="datasource"
    if ( !exists("nr", bcp)) bcp$nr = NA
    if ( !exists("YR", bcp)) bcp$YR = NA

    if ( !exists("tdif.min", bcp)) bcp$tdif.min=10 # min time difference (minutes)
    if ( !exists("tdif.max", bcp)) bcp$tdif.max=50  # max time difference (minutes) .. including tails
    if ( !exists("depthproportion", bcp)) bcp$depthproportion=0.6  # depthproportion controls primary (coarse)gating
    if ( !exists("depth.min", bcp)) bcp$depth.min=20
    if ( !exists("depth.range", bcp)) bcp$depth.range=c(-60,60)
    if ( !exists("setdepth", bcp)) bcp$setdepth=NA
    if ( !exists("time.gate", bcp)) bcp$time.gate=NA
    if ( !exists("eps.depth", bcp)) bcp$eps.depth = 1 # m

    if ( !exists("noisefilter.trim", bcp)) bcp$noisefilter.trim = 0.04  # proportion of data to remove/trim based upon adjacent differences that are too extreme and local variance windows
    if ( !exists("noisefilter.var.window", bcp)) bcp$noisefilter.var.window = 5  # 1/2 of moving window used to compute local variance and moving window mean
    if ( !exists("noisefilter.inla.h", bcp)) bcp$noisefilter.inla.h = 0.05
    if ( !exists("noisefilter.inla.diagonal", bcp)) bcp$noisefilter.inla.diagonal = 0.05
    if ( !exists("noisefilter.quants", bcp)) bcp$noisefilter.quants = c(0.05, 0.95)
    if ( !exists("noisefilter.sd.multiplier", bcp)) bcp$noisefilter.sd.multiplier = 4 
    if ( !exists("noisefilter.target.r2", bcp)) bcp$noisefilter.target.r2 = 0.8 # for noise filtering  .. ignore variations less than this threshold

    if ( !exists("smooth.filter.quants", bcp)) bcp$smooth.filter.quants=c(0.05, 0.95) # for dZ 
    if ( !exists("smooth.target.r2", bcp)) bcp$smooth.target.r2 = 0.8  # for smooth of dZ (slopes)
    if ( !exists("smooth.windowsize",  bcp))  bcp$smooth.windowsize = 5  # number of data points to assess for consistent passage into the non-modal (slopes) area
    if ( !exists("smooth.threshold",  bcp))  bcp$smooth.threshold = 0.5  # proportion of data outside of mode in window to consider  consistent passage into the non-modal (slopes) area
 
    if ( !exists("modal.sd.multiplier", bcp)) bcp$modal.sd.multiplier=2 # to detect if end point has been prefiltered/truncated 
    if ( !exists("modal.trim", bcp)) bcp$modal.trim = 0.001  # keep low as it is really only for interpolation of NAs
    if ( !exists("modal.filter.quants", bcp)) bcp$modal.filter.quants = c(0.05, 0.95) # as above 
    if ( !exists("modal.windowsize",  bcp))  bcp$modal.windowsize = 5  # number of data points to assess for consistent passage into the non-modal (depths) area
    if ( !exists("modal.threshold",  bcp))  bcp$modal.threshold = 0.5  # proportion of data outside of mode in window to consider  consistent passage into the non-modal (depths) area
    
    if ( !exists("maxdepth.sd.multiplier", bcp)) bcp$maxdepth.sd.multiplier = 2 # 
    
    if ( !exists("linear.sd.multiplier", bcp)) bcp$linear.sd.multiplier=2 # to detect if end point has been prefiltered/truncated 
    if ( !exists("linear.trim", bcp)) bcp$linear.trim = 0.001  # keep low as it is really only for interpolation of NAs
    if ( !exists("linear.filter.quants", bcp)) bcp$linear.filter.quants = c(0.05, 0.95) # as above 

    if ( !exists("user.interaction", bcp)) bcp$user.interaction=FALSE  # if you want to try to manually determine end points too

    return(bcp)
  
  }


