
  speciesarea.parameters = function( p=NULL ) {
    
    if (is.null(p)) p=list()
    
    if (! exists("project.outdir.root", p)) p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )  #required for interpolations and mapping 

    # faster to use RAM-based data objects but this forces use only of local cpu's
    # configure SHM (shared RAM memory to be >18 GB .. in fstab .. in windows not sure how to do this?)
    # file-backing is slower but can use all cpu's in a distributed cluster
    if (! exists("use.bigmemory.file.backing", p)) p$use.bigmemory.file.backing = FALSE  

    if (! exists("clusters", p)) p$clusters = rep("localhost", detectCores() ) 
    if (! exists("data.sources", p))   p$data.sources = c("groundfish", "snowcrab") 
    if (! exists("speciesarea.method", p)) p$speciesarea.method = "glm" 
  
    if (! exists("pred.radius", p)) p$pred.radius = 50 # km
    if (! exists("timescale", p)) p$timescale = c( 0, 1, 2 ) # yr
    if (! exists("lengthscale", p)) p$lengthscale = c( 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100, 110, 120 )  # km used in counting for rarefaction curve
    if (! exists("interpolation.distances", p)) p$interpolation.distances = 25  # habitat interpolation scale
   
    if (! exists("taxa", p)) p$taxa = "maxresolved" # p$taxa = "family.or.genera", "alltaxa"
    if (! exists("season", p)) p$season = "allseasons"

    if (! exists("varstomodel", p))  p$varstomodel = c( "C", "Z", "T", "Npred" )
    if (! exists("default.spatial.domain", p))  p$default.spatial.domain = "canada.east"

    if (! exists("modtype", p))  p$modtype = "complex" 
    if (! exists("prediction.dyear", p))  p$prediction.dyear = 0.75 # =9/12 ie., 1 Sept
    if (! exists("nw", p))  p$nw = 10
    if (! exists("spatial.knots", p))  p$spatial.knots = 100

    if (! exists("optimizer.alternate", p))  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is newton, then this .. see GAM options

    return(p)

  }


