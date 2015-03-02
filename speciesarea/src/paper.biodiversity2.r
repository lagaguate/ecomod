
  # Species-area analysis 
  ### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db"


# create base species area stats  ... a few hours

  p = list()
  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$init.files = loadfunctions( c("spacetime", "utility", "parallel", "bathymetry", "temperature", "habitat", "taxonomy", "bio", "speciesarea"    ) )
  p$data.sources = c("groundfish", "snowcrab") 
  p$speciesarea.method = "glm" 
  
  p$pred.radius = 50 # km
  p$timescale = c( 0, 1, 2 ) # yr
  p$lengthscale = c( 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100, 110, 120 )  # km used in counting for rarefaction curve
  p$interpolation.distances = 25  # habitat interpolation scale
   
  p$taxa = "maxresolved"
  # p$taxa = "family.or.genera"
  # p$taxa = "alltaxa"
  
  p$season = "allseasons"

  # choose:
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep( "localhost", 2 )
  # p$clusters = rep( "localhost", 8 )
  p$clusters = rep( "localhost", 24 )

  # p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("kaos", 24 ) )

   
  # map everything  ~ 30 minutes
  p$yearstomodel = 1970:2011 # set map years separately to temporal.interpolation.redo allow control over specific years updated
  p$varstomodel = c( "C", "Z", "T", "sar.rsq", "Npred" )
  # p$mods = c("simple","simple.highdef", "time.invariant", "complex" ) 
  p$mods =  c("simple","simple.highdef" )

  p$taxa = "maxresolved"
  # p$season = "alldata"



  bio = bio.db(DS="XXX", p)
  
# --- the following need to be modified ... 
  sc = bio[ which(bio$data.source =="snowcrab") ,]
  
	gf = bio[ which(bio$data.source =="groundfish") ,]

  length( unique(sc$sp$spec) )
  length( unique(gf$sp$spec) )
  length( unique(bio$sp$spec) )

  # breakdown by large taxonomic groups
  tx = taxonomy.db("complete")  # contains a cleaned list of all taxa found in region
  sc$sp = merge( sc$sp, tx, by="spec", all.x=T, all.y=F, sort=F )
  gf$sp = merge( gf$sp, tx, by="spec", all.x=T, all.y=F, sort=F )
  bio$sp = merge( bio$sp, tx, by="spec", all.x=T, all.y=F, sort=F )



