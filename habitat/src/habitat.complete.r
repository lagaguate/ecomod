# create required annual datasets for predictions of habitat and abundance
# ultimately to speed up processing by creating merged data that are repeatedly used by many other functions/analysis

# 1. create parameter list

p = list()
p$libs = RLibrary( "mgcv", "sp", "gstat",  "parallel", "fields", "chron", "lubridate", "raster", "rgdal"  ) 
p$init.files = loadfunctions( c(
  "spacetime", "utility", "parallel", "habitat", "substrate", "bathymetry", "speciesarea", "metabolism", 
  "sizespectrum", "speciescomposition", "temperature", "biochem", "condition" 
) )

p$taxa = "maxresolved"
p$season = "allseasons"
p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) 
p$interpolation.nmax = 100 


p$yearstomodel = 1970:2015
p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far


p$speciesarea.modeltype = "complex"
p$speciesarea.method = "glm"   ## this is chosen in speciesarea.r ... make sure it matches up
p$speciesarea.season = "allseasons"  
p$speciesarea.taxa = "maxresolved"  # use only unique taxa
p$speciesarea.data.sources = c("groundfish", "snowcrab")
p$speciesarea.variables = c( "C", "Z", "T", "Npred" )

p$speciescomposition.modeltype = "complex"  
p$speciescomposition.season = "allseasons"  
p$speciescomposition.taxa = "maxresolved"  
p$speciescomposition.variables = c( "ca1", "ca2" )

p$sizespectrum.modeltype = "complex" 
p$sizespectrum.taxa = "maxresolved"
p$sizespectrum.season = "allseasons"
p$sizespectrum.variables = c( "nss.b1", "nss.rsquared", "nss.shannon")

p$condition.modeltype = "complex" 
p$condition.taxa = "maxresolved"
p$condition.season = "allseasons"
p$condition.variables = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic", 
                           "coSmallPelagic", "coLargePelagic", "coSmallDemersal", "coLargeDemersal")

p$metabolism.modeltype = "complex" 
p$metabolism.taxa = "alltaxa"
p$metabolism.season = "allseasons"
p$metabolism.variables = c( "smr", "Pr.Reaction" , "Ea", "A", "qn", "qm", "mass", "len"  )
p$clusters = rep("localhost", detectCores() )


# 4. This step needs to be completed after all other incoming db are refreshed ... add biologicals 
### loadfunctions ( "bio", functionname="bio.r" )  
### loadfunctions ( "speciesarea", functionname="speciesarea.r" ) 
### loadfunctions ( "speciescomposition", functionname="speciescomposition.r" ) 
### loadfunctions ( "sizespectrum", functionname="sizespectrum.r" ) 
### loadfunctions ( "metabolism", functionname="metabolism.r" ) 
### loadfunctions ( "condition", functionname="condition.r" ) 
#
# TODO :: biologicals begin in 1970 ..  need to fix 
#        .. at present data from 1970 are copied to all pre 1970 data years

p = make.list( list( yrs=p$yearstomodel), Y=p )
parallel.run(  habitat.db, DS="complete.redo", p=p )
# habitat.db ( DS="complete.redo", p=p )