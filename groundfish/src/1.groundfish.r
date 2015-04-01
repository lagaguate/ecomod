
# the following create the basic datafiles from the database


# ------------------  Common initialisation for groundfish 
# ------------------
	
  p = list()

	p$init.files = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
  p$libs = RLibrary( c( "chron", "lubridate", "parallel","sp" )  )

# not too many as it has high memory requirements
# clusters=c("tethys", "tethys", "io", "io", "io" )
# clusters=c("tethys", "tethys", "tethys", "tethys", "lotka")
# clusters=rep("kaos",10)
  p$clusters = rep("localhost", detectCores() )
 
# choose taxa or taxonomic groups of interest
  p$taxa.of.interest = variable.list.expand("catch.summary")

  p$season = "summer"

  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far

  p$taxa =  "maxresolved"




# ------------------
# ------------------

# ---------
# primary data sets
# these should be run on a windows machine: NULL values get mangled for some reason
  
  odbc.data.yrs=1970:2014
    #  <<<<< ---- DATA YEAR can be a single year update too 
    # --- for import of data year only
  
  groundfish.db( DS="odbc.redo", datayrs=odbc.data.yrs )  
 


   
  refresh.bio.species.codes = F
  if (refresh.bio.species.codes ) {
    # the following is copied from taxaonomy/src/taxonomy.r
    groundfish.db( DS="spcodes.odbc.redo" )
    # bootstrap an initial set of tables .. these will be incomplete as a parsimonious tree needs to be created first but
    # it depends upon the last file created taxonomy.db("complete") .. so ...
    taxonomy.db( "groundfish.itis.redo" )  ## link itis with groundfish tables using taxa names, vernacular, etc
    taxonomy.db( "full.taxonomy.redo" )  # merge full taxonomic hierrachy (limit to animalia and resolved to species)
		## taxonomy.db( "parsimonious.redo" )  # (re)create lookups from old codes to a parsimonious species list
    taxonomy.db( "life.history.redo" ) # add life history data (locally maintained in groundfish.lifehistory.manually.maintained.csv )
    taxonomy.db( "complete.redo" )
    taxonomy.db( "parsimonious.redo" ) 
  }


  groundfish.db( DS="gscat.redo" )
  groundfish.db( DS="gsdet.redo" )
  groundfish.db( DS="gsinf.redo" )
  groundfish.db( DS="gshyd.profiles.redo" )
  groundfish.db( DS="gshyd.redo" )
  groundfish.db( DS="gshyd.georef.redo" )  # not used here but used in temperature re-analysis
   
 
# lookupregion = lookup.strata()  # create strata vs region lookup table

# ---------
# merged data sets
  groundfish.db( DS="set.base.redo" )
  groundfish.db( DS="cat.base.redo" )


******************
##### <<<<<<<<<<<<<<<< MUST ADD new routines to bring in Temp and Sal as gshyd is no longer maintained as of 2012/13
*******************


  groundfish.db( "det.base.redo")#, r2crit=0.75 ) # ~ 10 min 
  groundfish.db( "cat.redo" )  # add correction factors, and express per unit area
  groundfish.db( "det.redo" ) # ~ 10 min on io

  groundfish.db( "catchbyspecies.redo", taxa=p$taxa.of.interest )


# ---------
# 3. sm_det.rdata .. summarize condition 
  groundfish.db( "set.det.redo", taxa=p$taxa.of.interest )

  groundfish.db( "set.complete.redo", p=p )


  # -------------------------------------------------------------------------------------
# Run BIO.DB to update the multi-survey databases 
# -------------------------------------------------------------------------------------
  loadfunctions ( "bio", functionname="bio.r" ) 


# create a lookuptable for data transformations
  REPOS = recode.variable.initiate.db ( db="groundfish" )





