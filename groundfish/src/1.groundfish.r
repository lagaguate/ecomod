
# the following create the basic datafiles from the database


# ------------------  Common initialisation for groundfish 
# ------------------
	
	loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 

# not too many as it has high memory requirements
# clusters=c("tethys", "tethys", "io", "io", "io" )
# clusters=c("tethys", "tethys", "tethys", "tethys", "lotka")
# clusters=rep("kaos",10)
  clusters=c("localhost")

# choose taxa or taxonomic groups of interest
  taxa = variable.list.expand("catch.summary")

  season = "summer"


  p = list()
  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far

  p$taxa =  "maxresolved"




# ------------------
# ------------------

# ---------
# primary data sets
# these should be run on a windows machine: NULL values get mangled for some reason
  
  odbc.data.yrs=1970:2013
    #  <<<<< ---- DATA YEAR can be a single year update too 
    # --- for import of data year only
  
  groundfish.db( DS="odbc.redo", datayrs=odbc.data.yrs )  
  
 
  groundfish.db( DS="gscat.redo" )
    refresh.bio.species.codes = F
    if (refresh.bio.species.codes ) {
      # the folowing is copied from taxaonomy/src/taxonomy.r
      # refresh BIO's species codes from Oracle 
      taxa.db( "spcodes.itis.redo" )  # new identitfication that have not yet moved into the official taxa databases added here
      taxa.db( "full.taxonomy.redo" ) 
      taxa.db( "life.history.redo" ) # add life history data (locally maintained in gstaxa_working.xls )
      tx = taxa.db( "complete" )
    }

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


  groundfish.db( "det.base.redo", r2crit=0.75 ) # ~ 10 min on io
  groundfish.db( "cat.redo" )  # add correction factors, and express per unit area
  groundfish.db( "det.redo" ) # ~ 10 min on io

  groundfish.db( "catchbyspecies.redo", taxa=taxa )


# ---------
# 3. sm_det.rdata .. summarize condition 
  groundfish.db( "set.det.redo", taxa=taxa )

  groundfish.db( "set.complete.redo", p=p )


  # -------------------------------------------------------------------------------------
# Run BIO.DB to update the multi-survey databases /home/jae/ecomod/bio/src/bio.r
# -------------------------------------------------------------------------------------
  # source( file.path( project.directory("bio"), "src", "bio.r") )
  loadfunctions ( "bio", functionname="bio.r" ) 


# create a lookuptable for data transformations
  REPOS = recode.variable.initiate.db ( db="groundfish" )





