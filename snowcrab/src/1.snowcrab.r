# ----------------------------------------------------------------------------------
# NOTE : 
#  
#  1. The year in the file "current.assessment.year.r" must be changed every year.
#     It must be kept separate from "initialise.local.environment.r" as running in parallel mode 
#     requires occasionally overrding some parameters in "p". This override cannot be completed as  
#     "initialise.local.environment.r" is sourced with every initialisation of a a new CPU.
# ----------------------------------------------------------------------------------
   
# load required functions and parameters 
	
  loadfunctions( "snowcrab", functionname="initialise.local.environment.r") 
  
  if (debug) {
    p$do.parallel =F
    p$clusters= c("localhost")
  }

  
  # get data tables from Oracle server and store local copies
  # !!!!!! --------- these should be run on a windows machine: !!!!!!!!! <--------- READ THIS
 
    peaks = function(x) {
      # find adjacent local peaks
      l = length(x)
      xm1 = c(x[-1], x[l])
      xp1 = c(x[1], x[-l])
      out = which(x > xm1 & x > xp1 | x < xm1 & x < xp1 )
      return(out)
    }




  if (obtain.database.snapshot) {
    snowcrab.db( DS="set.odbc.redo", yrs=1996:p$current.assessment.year ) 
    snowcrab.db( DS="det.odbc.redo", yrs=1996:p$current.assessment.year ) 
    snowcrab.db( DS="cat.odbc.redo", yrs=1996:p$current.assessment.year ) 
    logbook.db(  DS="odbc.logbook.redo", yrs=1996:p$current.assessment.year ) 
    logbook.db(  DS="odbc.licence.redo" ) 
    logbook.db(  DS="odbc.areas.redo" ) 
    observer.db( DS="odbc.redo", yrs=1996:p$current.assessment.year )  
  }

# -------------------------------------------------------------------------------------
# produce base data files from snowcrab logbook database (marfis) and historical data
# and the observer databases: 
# this needs to be done after the above datadumps to refresh locally stored databases

  if (make.fisheries.data) {
    observer.db( DS="odb.redo", p=p )
    # fishing ground are used for determination of contraints for interpolation
    logbook.db( DS="logbook.redo", p=p )
    logbook.db( DS="logbook.filtered.positions.redo", p=p )
    logbook.db( DS="fishing.grounds.redo",  p=p )
    logbook.db( DS="logbook.gridded.redo", p=p )
  }


# -------------------------------------------------------------------------------------
# create base set data and add all historical data fixes

  if (get.base.data) {
    
    # sequence is important ... do not change 
    
    # creates initial rdata and sqlite db
    snowcrab.db( DS="setInitial.redo", p=p ) # this is required by the seabird.db (but not minilog and netmind) 
  
    seabird.yToload = 2012:p$current.assessment.year
      seabird.db( DS="load", Y=p$current.assessment.year ) # this begins 2012; requires "setInitial"
      seabird.db( DS="set.seabird.lookuptable.redo", Y=seabird.yToload )
      seabird.db( DS="stats.redo", Y=seabird.yToload ) # requires minilog stats .. do last ~ 2min/yr hrs

    
    # bring in the raw data for netmind and minilogs: incremental or a sequence of years
    # in 2004, new BIO data streams began: Assume historical data are correct
    minilog.yToload = 1999:p$current.assessment.year
      minilog.db( DS="load", Y=p$current.assessment.year ) # minilog data series "begins" in 1999 -- 60 min?
      minilog.db( DS="set.minilog.lookuptable", Y=minilog.yToload ) ## annual updates seem not to work -- need to check .. use full refresh until fixed 
      minilog.db( DS="stats.redo", Y=minilog.yToload ) # ~ 2hr for 1999 to 2010 
      

    netmind.yToload = 1999:p$current.assessment.year
      netmind.db( DS="load", Y=p$current.assessment.year) # netmind data series "begins" in 1998 -- 60 min?
      netmind.db( DS="set.netmind.lookuptable", Y=netmind.yToload )
      netmind.db( DS="stats.redo", Y=netmind.yToload ) # requires minilog stats .. do last ~ 3.5 hrs


    snowcrab.db( DS="set.clean.redo", proj.type=p$internal.projection )
    snowcrab.db( DS="det.initial.redo", p=p )
    snowcrab.db( DS="det.georeferenced.redo" ) 
    snowcrab.db( DS="cat.initial.redo", p=p )
    snowcrab.db( DS="cat.georeferenced.redo" )
    snowcrab.db( DS="set.merge.det.redo" )
    snowcrab.db( DS="set.merge.cat.redo" )  

   }  # end base data


# -------------------------------------------------------------------------------------
# External Dependencies: (must be completed before the final lookup/mathcing phase)
#
#     Bathymetry data :: loadfunctions("bathymetry", functionname="bathymetry.r" ) # if necessary
#     Substrate type  :: loadfunctions("substrate", functionname="substrate.r" ) # if necessary
#     Groundfish data :: loadfunctions( "groundfish", functionname="1.groundfish.r" ) 
#       NOTE  groundfish.db( DS="odbc.redo" ) must be done manually on a windows machine and data snapshots moved to local system
#     Taxonomy :: loadfunctions("taxonomy", functionname="taxonomy.r" ) # if necessary
#     BIO db update :: loadfunctions ( "bio", functionname="bio.r" ) 
#     Temperatures ::  loadfunctions ( "temperature", functionname="temperature.r" ) 
#     Species area data :: loadfunctions ( "speciesarea", functionname="speciesarea.r" ) 
#     Species composition data :: loadfunctions ( "speciescomposition", functionname="speciescomposition.r" ) 
#     Size spectrum data :: loadfunctions ( "sizespectrum", functionname="sizespectrum.r" ) 
#     Metabolism data :: loadfunctions ( "metabolism", functionname="metabolism.r" ) 
#     Habitat data :: loadfunctions ( "habitat", functionname="habitat.r" ) 
#       NOTE:: This glues all the above together in planar coord system to allow fast lookup of data for 
#       matching with set, logbook data



# -------------------------------------------------------------------------------------
# Final data lookup/matching .. AFTER refreshing all above tables (where relevent/possible)
  logbook.db( DS="fisheries.complete.redo", p=p )  
  snowcrab.db( DS ="set.complete.redo", p=p )   
  snowcrab.db( DS ="set.logbook.redo", yrs=1996:p$current.assessment.year ) # add gridded fisheries data
  
  make.timeseries.data(p=p, areas=p$regions )  #  timeseries of means of all survey data
  #  tsdata = snowcrab.db("set.timerseries")

# create a new lookuptable for data transformations after refreshing set data/ranges
  REPOS = recode.variable.initiate.db ( db="snowcrab" )

# snow crab found in external databases tapped into for habitat determination
  for ( vs in c( "R0.mass", "male.large", "male.small", "female.large", "female.small" ) ) {
    ### -------- not yet finished this one ...  TODO
    snowcrab.external.db(p=p, DS="set.snowcrab.in.groundfish.survey.redo", vname=vs )
  }



# simple geometric means of raw data:  used by indicators ordination and some figures
  # takes many hours ... need to make parallel  TODO 
  tsdata =  get.time.series ( x=snowcrab.db( DS="set.logbook"),
    regions=p$regions, vars=variable.list.expand("all.data"), from.file=F, trim=0 )



#  ----- experimental / tests / ideas
testing = F
if (testing) {
  s =  snowcrab.db( DS ="set.complete" )
  d =   snowcrab.db( DS ="det.georeferenced" ) 
  l = merge( d, s[, c("trip", "set", "t")], by=c("trip", "set"), all.x=T, all.y=F)
  rm(s,d); gc()
  l = l[ which( as.numeric(as.character(l$mat)) %in% c(mature, immature)  &
                l$sex %in% c(male, female) ) , ]
  l$sex[ which( l$sex ==male) ] = "male"
  l$sex[ which( l$sex ==female) ] = "female"

  l$sex= factor( as.character(l$sex) )
  l$mat = factor( as.character( l$mat))

  m = glm( t~ as.factor(mat) * as.factor(sex), data= l, family=gaussian())
  require(car)

  Anova( m)
  require(effects)
  k=all.effects(m, l)
  plot(k)


  # -------------------------------------------------------------------------------------
  # make size at maturity estimates in a spatial context

  if( make.maturity.db ) {
    maturity = make.maturity.spatial( distance=50 )
    save(maturity, file="maturity.rdata", compress=T)
    # load(file.path( project.directory("snowcrab"), "snowcrab", "R", "maturity.rdata"))
  }




  # -------------------------------------------------------------------------------------
  # example plot mechanism
  p = spatial.parameters( type="snowcrab" )
	xyz=bathymetry.db(p=p, DS="baseline.planar.500")
  map( xyz, xyz.coords="planar", cfa.regions=T, depthcontours=T, pts=NULL, annot=NULL, fn="test", loc=getwd() )

} # end testing

