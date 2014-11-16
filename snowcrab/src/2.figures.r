   
  # Figures obtained after completion of data assimilation and processing up to the end of "1.snowcrab.r"
   
	
	 loadfunctions( "snowcrab", functionname="initialise.local.environment.r") 
   loadfunctions( "plotting.methods") 
 
   p$do.parallel = FALSE  # mapping in parallel is broken .. must fix ::TODO


   p$clusters = rep("localhost", 24 )
   p$clusters = rep("localhost", 8 )



  # ------------------------------------------
  # Time-series: Fisheries landings
    figure.landings.timeseries( yearmax=p$current.assessment.year, outdir=file.path( p$annual.results,  "timeseries","fishery"), outfile="landings.ts" )

  # ------------------------------------------
  # Time-series: Fisheries effort 
    figure.effort.timeseries( yearmax=p$current.assessment.year, outdir=file.path( p$annual.results,"timeseries", "fishery"), outfile="effort.ts" )

  # ------------------------------------------
  # Time-series: Fisheries CPUE
    figure.cpue.timeseries( yearmax=p$current.assessment.year, outdir=file.path( p$annual.results,"timeseries", "fishery"), outfile="cpue.ts" )

  # ------------------------------------------
  # Size frequency distributions, broken down by moult category from at-sea observed data 
    figure.observed.size.freq( regions = c("cfanorth", "cfasouth", "cfa4x"), years="all", outdir=file.path( p$annual.results, "figures", "size.freq", "observer")  )   

  # ------------------------------------------
  # Size-frequency distributions of snow crab cw from trawl data, broken down by maturity classes
    histograms.size.maturity( outdir=file.path( p$annual.results, "figures", "size.freq", "survey"),  redo.data=T )
 
    
  # ------------------------------------------
  # Timeseries of all survey variables
    figure.timeseries.males( outdir=file.path(p$annual.results, "timeseries", "survey") )
    figure.timeseries.females( outdir=file.path(p$annual.results, "timeseries", "survey") )



  # ------------------------------------------
  # Timeseries of all survey variables
    figure.timeseries.survey(p, areas=c("cfanorth", "cfasouth", "cfa4x"), from.file=F )


  # ------------------------------------------
  # Timeseries: geometric mean density of R-1 recruits
    figure.timeseries.recruits( outdir=file.path(p$annual.results, "timeseries", "survey") )

  # ------------------------------------------
  # Timeseries: geometric mean density of CC5 crab
    figure.timeseries.CC5( outdir=file.path(p$annual.results,  "timeseries", "survey") )

  # ------------------------------------------
  # Timeseries: sex ratios of immature crab
    figure.timeseries.sexratios( outdir=file.path(p$annual.results,  "timeseries", "survey" ), type="immature" )

  # ------------------------------------------
  # Timeseries: sex ratios of mature crab
    figure.timeseries.sexratios( outdir=file.path(p$annual.results,  "timeseries", "survey"), type="mature" )
  
  # ------------------------------------------
  # Timeseries: geometric mean carapace width from trawl surveys 
    figure.timeseries.CW( outdir=file.path(p$annual.results,  "timeseries", "survey"), type="trawl" )
  
  # ------------------------------------------
  # Timeseries: geometric mean carapace width from at-sea observers 
    figure.timeseries.CW( outdir=file.path(p$annual.results,  "timeseries", "survey"), type="observer" )
     
   

  
  # ------------------------------------------
  # Map: Basemap of the Scotian Shelf used by all other mapping routines
  #   creating a partial postscript file via GMT 
  #   .. only required if changing resolution or spatial extent
    p$inp = ""

    gmt.basemap (p)

  # ------------------------------------------
  # Map: Scotian Shelf with CFA lines and labels
  #   this is the basemap from map.r which is then post-labelled in sodipodi
    p$outdir = file.path(p$annual.results,"figures")
    p$outfile.basename = file.path(p$outdir, "map.CFAs")
    p$basemap = file.path( project.directory("snowcrab"), "R", p$basemap)
    map.basemap.with.cfa.lines( p, conversions=c("ps2png")  )


  # ------------------------------------------
  # Map:  Mean/geometric mean of various variables in the set data table
    map.set.information( p, outdir=file.path( project.directory("snowcrab"), "R", "maps", "survey" )  )

  # ------------------------------------------
  # Map: Numerical density of by-catch species 
    map.cat.information( p, outdir=file.path( project.directory("snowcrab"), "R", "maps", "species" ) )

  # ------------------------------------------
  # Map:Fisheries logbook data (Effort, CPU, Landings)
    map.fisheries.data( p, outdir=file.path(project.directory("snowcrab"), "R", "maps", "logbook") )

  # ------------------------------------------
  # Map: Survey locations
    
    map.survey.locations( p, basedir=file.path(project.directory("snowcrab"), "R", "maps", "survey.locations"),  newyear=F, map.method="lattice"  )
    map.survey.locations( p, basedir=file.path(project.directory("snowcrab"), "R", "maps", "survey.locations"),  newyear=F, map.method="googleearth"  )

  # ------------------------------------------
  # Map: Observer locations
    map.observer.locations( p, basedir=file.path(project.directory("snowcrab"), "R", "maps","observer.locations" ), newyear=F , map.method="lattice"  )

  # ------------------------------------------
  # Map: Logbook recorded locations
    map.logbook.locations( p, basedir=file.path(project.directory("snowcrab"), "R", "maps","logbook.locations" ), newyear=F , map.method="lattice"  )

  # ------------------------------------------
  # Map: Crab movement from mark-recapture data
    map.movement( p, outdir=file.path(project.directory("snowcrab"), "R", "maps", "mark.recapture") ) 
  
  # ------------------------------------------
  # Map: Spatial representation of maturity patterns of snow crab
    map.maturity( p, outdir=file.path(project.directory("snowcrab"), "R", "maps", "maturity"), newyear=T ) 
 





  ##########################################################################
  ###############################  Retired figures #########################



  # ------------------------------------------
  # Habitat usage comparisons (bivariate) ... requires the full "set.rdata" database and "logbook.dZ.rdata" database
    habitat.usage( usevar="totno.all", covariate="depth", outdir = file.path(p$annual.results, "habitat.templates") )
  
  # ------------------------------------------
  # Habitat usage comparisons (bivariate) ... requires the full "set.rdata" database and "logbook.dZ.rdata" database
    habitat.usage( usevar="totno.all", covariate="temperature", outdir = file.path(p$annual.results, "habitat.templates") )

  # ------------------------------------------
  # Habitat usage comparisons (bivariate) ... requires the full "set.rdata" database and "logbook.dZ.rdata" database
    habitat.usage( usevar="totno.all", covariate="bottom.slope", outdir = file.path(p$annual.results, "habitat.templates") )

  # ------------------------------------------
  # Habitat usage comparisons (bivariate) ... requires the full "set.rdata" database and "logbook.dZ.rdata" database
    habitat.usage( usevar="totno.all", covariate="bottom.curvature", outdir = file.path(p$annual.results, "habitat.templates") )
  
  # ------------------------------------------
  # Habitat usage comparisons (bivariate) ... requires the full "set.rdata" database and "logbook.dZ.rdata" database
    habitat.usage( usevar="totno.all", covariate="substrate", outdir = file.path(p$annual.results, "habitat.templates") )


  # ------------------------------------------
  # Timeseries: Larval brachyura from the SSIP data 
    figure.timeseries.larvae( outdir=file.path(project.directory("snowcrab"), "R", "timeseries", "larvae") ) 

  # ------------------------------------------
  # Growth as a a function of instar for Scotian Shelf snow crab
    figure.growth.instar( outdir=file.path(project.directory("snowcrab"), "R", "growth") )

 
  # ------------------------------------------
  # Map: Larval distributions from the Scotian Shelf Ichtyoplankton Program data
    map.larvae( p, outdir=file.path(project.directory("snowcrab"), "R", "maps", "larvae"), conversions=conversions, init.files=p$init.files ) 


