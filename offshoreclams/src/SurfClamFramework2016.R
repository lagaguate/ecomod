###############################################################################
##
##
##  Artic Surf Clam Framework 
##
##  June 2016
##
##  Brad Hubley
##  Susan Heaslip
##
##
##
###############################################################################
# To run in ecomod, run the following commands

loadfunctions("offshoreclams")

RLibrary( "PBSmapping" ) # Load required packages


## Load Data

update.data=FALSE # TRUE accesses data from database if on a DFO windows machine


  # log data
  log.data <- GetLogData(update=update.data)
  processed.log.data <- ProcessLogData(log.data)

  # length frequency data
  lf.data <- GetLFData(update=update.data)

  # survey data
  loadfunctions( "offshoreclams", functionname="survey.process.r") 











ClamMap2('all',isobath=seq(50,500,50))

 with(subset(processed.log.data,Year==2015),points(LON_DD,LAT_DD,pch=16,cex=0.2,col=rgb(1,0,0,0.2)))
 with(catch_analysis,points(SLON,SLAT,pch=16,cex=0.2,col=rgb(0,0,0,0.1)))

ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy')



ClamMap2('Grand',isobath=seq(50,500,50))



