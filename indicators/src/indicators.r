 


  require(chron)

	loadfunctions( ("common") )
	loadfunctions( ("sorted.ordination") ) 
	loadfunctions( ("indicators") ) 
 
  setwd( project.directory("indicators") )
  
  
  # refresh the survey data
  # DEMOGRAPHICS goto:: http://www.gov.ns.ca/finance/communitycounts/dataview.asp?gnum=pro9012&gnum2=pro9012&chartid=&whichacct=&year2=&mapid=&ptype=&gtype=&yearid=2006&acctype=0&gname=&dcol=&group=&group1=&group2=&group3=&gview=3&table=table_d17&glevel=pro
  
  indic = indicators.db( db="indicators.all.refresh.all.data.streams" ) # refresh each timeseries
  indic = indicators.db( db="indicators.all.glue" )  # glue all time-series together
  # indic = indicators.db( db="indicators.all" ) # load the glued version
  

 # indic$data$Nwells.drilled = cumsum.jae(indic$data$Nwells.drilled)
 # indic$data$seismic.2D = cumsum.jae(indic$data$seismic.2D)
 # indic$data$seismic.3D = cumsum.jae(indic$data$seismic.3D)


  t0 = 1960
  t1 = 2011

  # ordination of selected key factors
  
  d = indicators.subset ( indic, type="keyfactors" )
  Y = pca.analyse.data(d, t0, t1, fname=file.path(project.directory("indicators"), "keyfactors" ) )
  

  sub = indic$data[, c("T_bottom_misaine", "SST_halifax", "ice_coverage.km.2", "Gulf.Stream.front.Ref.62lon", "T_sable_annual", "snowcrab.bottom.habitat.area", "snowcrab.kriged.R0.mass", "snowcrab.fishery.landings", "snowcrab.fishery.cpue", "groundfish.stratified.mean.temp" )]

  write.table(sub, file=file.path( project.directory( "snowcrab"), "research", "environ.management", "data.csv"), sep=";")


inn = names (indic$data) 
for (i in .keyfactors) {
  if ( i %in% inn ) next()
  print (i)
}


  ## smaller subsets


  # human 
  .human = c(indic$landings.totals.NS, indic$landedvalue.totals.NS, indic$human ) 
  .human = setdiff( .human, "No.Fish.processors" ) # this has no data yet
  Y = pca.analyse.data( indic$data, .human, t0, t1, fname=file.path(project.directory("indicators"), "human") )

  # fishery landings
  .fishery = c(indic$landings.NS, indic$landings.totals.NS ) 
  Y = pca.analyse.data( indic$data, .fishery, t0, t1, fname=file.path(project.directory("indicators"), "fishery" ))
  
  # fishery landed value
  .fishery.value = c(indic$landedvalue.NS, indic$landedvalue.totals.NS ) 
  Y = pca.analyse.data( indic$data, .fishery.value, t0, t1, fname=file.path(project.directory("indicators"), "landedvalue" ))

  # fishery -- overall
  .fishery = c(indic$landedvalue.NS, indic$landedvalue.totals.NS, indic$landings.NS, indic$landings.totals.NS ) 
  Y = pca.analyse.data( indic$data, .fishery, t0=1970, t1, fname=file.path(project.directory("indicators"), "fishery.overall" ))

  # snowcrab 
  .snowcrab = c(indic$snowcrab, "groundfish.stratified.mean.totwgt.snowcrab", "groundfish.stratified.mean.totno.snowcrab" ) 
  Y = pca.analyse.data(indic$data, .snowcrab, t0, t1, fname=file.path(project.directory("indicators"), "snowcrab" ))

  # climate
  .climate = c( indic$climate )
  Y = pca.analyse.data(indic$data, .climate, t0, t1, fname=file.path(project.directory("indicators"), "climate" ))

  # ecosystem
  .ecosystem = c( indic$ecosystem )
  Y = pca.analyse.data(indic$data, .ecosystem, t0, t1, fname=file.path(project.directory("indicators"), "ecosystem" ))




