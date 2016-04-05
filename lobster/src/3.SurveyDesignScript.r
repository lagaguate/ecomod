
  p = list()

	p$init.files = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
loadfunctions(c('utiltiy','lobster'))

p$libs = RLibrary( c( "chron", "lubridate", "parallel","sp" )  )

  
 odbc.data.yrs=1970:2015
 
  groundfish.db( DS="gscat.redo" )
  groundfish.db( DS="gsdet.redo" )
  groundfish.db( DS="gsinf.redo" )
  
  


gi = groundfish.db('gsinf')
gc = groundfish.db('gscat')
gc = gc[which(gc$spec==2550),]
gic =  merge(gi,gc,by='id',all.x=T)

i = which(is.na(gic$spec))
gic$spec[i] = 2550
gic$totno[i] = 0
gic$totwgt[i] = 0
gic = makePBS(gic,polygon=F)

#abundance and weight in per km2
gic$totwgt = gic$totwgt / gic$dist_km 
gic$totno = gic$totno / gic$dist_km 


LFAs<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFAPolys.csv"))


LobsterMap('34')
addPoints(na.omit(gic[which(gic$yr>=1999),c('X','Y','EID')]),pch=16,col='red',cex=0.75)
addPoints(na.omit(gic[which(gic$yr>=1999 & gic$totno>0),c('X','Y','EID')]),pch=16,col='green',cex=0.75) #lobster only


  # -------------------------------------------------------------------------------------
