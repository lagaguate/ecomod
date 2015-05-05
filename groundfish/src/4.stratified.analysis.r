 p = list()
 p$init.files = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
 p$init.files = c(p$init.files,loadfunctions( "BIOsurvey") 

p$strat=440:495
p$series =c('summer')# p$series =c('4vswcod');p$series =c('georges')
p$years.to.estimate = c(1970:2014)
#p$species = c(202,204,300,320)
p$vessel.correction = T
p$vessel.correction.fixed = 1.2
p$length.based = F
p$size.class= c(30,60)
p$by.sex = F

#out = groundfish.db(DS='gsdet.spec.redo',p=p)
p$alpha = 0.05

#out = groundfish.analysis(DS='ab.redo',p=p)
#MPA functional groups 
p$functional.groups = T
yy = list()
yy[['FPBSM']] = c(14,19,42,118,143,149,300,320)
yy[['FPBL']] = c(10,12,15,16,30,31,40,141,200,204,216,220,400,704)
yy[['FPPSML']] = c(63,159,169,712)
yy[['FBBS']] = c(44,302,303,304,306,307,313,314,316,331,340,341,350,502,503,505,508,520,603,617,621,626,637,816,880)
yy[['FBBM']] = c(17,41,43,111,114,115,122,123,142,156,202,203,301,308,410,412,512,616,620,622,631,647,742)
yy[['FBBL']] = c(11,50,51,52,201,221,411,604,630,640,743)
yy[['FPlPSM']] = c(60,61,62,160,163,165,610,625,720,721)
yy[['FZBSM']] = c(13,23,112,409,414,501,595,602,623,641,714,744)
yy[['FZPSM']] = c(64,70,158,646,701,711)
yy[['IBBS']] = c(2211,2212,2213,2221,2312,2316,2313,2319,2411,2414,2415,2417,2420,2511,2513,2519,2521,2523,2527,2532,2541,2555,2559,3200,3501,4211,4221,5100,6113,6117,6119,6125,6411,6413,6421,6511,8346,8347)
yy[['IBBM']] = c(2525,2526,2528,2550,6101,6111,6121,6123)
yy[['IZSML']] = c(4512,4536,8520)
yy[['IFBC']] = c(8322,8323,8324,8325,8326,8329,8330)
yy[['IFBnC']] = c(1823,4304,4312,4317,4321,4322,4331,4332,8335,8356,8601)
yy[['ID']] = c(6115,6201,6211,6213,6600,6611)
#p$species = c(10,11,12,13,14,16,23,30,31,40,41,42,43,50,60,200,201,202,203,204,300,320,400,610,640)
p$species = c('FPBSM','FPBL','FPPSML','FBBS','FBBM','FBBL','FPlPSM','FZBSM','FZPSM','IBBS','IBBM','IZSM',
  'IFBC','IFBnC','ID')
p$yy = yy

Xpaper=F
  if(Xpaper){
          p$years.to.estimate = 1999:2013
          p$strat = c(470:483)
          p$species = c(2526)
          p$file.name = 'snowcrab.inshore.4x.png'
          }
#
p$clusters = c( rep( "localhost", 7) )

p = make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)
p$runs = p$runs[order(p$runs$v),]
#parallel.run(groundfish.analysis,DS='stratified.estimates.redo',p=p,specific.allocation.to.clusters=T) #silly error arisingexit

#not finished

aout= groundfish.analysis(DS='stratified.estimates.redo',p=p)

#habitat associations
p$strata.files.return =T
p$plot.name = 'white.hake.4vw.habitat.associations.pdf'
aout= groundfish.analysis(DS='stratified.estimates.redo',p=p)
figure.habitat.associations(aout,p=p)

#redo a's and b's
p$alpha = 0.05
out = groundfish.analysis(DS='ab.redo',p=p)


#figure stratified analysis Note--the values after comments are the other options
p$add.reference.lines = T
p$time.series.start.year = 1970
p$time.series.end.year = 2013
p$reference.start.year = 1999
p$reference.end.year = 2013
p$add.primary.line = F # the center estimate for reference point
p$metric = 'weights' #weights
p$measure = 'stratified.total' #'stratified.total'

p$reference.measure = 'median' # mean, geomean 
p$file.name = 'winterskate-4vsw.png'
       
#stock reference lines based on primary measure as above
  p$add.upper.lower = T
        p$upper.reference.line = 0.8
        p$lower.reference.line = 0.4
        
        p$figure.title = 'Winter skate 4VsW'
        p$y.maximum = NULL # NULL # if ymax is too high for one year
	p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

        p$legend.placement = 'topright'
        p$running.median = T
		p$running.length = 3
		p$running.mean = F #can only have rmedian or rmean

		

     ref.out=   figure.stratified.analysis(x=aout,p=p)

sfp = file.path(fp,'analysis','saved p files')
dir.create(sfp,recursive=T,showWarnings=F)
save(p,file=file.path(sfp,paste('pfile',p$species,p$series,'strata',min(p$strat),max(p$strat),'rdata',sep=".")))
 




