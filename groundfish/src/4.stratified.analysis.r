 p = list()
 p$init.files = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 

p$strat=440:495
p$series =c('summer')# p$series =c('4vswcod');p$series =c('georges')
p$years.to.estimate = c(1970:2013)
p$species = 11
p$vessel.correction = F
p$length.based = F
p$length.grouping = 1
p$size.class= c(30,60)
p$by.sex = F

#out = groundfish.db(DS='gsdet.spec.redo',p=p)
p$alpha = 0.05

#out = groundfish.analysis(DS='ab.redo',p=p)


p$species = c(10,11,12,13,14,16,23,30,31,40,41,42,43,50,60,200,201,202,203,204,300,320,400,610,640)

Xpaper=T
if(Xpaper){
p$years.to.estimate = 1999:2013
p$strat = c(470:483)
p$species = c(2526)
}
#
p$clusters = c( rep( "localhost", 7) )

p = make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)
p$runs = p$runs[order(p$runs$v),]
#parallel.run(groundfish.analysis,DS='stratified.estimates.redo',p=p,specific.allocation.to.clusters=T) #silly error arisingexit


aout= groundfish.analysis(DS='stratified.estimates.redo',p=p)


#redo a's and b's
p$alpha = 0.05
out = groundfish.analysis(DS='ab.redo',p=p)


#figure stratified analysis Note--the values after comments are the other options
p$add.reference.line = F
p$time.series.start.year = 1999
p$time.series.end.year = 2013
p$reference.start.year = 1999
p$reference.end.year = 2013

p$metric = 'numbers' #weights
p$measure = 'stratified.total' #'stratified.total'

p$reference.measure = 'median' # mean, geomean
p$file.name = 'snowcrab.inshore.4x.png'
figure.stratified.analysis(x=aout,p=p)


