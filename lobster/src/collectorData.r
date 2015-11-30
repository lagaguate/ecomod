loadfunctions('lobster')
options(stringsAsFactors=F)
fp = file.path(project.datadirectory('lobster'),'data')

x = read.csv(file.path(fp,'CollectorData.csv'))
x$Study.Area = trimws(x$Study.Area,'right') #lobster bay has space in name sometimes

#set up counter for individuals
x$n = 1

#distinct number of collectors sampled per year

a = data.frame(unique(cbind(x$Year,x$Collector.Number,x$Study.Area)))
names(a) = c('Year','Collector.Count','Study.Area')
a = aggregate(Collector.Count~Year+Study.Area,data=a,FUN=length)
x = merge(x,a,by=c('Year','Study.Area'))
#cpue by area by year
ax = aggregate(n~Study.Area+Year+Collector.Count+Common.Name,data=x,FUN=sum)

ax$Density = ax$n/ax$Collector.Count

axL = ax[which(ax$Common.Name== 'American Lobster'),]
axL = axL[sort(axL$Study.Area,axL$Year),]