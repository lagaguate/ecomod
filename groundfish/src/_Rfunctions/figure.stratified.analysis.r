figure.stratified.analysis <- function(x,p) {
	fn=file.path(project.directory('groundfish'),'analysis','figures')
	dir.create(fn,showWarnings=F)
	if(is.character(x)) {
		#if using file name
		load(file.path(project.directory('groundfish'),'analysis',x))
		x = out; rm(out)
		}
	#default is to use the object directly	

	with(p,{
		 png(file=file.path(fn,file.name),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')		
		m='Yst' ; mm = 'n'; lev='Stratified Total'; mt= 'Number'
		if(grepl('mean',measure)) {m = 'yst'; lev = 'Stratified Mean'}
		if(grepl('weight',metric)) {mm = 'w'; mt = 'Weight'}

		n1 = names(x)[grep(m,names(x))]
		n2 = names(x)[grep(mm,names(x))]
		n = intersect(n1,n2)
		xp = x[,c('yr',n)]
		names(xp) = c('year','mean','lower','upper')
		ylim=c(min(xp$lower),max(xp$upper))
		
		xpp = xp[which(xp$year>=time.series.start.year & xp$year<=time.series.end.year),  ]

			plot(xpp$year,xpp$mean,type='n',xlab='Year',ylab = paste(lev,mt,sep=" "),ylim=ylim)
			polygon(c(xpp$year,rev(xpp$year)),c(xpp$lower,rev(xpp$upper)),col='grey60', border=NA)
			points(xpp$year,xpp$mean,type='b',lty=1,pch=16,lwd=2)
			#lines(xpp$year,xpp$upper,lty=2,col='grey10')
			#lines(xpp$year,xpp$lower,lty=2,col='grey10')

	if(add.reference.line) {
			me = xp[which(xp$year>=reference.start.year & xp$year<=reference.end.year), 'mean' ]
			if(reference.measure=='median')	xref = median(me)	
			if(reference.measure=='mean')	xref = mean(me)	
			if(reference.measure=='geomean') xref = geomean(me)	
			lines(x=c(reference.start.year,reference.end.year),y=c(xref,xref),col='blue',lty=1,lwd=2)
		}
		print(file.path(fn,file.name))
	})
dev.off()

}