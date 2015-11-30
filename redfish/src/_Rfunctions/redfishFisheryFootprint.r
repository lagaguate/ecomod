redfishFisheryFootprint <- function(x,res=0.1,fig.lab=NULL,survey.data=NULL) {
			fn = file.path(project.datadirectory('redfish'),'figures')
			xr = bufferRange(x$X)
			yr = bufferRange(x$Y)
			
			grid<-makeGrid(x=seq(xr[1],xr[2],res), y=seq(yr[1],yr[2],res), projection="LL")
			yy = sort(unique(x$fishing_year))
			yy = yy[which(yy>2003)]
			locData =findCells(x,grid)
			x = merge(x,locData,by='EID',all.x=T)
			xy = aggregate(rpt_weight_kgs~PID+SID+fishing_year,data=x,FUN=sum)
			xy$lL = log(xy$rpt_weight_kgs)
			xq = quantile(xy$lL,c(0.25,0.4,0.55,0.7,0.85,0.95))	
			xq = log(round(exp(xq)/5)*5)

				lbrks<-length(xq)
				cols<-c(brewer.pal(lbrks+1,'Reds'))

		for(y in yy) {
					xx = xy[which(xy$fishing_year==y),]
					xx$Z = xx$lL
					xx<-as.PolyData(xx)
					xx<-makeProps(xx,xq,"col",cols)
					xx[which(xx$Z<xq[1]),'col'] <- cols[1]				
					xx[which(xx$Z>xq[length(xq)]),'col'] <- cols[length(cols)]				
			if(is.null(fig.lab)) {pdf(file.path(fn,paste('fisheryFootprint',y,'pdf',sep=".")))}
			if(!is.null(fig.lab)){pdf(file.path(fn,paste('fisheryFootprint',fig.lab,y,'pdf',sep=".")))}
			LobsterMap(title=y,ylim=c(42,46.5),xlim=c(-68,-58),boundaries='uu',labels='nn',addSummerStrata=F,poly.lst = list(grid,xx))
			ContLegend('bottomright',bty='n',cex=0.8,lvls = round(exp(xq)),Cont.data = data.frame(col=cols))
 			if(!is.null(survey.data)){
 				t = survey.data[which(survey.data$yr==y),]
 				t$slong = t$slong*-1
				z = as.matrix(t[,grep('totno',names(t))])
				draw.pie(x=t$slong,y=t$slat,z=z,radius=.2,col=rgb(0,1,0,0.2))
			}
 			dev.off()
 		}
 	}