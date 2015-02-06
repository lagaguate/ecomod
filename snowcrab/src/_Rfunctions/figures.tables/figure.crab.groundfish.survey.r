figure.crab.groundfish.survey <- function(species , outdir=file.path(project.directory('snowcrab'), 
	"assessments",p$current.assessment.year)) {
		require(latticeExtra)
    require(boot)
      areas.crab = c("cfa4x", "cfasouth", "cfanorth" )
      areas.groundfish = list(cfa4x = c(470,483),cfasouth = c(443,467),cfanorth=c(440,442))
      regions = c("4X", "S-ENS", "N-ENS")
      n.regions = length(regions)
      n.areas = length(areas.crab)
	      ft2m = 0.3048
	      m2km = 1/1000
	      nmi2mi = 1.1507794
	      mi2ft = 5280
	      sakm2 = (41 * ft2m * m2km ) * ( 1.75 * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled in km^2
		
 
 set = snowcrab.db( DS="set.complete")
 set = set[which(set$yr>2003),]
		 ii  = names(set)[grep(paste("^ms.mass.",species,"$",sep=""),names(set))]
		 set = set[,c('yr','cfa',ii)]
		 names(set)[3] = 'mass'
		 ou = unique(set[,c('yr','cfa')])
		 ou = ou[order(ou$cfa,ou$yr),]
		 ou$mean = ou$upper = ou$lower = 0
		 boot.mean <- function(x,i){mean(x[i])}
 #get the bs estimates for snowcrab
 for(j in 1:nrow(ou)) {
 	lp = set[which(set$yr==ou[j,'yr'] & set$cfa==ou[j,'cfa']),'mass']
if(all(lp==0)) next()
 	ir = boot(lp,statistic=boot.mean,R=1000)
 	irr = boot.ci(ir,type='bca')
 	ou[j,c('lower','upper','mean')] = c(irr$bca[c(4,5)],ir$t0)
 }


 gr.dir = file.path(project.directory('groundfish'),'analysis')
 gr.list = list()

 for(i in 1:n.regions) {
 		io = paste('stratified',species,'summer.strata',areas.groundfish[[c(i,1)]],areas.groundfish[[c(i,2)]],sep=".")
 		ioi = grep(io,dir(gr.dir))
 		if(length(ioi)==0) stop(paste(paste(gr.dir,io,sep="/"),'not found'))
 		if(length(ioi)>1) stop(paste(paste(gr.dir,io,sep="/"),'has multiple entries'))
 		load(paste(gr.dir,dir(gr.dir)[ioi],sep="/"))
 		out = out[,c('yr','w.yst','w.ci.yst.l','w.ci.yst.u')]
 		names(out) = c('yr','mean','lower','upper')
 		out$cfa = areas.crab[i]
 		out$mean = out$mean / sakm2
 		out$lower = out$lower / sakm2
 		out$upper = out$upper / sakm2
 		gr.list[[i]] = out
 		}
gr.list = do.call(rbind,gr.list) 
gr.list$grp = 'groundfish'
ou$grp = 'snowcrab'

xlim=c(1968,p$current.assessment.year+2)

oi = rbind(ou,gr.list)
      #dir.create( outdir, recursive=T, showWarnings=F )
#      fn = file.path( outdir, paste( "groundfish.snowcrab.survey.species",species, "png", sep="." ) )
#      png( file=fn,units='in', width=15,height=12,pointsize=18, res=300,type='cairo')

#setup.lattice.options()
#w <- list(left.padding = list(x = 0.5, units = "inches"),right.padding = list(x = 0.5, units = "inches"))
#h <- list(bottom.padding = list(x = 0, units = "inches"), top.padding = list(x = 0, units = "inches"))
#attice.options(layout.widths = lw, layout.heights = lh) 

#     plg = xyplot( mean~yr|cfa, data=gr.list, upper=gr.list$upper, lower=gr.list$lower,
#       layout=c(1,n.regions), 
#       par.strip.text = list(cex=2),
#       par.settings=list(  
#         axis.text=list(cex=1.8), 
#         par.main.text = list(cex=1.5),
#         layout.heights=list(strip=0.8, panel=3, main=1.1),
#         layout.widths=list(ylab.axis.padding=1.5,units='inches') 
#       ),
#       xlim=xlim, scales = list(y = "free"),
#       xlab=list("Year", cex=2), ylab=list("Kg per km2", cex=2),
#       prepanel = function(x,y,subscripts,lower,upper,...) {
#         list(ylim=c(0,max(upper[subscripts])*0.75))
#       },        
#       panel = function(x, y, subscripts, lower, upper, ...) {
#         larrows(x, lower[subscripts], x, upper[subscripts], angle = 90, code = 3, length=0.05,lwd=3)
#         panel.xyplot(x, y, type="b", lty=1, lwd=6, pch=20, col="black",...)
#        }
#     )
#     plc = xyplot( mean~yr|cfa, data=ou, upper=ou$upper, lower=ou$lower,
#       layout=c(1,n.regions), 
#       par.strip.text = list(cex=2),
#       par.settings=list(  
#         axis.text=list(cex=1.8), 
#         par.main.text = list(cex=1.5),
#         layout.heights=list(strip=0.8, panel=3, main=1.1),
#        layout.widths=list(ylab.axis.padding=1.5,units='inches')
#       ),
#       xlim=xlim, scales = list(y = "free"),
#       xlab=list("Year", cex=2), ylab=list("Kg per km2", cex=2),
#       prepanel = function(x,y,subscripts,lower,upper,...) {
#         list(ylim=c(0,max(upper[subscripts])*0.75))
#       },
#       panel = function(x, y, subscripts, lower, upper, ...) {
#         larrows(x, lower[subscripts], x, upper[subscripts], angle = 90, code = 3, length=0.05,lwd=3)
#         panel.xyplot(x, y, type="b", lty=1, lwd=6, pch=20, col="red",...)
#        }
#     ) 
#     doubleYScale(plg,plc,rows=3,add.ylab2=T)
#     update(trellis.last.object(), par.settings = simpleTheme(col = c("black", "red")))

#     graphics.off()

#######

###TO DO## this figure is not working properly but can run through one at a time.  AMC Feb2015

for(l in areas.crab) {

ooi = oi[oi$cfa==l,]
     fn = file.path( outdir, paste( "groundfish.snowcrab.survey.species.one.axis",species,l, "png", sep="." ) )

    x11()
    browser()
    panel.ci <- function(x, y, ly, uy, subscripts, pch = 20, col=c('black','red'), ...)   { 
        x <- as.numeric(x) 
        y <- as.numeric(y) 
        ly <- as.numeric(ly[subscripts]) 
        uy <- as.numeric(uy[subscripts]) 
        panel.arrows(x, ly, x, uy, col = col, 
                 length = 0.25, unit = "native", 
                 angle=90,code=3)
        panel.xyplot(x, y, pch = pch, col= col,, ...) 
        } 
xyplot(mean ~ yr, 
        groups=grp, 
        data=ooi, 
        ly = ooi$lower, 
        uy = ooi$upper, 
        prepanel = function(x,y,uy)
         list(ylim=c(0,max(uy)*0.75))
        ,ylab=expression("kg per km"^2),
        xlab='Year',
        main=unique(ooi$cfa),
        panel = panel.superpose, 
        panel.groups = panel.ci, 
        type="b",col=c('black','red'),pch=20,lwd=3
       ) 
  # savePlot(fn,type='png')
   print(fn)
 }
 }  

