

image.scale <- function(z, zlim, col = heat.colors(12),      breaks, horiz=TRUE, ...){
	#//This function creates a color scale for use with the image()
#function. Input parameters should be consistent with those
#used in the corresponding image plot. The "horiz" argument
#defines whether the scale is horizonal(=TRUE) or vertical(=FALSE).
#Example
#pal.1=colorRampPalette(c("black", "red", "yellow"), space="rgb")
#pal.2=colorRampPalette(c("black", "blue", "cyan"), space="rgb")
# 
#layout(matrix(c(1,2), nrow=2, ncol=1),heights=c(4,1))
##1st image
#breaks <- seq(min(volcano), max(volcano),length.out=100)
#par(mar=c(1,1,1,1))
#image(seq(dim(volcano)[1]), seq(dim(volcano)[2]), volcano, 
#col=pal.1(length(breaks)-1), breaks=breaks, xaxt="n", yaxt="n", ylab="", xlab="")
##Add additional graphics
#highest <- which.max(volcano)
#points(highest %% dim(volcano)[1], highest %/% dim(volcano)[1], 
#pch=2, lwd=2, cex=2,col="blue")
##Add scale
#par(mar=c(3,1,1,1))
#image.scale(volcano, col=pal.1(length(breaks)-1), breaks=breaks, horiz=TRUE)
#box()

	if(!missing(breaks)){
		if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
	}
	if(missing(breaks) & !missing(zlim)){
		breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
	}
	if(missing(breaks) & missing(zlim)){
		zlim <- range(z, na.rm=TRUE)
		breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
	}
	poly <- vector(mode="list", length(col))
	for(i in seq(poly)){
		poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
	}
	xaxt <- ifelse(horiz, "s", "n")
	yaxt <- ifelse(horiz, "n", "s")
	if(horiz){ylim<-c(0,1); xlim<-range(breaks)}
	if(!horiz){ylim<-range(breaks); xlim<-c(0,1)}
	plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
	for(i in seq(poly)){
		if(horiz){
			polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
		}
		if(!horiz){
			polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
		}
	}
}

