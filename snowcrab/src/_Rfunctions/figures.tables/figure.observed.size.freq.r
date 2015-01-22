 figure.observed.size.freq = function(regions= c("cfanorth", "cfasouth", "cfa4x"), years=NULL, outdir=NULL ) {
    
    odb = observer.db( DS="odb")
    # Remove CW's outside norms 
    ii = which( odb$cw > 50 & odb$cw < 170) 
    odb = odb[ii,]
    
    if (years=="all") years = sort( unique( odb$fishyr ) )
years=2014
    for (reg in regions) {
      r = filter.region.polygon(x=odb, region=recode.areas(reg), planar=F)
      for (y in years) {
        # remove production (pre-sorted) samples in historical data
        #if(y < 2004) odb = odb[which(odb$prodcd_id=="0"),]
        out= NULL
        i = which( odb$fishyr==y )  # use fishing year and not year of the actual sample
        j = intersect (r, i)
        breaks = seq(44, 184, by=4)
        soft = which(odb$durometer<68)
        cc1 = intersect( which( odb$shell==1 ), j )
        cc2 = intersect( which( odb$shell==2 ), j )
        cc3 = intersect( which( odb$shell==3 ), j )
        cc4 = intersect( which( odb$shell==4 ), j )
        cc5 = intersect( which( odb$shell==5 ), j )
        histcc1= hist(odb$cw[cc1], breaks=breaks, freq=F, plot=F)
        histcc2= hist(odb$cw[cc2], breaks=breaks, freq=F, plot=F)
        histcc3= hist(odb$cw[cc3], breaks=breaks, freq=F, plot=F)
        histcc4= hist(odb$cw[cc4], breaks=breaks, freq=F, plot=F)
        histcc5= hist(odb$cw[cc5], breaks=breaks, freq=F, plot=F)
        toplot= rbind(histcc1$counts, histcc2$counts, histcc3$counts, histcc4$counts, histcc5$counts)
        ntot = sum( toplot)
        rowtot = rowSums( toplot )
        ncc = round( rowtot / ntot * 100, 1)
        legend.text = c( paste("CC5 - ",ncc[5],"%", " (", rowtot[5], ")", sep=""), 
                         paste("CC4 - ",ncc[4],"%", " (", rowtot[4], ")",  sep=""), 
                         paste("CC3 - ",ncc[3],"%", " (", rowtot[3], ")",  sep=""), 
                         paste("CC2 - ",ncc[2],"%", " (", rowtot[2], ")",  sep=""), 
                         paste("CC1 - ",ncc[1],"%", " (", rowtot[1], ")",  sep="") )
        maintitle = switch(reg,
          cfanorth = paste("Size frequency distribution in N-ENS --", y), 
          cfasouth = paste("Size frequency distribution in S-ENS --", y),
          cfa4x = paste("Size frequency distribution in 4X --", y)
        )

        dir.create( outdir, recursive=T, showWarnings=F  )
        fn = file.path( outdir, paste("size.freq", reg, y, ".png", sep="") )
      #  Cairo( file=fn, type="png", bg="white",  units="in", width=6, height=4, pointsize=50, dpi=300 )
        png( file=fn,units='in', width=15,height=12,pointsize=18, res=300,type='cairo')
 
           barplot( toplot[c(5:1),], space=0, names.arg=breaks[-1], lwd=8, 
             main=maintitle, legend.text=legend.text, xlab="Carapace width (mm)", ylab="Number of crab",
             args.legend=list(x = "topright", cex=0.8) )
           abline( v=12, lty="dashed", lwd=6)  
        dev.off()

        cmd( "convert -trim -frame 10x10 -mattecolor white ", fn, fn )
    
        print( "-------------------------------")
        print( paste( reg, y, ": N total =", ntot) )
        print( toplot )
    }  }
    return("Done")  
  }



