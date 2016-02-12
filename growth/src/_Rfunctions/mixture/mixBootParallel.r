

						

  
  mixBootParallel = function( ip=NULL, p=NULL, ... ) {

    # must have ip as the first parameter if using "parallel" or "snow"
ip=NULL
    if (is.null(ip)) ip = p$good.ps 
    if (exists( "init.files", p)) for(i in  p$init.files ) source(i) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    
    outdir = p$outdir
    det = p$Det
    gg  = p$gg
    groups = p$groups	
    
    for( iip in ip ) {							
				dg <- which(gg==iip)	
				x	<- reshapeCW(det[which(det$PID %in% groups[[dg]][,'PID']) ,]) 
				
			if(length(x[[1]])>0 & !is.vector(x[[1]])) {
				yrs 	<- x[[2]]
				x 		<- x[[1]]
				lw <- apply(x,2,function(x) length(na.omit(x)))
			
			if(any(lw<75)) {
					po <- which(lw<75)	
					x <- x[,-po]
					yrs <- yrs[-po]
				}

				xi 		<- identifyModes(x,span=5)
				vi		<- identifyVariancesLambdas(x,xi,span=5)
				li		<- vi[[2]]
				vi		<- vi[[1]]
				out 	<- annualMixBoot(x=x,init.mode=xi,ni=5000,mode.thresh=6, init.lam=li,init.var=vi)
				fname 	<- paste("PID",iip,"-",0,".rdata",sep="")
				browser()
			gc(reset=T)
			save(out,file=file.path(outdir,"R",fname))
			   		}
			 	}
			}
    
    


