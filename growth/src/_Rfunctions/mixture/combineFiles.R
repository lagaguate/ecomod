path <- 'F:/Indicators'

combineIndicators <-	function(path) {
		fp 		<- file.path(path,"output","Estimated Indicators")
 		fi 		<- dir(fp,full.names=T,recursive=T)
 		o 		<- matrix(NA,ncol=7,nrow=length(fi))
 	
 	for(i in 1:length(fi)) {
 		o[i,] 	<- strsplit(fi[i],"/")[[1]]
 		}
 		
 		if(any(duplicated(o[,6:7]))) {
 			k 	<- bothDuplicated(o[,6:7])
 			cat('You have duplicated files:\n remove before proceeding\n')
 			print(fi[k])
 			stop()
 		}
 	
 		#setup indices
 		qa 		<- which(o[,6]=='Qadj')
 		nqa 	<- which(o[,6]=='NonQadj')
 		la 		<- which(o[,6]=='Land')
 		ew 		<- grep('esswss',o[,7])
 		naf 	<- grep('nafo',o[,7])
 		stra 	<- grep('strat',o[,7])
 		shel	<- grep('shelf',o[,7])
 		
 		ewqa 	<- fi[ew[which(ew %in% qa)]]
 		nafqa   <- fi[naf[which(naf %in% qa)]]
 		straqa  <- fi[stra[which(stra %in% qa)]]
 		shelqa  <- fi[shel[which(shel %in% qa)]]
 		ewnqa 	<- fi[ew[which(ew %in% nqa)]]    
 		nafnqa  <- fi[naf[which(naf %in% nqa)]]      
 		stranqa <- fi[stra[which(stra %in% nqa)]]    
 		shelnqa <- fi[shel[which(shel %in% nqa)]]    
 		ewqla 	<- fi[ew[which(ew %in% la)]]    
 		nafla   <- fi[naf[which(naf %in% la)]]      
 		shella  <- fi[shel[which(shel %in% la)]]
 		
 		csv.ewqa    
 		csv.nafqa   
 		csv.straqa  
        csv.shelqa  
        csv.ewnqa   
        csv.nafnqa  
        csv.stranqa 
        csv.shelnqa 
        csv.ewqla   
        csv.nafla   
        csv.shella  
        browser()
        
        }
        
    combineFiles <- function(x) {
     	for(i in 1:length(x)) {
     	 ou <- read.csv(x[i],header=T)
     	 ou <- ou[,-1]
     	 if(any(names(ou) =='NAMES')) names(out)[which(names(ou)=='NAMES')] <- "ID"
     	 ih <- which(!names(ou) %in% c('ID','YEAR'))
     	 if(grepl('nafo',x[i]))    nn <-  sub('.csv','',sub('nafo','',x[i]))
     	 if(grepl('strat',x[i]))   nn <-  sub('.csv','',sub('strata','',x[i]))
     	 if(grepl('esswss',x[i]))     nn <-  sub('.csv','',sub('esswss','',x[i]))
     	 if(grepl('shelf',x[i]))   nn <-  sub('.csv','',sub('shelf','',x[i]))
     	 
     	 nn <- strsplit(nn,"/")[[1]][7]
     	 names(ou)[ih] <- nn
     	 
     	 if(i==1) {
     	 	out <- ou
     	 	} else {
     	 	out <- merge(out,ou,by=c('YEAR','ID'),all=T)
     	 	}
     	 	}
     	 	return(out)
     	 }
    
    
    