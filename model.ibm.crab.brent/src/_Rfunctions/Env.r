setConstructorS3("Environment", function(boundary = "", ncrabs = 0, ndays = 0) {
	extend(Object(), "Environment",
		.sch = Schedule(0, ""),
		.boundary = boundary, 
		.ncrabs = ncrabs,
		.crablis = list(),
		.lisplot = ma2,
		.ndays = ndays
	)
})
setMethodS3("init", "Environment", function(this) {
	
	for(i in 1:this$.ncrabs){
	    lo <- runif(1, min(bound[,1]), max(bound[,1]))
		la <-runif(1, min(bound[,2]), max(bound[,2]))
		h <- runif(1, 0 , 361)
		
	    a = cbind(c(lo), c(la))
		this$.lisplot = rbind(this$.lisplot, a)
		
		this$.crablis[[length(this$.crablis)+1]] <- SnowCrab(sc = Schedule(), pos = GeoPos(lo, la), hding = h, vel = 2.2) ;  
	}
	plot(this$.lisplot)
	cont = 0  # Eventually may intrupt the simulation, not currently used
	steps = 0
	ten = 0
	while(cont == 0 && steps < this$.ndays){
	    print(steps)
		steps = steps + 1
		ten = ten+1
	    if(ten>10) ten = 1
		#for each crab add a move commad to their event list and 
		for(i in 1:this$.ncrabs) this$.crablis[i][[1]]$.sc$addevent("move")
		this$.sch$addevent("visitcrabs")
		
		##ADD MORE COMMANDS TO VARIOUS AGENTS HERE
		
		#Continue looping untill all tasks are completed for this day, agents may eventually add
		#their own commands to the environment schedule
		while( this$.sch$getevent() != ""){
			
			if(this$.sch$getevent() == "visitcrabs"){
		
				for(i in 1:this$.ncrabs){ 
					this$.crablis[i][[1]]$dotasks()
					a = cbind(c(this$.crablis[i][[1]]$getpos()[1]), c(this$.crablis[i][[1]]$getpos()[2]))
					
					if(ten == 10) this$.lisplot = rbind(this$.lisplot, a)
			   }
			}
		    ##ADD MORE COMMAND MATCHES+OPERATIONS HERE
		
		this$.sch$removeevent()
		}
    
	if(ten == 10) plot(this$.lisplot)
	}
	plot(this$.lisplot)

})