
setConstructorS3("Schedule", function(steptime=0, events="") {
	extend(Object(), "Schedule",
		.steptime = steptime,
		.events = events
	)
})
setMethodS3("addevent", "Schedule", function(this, newevent) {
	if(this$.events[1] == "") this$.events <- c(newevent)
	else this$.events <- c(this$.events, newevent)
})
setMethodS3("removeevent", "Schedule", function(this) {
	if(length(this$.events) == 1) this$.events <- ""
	else this$.events <- this$.events[2:length(this$.events)] ;
})
setMethodS3("getevent", "Schedule", function(this) {
	this$.events[1];
})
