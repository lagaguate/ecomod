
setConstructorS3("SnowCrab", function(sc = "", pos = "", hding=0, vel=0) {
	extend(Object(), "SnowCrab",
		.sc = sc, 
		.pos = pos,
		.hding = hding,
		.vel = vel
	)
})
setMethodS3("getpos", "SnowCrab", function(this) {
	this$.pos$getpos();
})
setMethodS3("gethding", "SnowCrab", function(this) {
	this$.hding;
})
setMethodS3("getvel", "SnowCrab", function(this) {
	this$.vel;
})
setMethodS3("setlon", "SnowCrab", function(this, newlon) {
	this$.pos[1] <- newlon;
})
setMethodS3("setlat", "SnowCrab", function(this, newlat) {
	this$.pos[2] <- newlat;
})
setMethodS3("sethding", "SnowCrab", function(this, newhding) {
	this$.hding <- newhding;
})
setMethodS3("setvel", "SnowCrab", function(this, newvel) {
	this$.vel <- newvel;
})
setMethodS3("move", "SnowCrab", function(this) {
	dis <- rnorm(1, 250, 100)
	hd <- rnorm(1, this$.hding, 30)

	this$.hding <- hd
    if(hd > 360) hd <- hd - 360
    if(hd < 0 ) hd	<- 360 + hd
	newpt <- destPoint(this$getpos(), hd, dis, r = 6378137)
	
	this$.pos$setpos(newpt[1], newpt[2])
})
setMethodS3("dotasks", "SnowCrab", function(this) {
	while(this$.sc$getevent() != ""){
		if(this$.sc$getevent() == "move"){
		   this$move()
		}
	this$.sc$removeevent()
	}
})
