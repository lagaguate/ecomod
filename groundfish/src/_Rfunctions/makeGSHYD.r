makeGSHYD <- function(x) {
	#x comes from compileODF.r
	loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
	loadfunctions( "bathymetry")
	gshyd <- x[,c('mission','setno','temperature_ITS-68,degC_-99', 'salinity_PSU_-99' ,'oxygen_by_volume_ml/l_-99','depth')]
	gshyd$gear <- 2
	gshyd$eind <- NA
	gshyd$tagno <- NA
	gshyd$bid  <- NA
	names(gshyd)[1:6] <- c('mission','setno','temp','sal','oxyml','sdepth')
	idx <- sapply(split(1:nrow(gshyd), gshyd$setno), function(x) { x[which.max(gshyd[x,"sdepth"])]})
	gshyd$bid[idx] <- 'B'
	gshyd$year <- substring(gshyd$mission,4,7)
	return(gshyd)
}