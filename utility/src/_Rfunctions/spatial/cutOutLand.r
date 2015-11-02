cutOutLand<-function(Polys,coast,mapRes="UR"){
#// PBS mapping utility to remove the land from management area polygons, include coastline data or defaults to "ecomod_data/lobster/data/maps/gshhs/shorelineUR.csv"
	if(missing(coast))coast<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","gshhs",paste0("shoreline",mapRes,".csv")))
	attr(coast,"projection")<-"LL"
	coast<-clipPolys(coast,xlim=range(Polys$X),ylim=range(Polys$Y))
	if(!is.null(coast)){
		coast<-idHoles(coast)
		Polys.out<-joinPolys(Polys,subset(coast,Hole==F),"DIFF")
	}
	else Polys.out<-Polys
}						
