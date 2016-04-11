loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
	
loc = file.path( project.datadirectory("groundfish"), "data", "2006")
require(chron)
        
    load( file.path(loc,"gscat.rdata") )
    load( file.path(loc,"gsinf.rdata") )
    set = merge(x=gscat, y=gsinf, by=c("id"), all.x=T, all.y=F, sort=F) 
    rm (gscat, gsinf)     
   
    load( file.path(loc,"gshyd.rdata") )
    set = merge(x=set, y=gshyd, by=c("id"), all.x=T, all.y=F, sort=F) 
    rm (gshyd)

    gstaxa = taxonomy.db( "gstaxa") 
    set = merge(x=set, y=gstaxa, by=c("spec"), all.x=T, all.y=F, sort=F) 
    rm (gstaxa)

    set$chron = as.chron(set$sdate)
    set$sdate = NULL
    set$yr = convert.datecodes(set$chron, "year" )
    set$julian = convert.datecodes(set$chron, "julian")

    vars = c("id", "spec", "namecom", "yr","totwgt","totno", "lon", "lat")
    
    sets.requested = which(set$settype==1 )
    set=set[sets.requested,vars]

  write.table(set, "groundfish.data.laurel.extraction.cvs", sep=";")

