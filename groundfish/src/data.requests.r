


> My name in Ben Laurel and I'm a research fisheries biologist with the
> NOAA/NMFS Alaska Fisheries Science Center (AFSC).  Mike Litzow
> (NOAA/NMFS-AFSC) and I are working on a comparison of ecosystem regime
> shifts in North Pacific and North Atlantic waters.  We think
> combining the NW Atlantic data with the Alaskan data would be a great
> way to unify study of sudden transitions in marine ecosystems.  Our
> analysis from Alaskan waters is complete and we've begun a similar
> examination on the Scotian Shelf using ECNASAP data from 1971-1994.
> However, we were hoping to complete the analysis with data from
> 1995-current. We are restricting our analysis to the Scotian Shelf,
> specifically to the area covered in the Choi et al. 61:505-510 CJFAS
> paper on transition between alternate stable states
> - I'm attaching that paper in case you don't have a copy handy.  
> 
>  
> 
> My question is whether post-1994 trawl data (numbers and weights -
> CPUE for each spp. in each set) from the Scotian Shelf could be made
> available to us for our study?  I appreciate any help or guidance you
> could provide in this matter.
> 
>  
> 
> Thanks for your time,
> 
>  
> 
> Benjamin J. Laurel- PhD
> Fisheries Behavioral Ecology Program
> Alaska Fisheries Science Center
> NOAA Fisheries, Hatfield Marine Science Center Newport OR 97365 USA
> phone: (541) 867-0197
> fax: (541) 867-0136
> email: Ben.Laurel@noaa.gov 


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

