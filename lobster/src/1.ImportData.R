
#### Import lobster data from various databases

    
loadfunctions( "lobster", functionname="initialise.local.environment.r") 

##### lumped function lobster.db

        # run in windows emvironment
        lobster.db(DS="complete.redo")


	# load .RData objects

        lobster.db( DS="logs")		# Inshore logs summary documents
        lobster.db( DS="logs41")	# Offshore logs monitoring documents
        lobster.db( DS="atSea")		# at Sea sampling from materialized view
        lobster.db( DS="cris")		# CRIS database
        lobster.db( DS="port")		# Port Sampling
        lobster.db( DS="vlog")		# Voluntary logs
        lobster.db( DS="fsrs")		# FSRS recruitment traps
        lobster.db( DS="scallop")	# scallop survey bycatch
        lobster.db( DS="survey")	# ITLS Lobster Survey

#### Data Processing

#### Landings
        
       # Recent Landings Update
        Landings<-read.csv(file.path( project.datadirectory("lobster"), "data","inputs","AnnualandSeasonalLandingsLFA27-38.LFS2015.csv"))

        lfas=c(27,28,29,30,31.1,31.2,32,33,34)
        Landings$YEAR<-as.numeric(substr(Landings$YEAR,1,4))
        Landat<-merge(subset(Landings,TYPE=="Annual"&YEAR>1998,c("YEAR","LFA27", "LFA28", "LFA29", "LFA30", "LFA31A", "LFA31B", "LFA32")),
        subset(Landings,TYPE=="Seasonal"&YEAR>1998,c("YEAR","LFA33", "LFA34")))
        TotalLandings<-with(Landat,data.frame(LFA=sort(rep(lfas,nrow(Landat))),SYEAR=rep(YEAR,length(lfas)),C=c(LFA27, LFA28, LFA29, LFA30, LFA31A, LFA31B, LFA32, LFA33, LFA34)))

  
        write.csv(TotalLandings,file.path( project.datadirectory("lobster"), "data","products","TotalLandings.csv"))







#### FSRS recruitment traps only

recruitment.trap.db('raw.redo')
