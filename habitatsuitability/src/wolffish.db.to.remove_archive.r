
  wolffish.db = function( DS="", p=NULL ) {
    
    wdir = file.path( project.directory("habitatsuitability"), "analysis", p$speciesofinterest, "data" )

    if (DS %in% c("complete.redo", "complete") ) {
      
      fn = file.path( wdir, paste( p$speciesofinterest, "complete.rdata", sep="." ) )
      
      if (DS=="complete") {
        W = NULL
        if (file.exists ( fn)) load(fn)
        return (W)
      }
    

      #----------------------------
      # 1. halibut surveys be used only to indicate presence (not absence) .. makes sense due to usage of hooks
      # 2. ITQ is potentially useless -- it also hook based?
      # 3. Landings suggest only presence as well. 
      #    Absence is not known for sure as mostly positive valued landings are recorded

  
      # ol = wolffish.db( DS="landings.worksheet", p=p) 
      os = wolffish.db( DS="surveys.worksheet", p=p )

      # use bio.db
      bio = bio.db("set", p)

      ex = c( "trip", "set", "survey", "lat", "lon", "chron", "abundance", "metric", "z", "sal", "t", "sa" ) 
      W = rbind( bio[, ex] , os[, ex] )
      
      surveys.with.real.zeros = c( "snowcrab", "4VWSentinel", "groundfish", "ITQ", "snowcrab", "USFall", "Usspring")
      P = which( W$abundance > 0 ) # presence
      A = which( W$abundance == 0  & W$survey %in% surveys.with.real.zeros ) # absence

      W$presence = NA
      W$presence[ P ] = 1
      W$presence[ A ] = 0

      rm( P, A ) 


      W = habitat.data.prep (W, p)  # filter and interpolate covariates
      W = habitat.abundance.quantiles( W, p)   
     
      # when quantiles are lower than a given threshold in surveys with informative relative abundance metrics, consider it absent: ~5% or 1%
      PP = which(W$q < p$habitat.threshold.quantile & W$survey %in% surveys.with.real.zeros )
      if (length(PP)>0) W$presence[ PP ] = 0  # about 132 cases

      W = W[ which(is.finite(W$presence + W$abundance )),]


      W$tmean[ which( W$tmean> 12 ) ] = 12
      W$tamp[ which( W$tamp> 15 ) ] = 15
      W$massTot[ which( W$massTot > 200 ) ] = 200
      W$Npred[ which( W$Npred > 120 ) ] = 120
      W$Npred[ which( W$Npred < 10 ) ] = 10
      W$smr[ which( W$smr > 0.0065 ) ] = 0.0065
  

      debug = F
      if (debug) {

        surveys = as.character( unique( W$survey ) )
        length( surveys )

        # there are eight surveys
        graphics.off() # close all graphics
        x11() # start a new graphic  
        layout ( matrix( c(1:8), nrow=2, ncol=4, byrow=T ) )
        for ( i in 1:length(surveys) ) {
          hist( W$ntot[ which(W$survey==surveys[i] ) ], main=surveys[i], xlab="" )  # there is a data error with values > 40 ?
        } 
          
        # exploratory analysis/figures
        hist( W$mass.mean )  # mean weight of wolf fish
        median ( W$mass.mean, na.rm=T )
        coplot( lat ~ lon | survey, data = W )

        # simple catch rate distributions to get a sense of scale
        unique( W$survey )  # there are four surveys
        graphics.off() # close all graphics
        x11() # start a new graphic  
        layout ( matrix( c(1,2,3,4), nrow=2, ncol=2, byrow=T ) )

        hist( W$totwgt[ which(W$survey=="halibutindex" & W$totwgt < 200  ) ], main="Halibut index", xlab="" )  # there is a data error with values > 40 ?
        hist( W$totwgt[ which(W$survey=="Halibutfixed" & W$totwgt < 20 ) ], main="Halibut Fixed index", xlab="" )  # there is a data error with values > 40 ?
        hist( W$totwgt[ which(W$survey=="landings"& W$totwgt < 100  ) ], main="Landings", xlab="" )  # there is a data error with values > 100 ?, units?
        hist( W$totwgt[ which(W$survey=="ITQ"& W$totwgt < 0.4  ) ], main="ITQ", xlab="" )  # there is no data with values > 0 ?
       
      }

      save( W, file=fn, compress=T )
      return(fn)
  
    }
    
  # ----------------------------    

    
    if ( DS=="landings.worksheet" ) {
     
      require( gdata ) # to access read.xls()
      
      wdir = file.path( project.directory("habitatsuitability"), "analysis", p$speciesofinterest, "data" )


      # landings from MARFIS -- weight per tow, etc: source Jim Simon
      fn = file.path( wdir, "wolffish.xls" )
      ol = read.xls( fn, sheet=3 )
      names(ol) = c( "yr", "survey", "month", "lat", "lon", "abundance" )   
      ol$survey = as.character ( ol$survey )
      ol$chron = chron( dates.=paste("15", ol$month, ol$yr, sep="/"), times.="12:00:00", 
          format=c(dates="d/m/y",  times = "h:m:s"), 
          out.format=c( "year-m-d", "h:m:s") )
      ol$metric="weight"
      ol$trip = "NA"
      ol$set="NA"
      ol$z = NA
      ol$t = NA
      ol$sa = 1
      ol$sal = NA

      return ( ol )
    }
    
    if ( DS=="surveys.worksheet" ) {
  
      require( gdata ) # to access read.xls()
     wdir = file.path( project.directory("habitatsuitability"), "analysis", p$speciesofinterest, "data" )


      # rv surveys and 4VsW sentinel surveys, etc: source Jim Simon
      fn = file.path( wdir, "wolffish.xls" )
      os = read.xls( fn, sheet=2 )
      names(os) = c("yr", "survey", "month", "lat", "lon", "abundance" ) 
      os$survey = as.character ( os$survey )
      os$chron = chron( dates.=paste("15", os$month, os$yr, sep="/"), times.="12:00:00", 
          format=c(dates="d/m/y",  times = "h:m:s"), 
          out.format=c( "year-m-d", "h:m:s") )
      os$metric="number"
      os$trip="NA"
      os$set="NA"
      os$z = NA
      os$t = NA
      os$sa = 1
      os$sal = NA
      # drop groundfish surveys as they are captured later
      surveyids = c("Fall78-84", "Spring79", "Summer", "4VWCOD", "Georges,Cdn" , "RV")
      os = os[ -which( os$survey %in% surveyids), ]
      return (os)
    }


    if ( DS=="snowcrab" ) {      ############ no biomass only counts
      #  snow crab trawl-based data tables
    
      loadfunctions( "snowcrab", functionname="initialise.local.environment.r"  ) 
      loadfunctions( "taxonomy" )

      # loaded by initialize.local.environment above
      # p = parameter.list.snowcrab ( current.assessment.year=current.assessment.year, set="default")
     
      tx = taxonomy.db( "complete" )
      txi = tx[ grep( "ATLANTIC WOLFFISH",  tx$name.common, ignore.case=T ) ,]

      wf = snowcrab.db ( DS="cat.georeferenced" ) 
      wf = wf[ which( wf$spec %in% txi$spec ) ,]
      wf = wf[ , c("trip", "set", "totno" ) ]

      sc = snowcrab.db ( DS="set.complete" ) 
      sc = sc[ sc$yr >=2004 ,]
      sc = merge( sc, wf, by=c("trip", "set"), all.x=T, all.y=F )
      names(sc)[which( names(sc)=="totno")] = "abundance"  # <<<<<<<<<<<<< counts only
      sc$survey = "snowcrab"
      sc$metric = "number"
      sc$sal = NA
      sc$abundance[ which( !is.finite( sc$abundance ) ) ] = 0  # trawl-based NA==0
      sc$z = exp( sc$z )


      # data dump for Jim Simon
      # ex = c("lat", "lon", "abundance" )
      # write.table( sc[,ex], file=file.path( project.wolffish, "wolffish_snowcrabsurvey.csv"), sep="," )
      return (sc )
    }     


  }

 

