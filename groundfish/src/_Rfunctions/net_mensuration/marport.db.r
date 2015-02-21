

marport.db = function( DS, p, YRS=NULL ){
    
  if (!file.exists( p$marport.dir )) {
    mk.Dir = readline("Directory not found, shall we create it? Yes/No (Default No) : ")
    if ( mk.Dir =="Yes" ) {
      dir.create( p$marport.dir, recursive=TRUE) 
      print( paste("The directory -- ",  p$marport.dir, " -- has been created") )
    } else {
      warning( paste("Directory: ", p$marport.dir, " was not found." )) 
    }
  }

 
  if(DS %in% c("marport", "marport.redo"))  {
    
    if (is.null (YRS) ) YRS = p$netmensuration.years 
    
    basedata=NULL
    dir.create( file.path( p$marport.dir, "basedata"), recursive=TRUE, showWarnings=FALSE ) 
      
    if(DS == "basedata"){
      out = NULL
      for ( YR in YRS ) {
        basedata=NULL
        fn = file.path( p$marport,  "basedata",  paste( "marport", "basedata", YR, "rdata", sep="." ))
        if ( file.exists(fn)) {
          load(fn)
          out = rbind( out, basedata )
        }
      }
      return(out)
    }

       
    for ( YR in YRS ) {
      basedata = NULL
      fn = file.path( p$marport,  "basedata",  paste( "marport", "basedata", YR, "rdata", sep="." ))
  
      rawdata.dir = file.path( p$marport, "marport.logs", YR )
      
      # configs
      # p$marport.dir = file.path("C:", "Users", "MundenJ", "Desktop", "Marport")
      filelist4 = list.files(path=rawdata.dir, pattern="^config.*.ini$", full.names=T, recursive=TRUE, ignore.case=TRUE)
      cfg = data.frame( configroot = dirname( filelist4 ), fn=filelist4, stringsAsFactors=FALSE )
      
      filelist1 = list.files(path=rawdata.dir, pattern=".log$", full.names=T, recursive=TRUE, ignore.case=TRUE)
      filelist2 = list.files(path=rawdata.dir, pattern=".gps$", full.names=T, recursive=TRUE, ignore.case=TRUE)
      filelist3 = list.files(path=rawdata.dir, pattern=".sgp$", full.names=T, recursive=TRUE, ignore.case=TRUE)
     
      filelist1 = gsub( ".log$", "", filelist1)
      filelist2 = gsub( ".gps$", "", filelist2)
      filelist3 = gsub( ".sgp$", "", filelist3)
      
      fileroots = unique( c( filelist1, filelist2, filelist3 ) )
      filelist = data.frame( fileroots=fileroots, configroot = dirname( fileroots ), stringsAsFactors=FALSE )
      filelist = merge( filelist, cfg, by="configroot", all.x=TRUE, all.y=FALSE )
      
      no.files = nrow(filelist)
      for ( ii in 1:no.files ) {
        fl = filelist$fileroots[ii]
        sensorconfig = filelist$fn[ii]
        print(fl)
        j = load.marport.rawdata( fl, sensorconfig )  # variable naming conventions in the past
        if (is.null(j)) next()
        j$rootname=fl
        basedata = rbind( basedata, j)
      }
      
      # Include mission as a variable (also trip and year)
      g=substring(basedata$rootname,54,63)      
      basedata$mission = paste(g, basedata$set, sep=".")
      basedata$year = substring(basedata$mission, 4,7)

    print ( "Check this section with Jenna" ) 
    {
    
    # remove US trawls
    i = grep("us", basedata$mission)
    basedata = basedata[-i,]
    n = grep("US", basedata$mission)
    basedata = basedata[-n,]
    
    # Produce standard format for mission to enable comparision with Scanmar
    basedata$mission = gsub("W2A0", "", basedata$mission)
    basedata$mission = gsub("001W2", "", basedata$mission)
    basedata$mission = gsub("WIIA0", "", basedata$mission)
    basedata$mission = gsub("W2a", "", basedata$mission)
    basedata$mission = gsub("W2", "", basedata$mission)
    basedata$mission = gsub("w2", "", basedata$mission)
    
    
    # Remove extra zeros
    uni = strsplit(basedata$mission,".", fixed = TRUE)
    uni1 = as.data.frame(matrix(unlist(uni), ncol = 2, byrow = TRUE))
    basedata$mission = paste(uni1[,1], as.numeric(uni1[,2]),sep=".")
    }


      # rename mission to id, so comparisons with Scanmar are easier
      basedata$id = basedata$mission
      
      # Make year numeric and as trip as a variable
      basedata$year=as.numeric(basedata$year)
      basedata$trip = substring(basedata$mission, 8,10)
      basedata$trip=as.numeric(basedata$trip)
      
      save(basedata, file=fn, compress= TRUE)
      print( fn)
    }

    return (YRS )
  }


  # -------------------------------


  if(DS %in% c("marport.gated", "marport.gated.redo"))  {
    nm=NULL
    fn=file.path( p$marport.dir, paste( "marport.gated", "rdata", sep="." ))
    if(DS == "marport.gated"){
      if (file.exists(fn)) load(fn)
      return(nm)
    }
      nm = marport.db( DS="marport",  p=p ) # QA/QC of data
        
      nm$doorspread = filter.nets("doorspread.range", nm$doorspread)
      nm$wingspread = filter.nets("wingspread.range", nm$wingspread)
      nm$clearance = filter.nets("clearance.range", nm$clearance)
      nm$opening.scanmar = filter.nets("opening.range", nm$opening.scanmar)
      nm$depth = filter.nets("depth.range", nm$depth)
    
    save(nm, file=fn, compress=TRUE)
    return(fn) 
  }


}


