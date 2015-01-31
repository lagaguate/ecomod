PortSamples.make.kml<-function(df_in){
  #pull out stuff passed with dataframe
  df        <-as.data.frame(df_in[[1]]) 
  vertexFields<-            df_in[[2]]
  the.SQL   <-              df_in[[3]]
  days      <-              df_in[[4]]
  vrn       <-              df_in[[5]]
  titlebit  <-              df_in[[6]]
  filename  <-              df_in[[7]]
  title     <-              df_in[[8]]
  workdir   <-              df_in[[9]]
  
  library(R.utils)
  
  title<-paste(title," ",titlebit,sep="")
  rightnow<-Sys.time()
  timestamp<-format(rightnow, "%Y%m%d_%H%M")
  descDate<-format(rightnow, "%a %b %d %Y, at %I:%M%p")
  theFile<-paste(filename,"_",timestamp,sep="" )
  kmlName<-paste(theFile, ".kml",sep="" )
  
  #identify the template location and prepare output file 
  fn.kml = file.path( workdir, "templates","PortSamples_template.kml" )
  tmpfile =  file.path( workdir, kmlName) 
  kmzfolder =  file.path( workdir, "output")  
  con = file( tmpfile, open="a")
  kml = readLines( fn.kml)
  kml.toSupportingData = grep (":__SupportingData",kml)
  kml.toData = grep (":__Data" , kml )
  kml.toStyle = grep (":__Style" , kml )
  kml.style = kml[(kml.toStyle+1) : (kml.toSupportingData-1)]
  kml.tail = kml[ (kml.toData+1) : length( kml ) ]
  kml.head = kml[  1 : kml.toStyle-1 ]
  kml.head = sub( ":__HeaderName", title, kml.head )
  metadata = paste("Generated: ",descDate, "<br>SQL used:<br> ",the.SQL,"<br><br>
                    Please contact <a href='mailto:Mike.McMahon@dfo-mpo.gc.ca?Subject=PED kml file'>Mike McMahon</a> (Population Ecology Division) for assistance, or to unsubscribe from this (or other) reports.",sep="")
  kml.head = sub( ":__HeaderDescription", metadata, kml.head )
  

  if (!NROW(df)<1) { 
    placemarks=NULL 

    cat(paste("Got the data, generating '",title,"'...\n",sep=""))

  folderLevel1<-"VESSEL_NAME"
    
  df<-df[order(df[, folderLevel1]),]
  level1types<-unique(df[, folderLevel1]) 
  for (a in 1:NROW(level1types)){
  folderName<-level1types[a]
  placemarks=c(placemarks, paste("<Folder><name><![CDATA[",folderName,"]]></name>
  <open>1</open>
  <visibility>1</visibility>
  <styleUrl>#checkHideChildren</styleUrl>",sep=""))
      level1df<-subset(df, df[,folderLevel1]==level1types[a])
      placemarks=c(placemarks, PortSamples.make.placemark(level1df,vertexFields))
      placemarks=c(placemarks, "</Folder>")
      level1df=NULL
    }

  }else{
    cat(paste("No data was found for '",title,"'...\n",sep=""))
    placemarks<-paste("<Folder><name><![CDATA[No data found!]]></name><open>0</open><visibility>0</visibility><styleUrl>#checkHideChildren</styleUrl></Folder>",sep="")
  }
    
  writeLines(kml.head,  con )
  writeLines(kml.style, con )
  #writeLines(background.kml, con )
  writeLines(placemarks, con )
  writeLines( kml.tail, con )
  close(con)
  kmz<-file.path(kmzfolder,paste(theFile,".kmz",sep=""))
  kmzfile<-zip(kmz,
		c(file.path( workdir,kmlName),
		paste(workdir,"/customIcons/arrow.png",sep="")))
  cat("Cleaning up.")
  file.remove( tmpfile) 
  cat(kmz)
}