#source("Oceans/oceans.make.placemark.R")
#source("Oceans/oceans.ocmd.areas.R")
#source("Oceans/oceans.export.R")

oceans.make.kml<-function(df_in){
  


  #pull out stuff passed with dataframe
  df        <-as.data.frame(df_in[[1]]) 
  vertexFields<-            df_in[[2]]
  the.SQL   <-              df_in[[3]]
  days      <-              df_in[[4]]
  startDate <-              df_in[[5]]
  endDate   <-              df_in[[6]]
  vrn       <-              df_in[[7]]
  filename  <-              df_in[[8]]
  dwindow   <-              df_in[[9]]
  workdir   <-              df_in[[10]]
  tmpdir    <-              df_in[[11]]
  savelocation    <-        df_in[[12]]
  if (is.null(filename)){
    filename<-"All"
  }else if (filename == "VazellaEmerald"){
    filename<-"Vazella"
  }
  background.kml<-oceans.ocmd.areas(filename,dwindow)
  
  if (!is.null(startDate) && !is.null(endDate)){
    title= paste("Activity in ", filename, " between ",startDate," and ",endDate,sep="")
  }else if (!is.null(days) && (!is.null(startDate) || !is.null(endDate))){
    thedate<-c(startDate,endDate)
    title= paste(days," days of activity prior to ", thedate," in ",filename,sep="")
  }else if (!is.null(days)){
    title= paste("Last ",days," days of activity in ",filename,sep="")
  }else{
    title= "No title"
  }
  if(!is.null(vrn)){
    title<-paste(title, " for VRN: ",vrn,sep="")
  }
  

  rightnow<-Sys.time()
  timestamp<-format(rightnow, "%Y%m%d_%H%M")
  descDate<-format(rightnow, "%a %b %d %Y, at %I:%M%p")
  theFile<-paste(filename,"_",timestamp,sep="" )
  kmlName<-paste(theFile, ".kml",sep="" )
  xlsName<-paste(theFile, ".xls",sep="" )
  #cat("Writing the raw data to an Excel file...\n")
  #xlsFile<-generateExcel(c(df_in,descDate, xlsName,workdir,tmpdir,savelocation))
  #identify the template location and prepare output file 

  fn.kml =file.path( workdir,"templates","Oceans_template.kml")
  tmpfile =  file.path( tmpdir, kmlName) 
  #kmzfolder =  file.path( workdir, "Oceans","output")  
  kmzfolder =  savelocation  
  #print('past')
  con = file( tmpfile, open="a")
  kml = readLines( fn.kml)
  kml.toSupportingData = grep (":__SupportingData",kml)
  kml.toData = grep (":__Data" , kml )
  kml.toStyle = grep (":__Style" , kml )
  kml.style = kml[(kml.toStyle+1) : (kml.toSupportingData-1)]
  kml.tail = kml[ (kml.toData+1) : length( kml ) ]
  kml.head = kml[  1 : kml.toStyle-1 ]
  kml.head = sub( ":__HeaderName", title, kml.head )
  metadata = paste("Generated: ",descDate, "<br><br>
                    Please contact <a href='mailto:",mike.email,"?Subject=PED kml file'>Mike McMahon</a> (Population Ecology Division) for assistance, or to unsubscribe from this (or other) reports.",sep="")
  kml.head = sub( ":__HeaderDescription", metadata, kml.head )

  if (!NROW(df)<1) { 
    placemarks=NULL 

  cat(paste("Got the data, generating kml file  '",title,"'...\n",sep=""))
  #hierarchy
  folderLevel1<-"GEAR"
  folderLevel1a<-"HAIL_OUT_SPECIES_CATEGORY"
  folderLevel2<-"VR_NUMBER" 
  folderLevel3<-"SOURCE"
  
 
  df<-df[order(df[, folderLevel1],df[, folderLevel1a]),]
  level1types<-unique(df[, c(folderLevel1,folderLevel1a)]) 
  level1typeGrp <- do.call(paste, c(level1types[c(folderLevel1,folderLevel1a)], sep = " - "))
    for (a in 1:NROW(level1types)){
      placemarks=c(placemarks, paste("<Folder><name><![CDATA[",level1typeGrp[a],"]]></name><open>1</open><visibility>0</visibility><styleUrl>#radioFolder</styleUrl>",sep=""))
      level1df<-subset(df, df[,folderLevel1]==level1types[a,1] & df[,folderLevel1a]==level1types[a,2])
###################START BY VESSEL####################  
      #sort vessel names alphabetically
      level1df<-level1df[order(level1df$VESSEL_NAME,level1df$VR_NUMBER),]
      
      level2types<-unique(level1df[, folderLevel2])
      placemarks=c(placemarks, paste("<Folder><name><![CDATA[BY VESSEL]]></name><open>0</open><visibility>0</visibility>",sep=""))
      for (b in 1:NROW(level2types)){       
        level2df<-subset(level1df, level1df[,folderLevel2]==level2types[b])
        folder2<-NULL
        if (folderLevel2=="VR_NUMBER"){
          #default label for folder2 is vessel number - overwritten with name if available
          folder2<-level2df[1,]$VR_NUMBER
          if((is.na(level2df[1,]$VESSEL_NAME)) || (length(level2df[1,]$VESSEL_NAME)<1)|| is.null(level2df[1,]$VESSEL_NAME)|| (level2df[1,]$VESSEL_NAME=='Unknown')){
          }else{
            folder2<-level2df[1,]$VESSEL_NAME
          } 
          #if the 2nd level is SOURCE, we want to label the folder with SOURCE type
        }else if(folderLevel2=="SOURCE"){
          folder2<-level2df[1,]$SOURCE
        }
        placemarks=c(placemarks, paste("<Folder><name><![CDATA[",folder2,"]]></name><visibility>0</visibility>",sep=""))
        level2df<-level2df[order(level2df[,folderLevel3]),]
        level3types<-unique(level2df[,folderLevel3])
        for (c in 1:NROW(level3types)){
          level3df<-subset(level2df, level2df[,folderLevel3]==level3types[c])
          folder3<-NULL
          if (folderLevel3=="VR_NUMBER"){
            folder3<-level3df[1,]$VR_NUMBER
            if((is.na(level3df[1,]$VESSEL_NAME)) || (length(level3df[1,]$VESSEL_NAME)<1)|| is.null(level3df[1,]$VESSEL_NAME)|| (level2df[1,]$VESSEL_NAME=='Unknown')){
            }else{
              folder3<-level3df[1,]$VESSEL_NAME
            } 
          }else if(folderLevel3=="SOURCE"){
            folder3<-level3df[1,]$SOURCE
          }    
          placemarks=c(placemarks, paste("<Folder><name><![CDATA[",folder3,"]]></name><visibility>0</visibility>",sep=""))        
          if(level3df$SOURCE=="VMS"){
            placemarks=c(placemarks, oceans.make.placemark(level3df, vertexFields))
          }else{        
            folder4="Cowabunga"
            prevfolder4="Booyuckasha!"
            for (a in 1:NROW(level3df)){
              folder4a<-level3df[a,]$TRIP_ID
              folder4b<-level3df[a,]$SUBTRIP
              folder4<-paste("Trip:",folder4a," Subtrip:", folder4b,sep="")
              if(folder4!=prevfolder4){
                #need to make folder
                if (prevfolder4!="Booyuckasha!"){
                  #if this is the first time around, don't close the folder
                  placemarks=c(placemarks, "</Folder>")
                }
                placemarks=c(placemarks, paste("<Folder>\n<name><![CDATA[",folder4,"]]></name><visibility>0</visibility>",sep=""))
                placemarks=c(placemarks, oceans.make.placemark(level3df[a,], vertexFields))
              }else{ 
                placemarks=c(placemarks, oceans.make.placemark(level3df[a,], vertexFields))
              }
              prevfolder4<-folder4
            }
            placemarks=c(placemarks, "</Folder>")
            
          }
          placemarks=c(placemarks, "</Folder>")
          level3df<-NULL
        }  
        placemarks=c(placemarks, "</Folder>")
      level2df<-NULL
      }
      placemarks=c(placemarks, "</Folder>")
###################END BY VESSEL######################  
      level2types<-NULL
        
###################START BY SOURCE####################  
      level2types<-unique(level1df[, folderLevel3])
      placemarks=c(placemarks, paste("<Folder><name><![CDATA[BY SOURCE]]></name><open>0</open><visibility>0</visibility>",sep=""))
      for (b in 1:NROW(level2types)){       
        level2df<-subset(level1df, level1df[,folderLevel3]==level2types[b])
        bysource<-level2df
        folder2<-NULL
        if (folderLevel3=="VR_NUMBER"){
          #default label for folder2 is vessel number - overwritten with name if available
          folder2<-level2df[1,]$VR_NUMBER
          if((is.na(level2df[1,]$VESSEL_NAME)) || (length(level2df[1,]$VESSEL_NAME)<1)|| is.null(level2df[1,]$VESSEL_NAME)|| (level2df[1,]$VESSEL_NAME=='Unknown')){
          }else{
            folder2<-level2df[1,]$VESSEL_NAME
          } 
          #if the 2nd level is SOURCE, we want to label the folder with SOURCE type
        }else if(folderLevel3=="SOURCE"){
          folder2<-level2df[1,]$SOURCE
        }
        placemarks=c(placemarks, paste("<Folder><name><![CDATA[",folder2,"]]></name><visibility>0</visibility>",sep=""))
        level2df<-level2df[order(level2df[,folderLevel2]),]
        level3types<-unique(level2df[,folderLevel2])
        for (c in 1:NROW(level3types)){
          level3df<-subset(level2df, level2df[,folderLevel2]==level3types[c])
          folder3<-NULL
          if (folderLevel2=="VR_NUMBER"){
            folder3<-level3df[1,]$VR_NUMBER
            if((is.na(level3df[1,]$VESSEL_NAME)) || (length(level3df[1,]$VESSEL_NAME)<1)|| is.null(level3df[1,]$VESSEL_NAME)|| (level2df[1,]$VESSEL_NAME=='Unknown')){
            }else{
              folder3<-level3df[1,]$VESSEL_NAME
            } 
          }else if(folderLevel2=="SOURCE"){
            folder3<-level3df[1,]$SOURCE
          }
          placemarks=c(placemarks, paste("<Folder><name><![CDATA[",folder3,"]]></name><visibility>0</visibility>",sep=""))
          if(level3df$SOURCE=="VMS"){
            placemarks=c(placemarks, oceans.make.placemark(level3df, vertexFields))
          }else{        
            folder4="Cowabunga"
            prevfolder4="Booyuckasha!"
            for (a in 1:NROW(level3df)){
              folder4a<-level3df[a,]$TRIP_ID
              folder4b<-level3df[a,]$SUBTRIP
              folder4<-paste("Trip:",folder4a," Subtrip:", folder4b,sep="")
              if(folder4!=prevfolder4){
                #need to make folder
                if (prevfolder4!="Booyuckasha!"){
                  #if this is the first time around, don't close the folder
                  placemarks=c(placemarks, "</Folder>")
                }
                placemarks=c(placemarks, paste("<Folder>\n<name><![CDATA[",folder4,"]]></name><visibility>0</visibility>",sep=""))
                placemarks=c(placemarks, oceans.make.placemark(level3df[a,], vertexFields))
              }else{ 
                placemarks=c(placemarks, oceans.make.placemark(level3df[a,], vertexFields))
              }
              prevfolder4<-folder4
            }
            placemarks=c(placemarks, "</Folder>")
            
          }
          placemarks=c(placemarks, "</Folder>")
          level3df<-NULL
        }  
        placemarks=c(placemarks, "</Folder>")
        level2df<-NULL
      }
      
      
      
      placemarks=c(placemarks, "</Folder>")
###################END BY SOURCE######################  
      placemarks=c(placemarks, "</Folder>")
      level1df=NULL
      level2types=NULL
    }
  }else{
    cat(paste("No data was found for '",title,"'...\n",sep=""))
    placemarks<-paste("<Folder><name><![CDATA[No data found!]]></name><open>0</open><visibility>0</visibility><styleUrl>#checkHideChildren</styleUrl></Folder>",sep="")
  }
  writeLines(kml.head,  con )
  writeLines(kml.style, con )
  writeLines(background.kml, con )
  writeLines(placemarks, con )
  writeLines( kml.tail, con )
  close(con)
  # cat("Shrinking your file...\n")
  setwd(tmpdir)
  library(R.utils)
  kmz<-file.path(kmzfolder,paste(theFile,".kmz",sep=""))
  kmzfile<-zip(kmz,c(paste(theFile,".kml",sep=""),"customIcons"))
  #WRITE THE FILE TO web folder
  download<-file.copy(kmz, paste(savelocation,'/',theFile,'.kmz',sep=""))
  downloadLoc<-paste(savelocation,'/',theFile,'.kmz',sep="")
  #Capture stuff relevant to file (for emailing/downloading)
  #original datawindowname, filename, filepath, when run, days, startDate,endDate, vrn
  results<-c(df_in[[8]], theFile, kmz, descDate, the.SQL)
  #cat("<h4>Cleaning up.</h4>")
  file.remove( tmpfile)
  return(results)
}