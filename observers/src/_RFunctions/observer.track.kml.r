observer.track.kml<-function(df.tracks,filename="observer_data"){
  library(kmlbuilder)
  filename=paste0(filename,".kml")
  mykml = RKmlObject() #Create kml object
  #ugly style - should update
  mykml$addLineStyle(styleid = "linestyle", color = "#44adad", transparency = 1, width = 2, labelVisibility = 1)
  description=apply(df.tracks, 1, row.to.html.table, 
                    main=paste0("Observer data for this track, as of ",format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
                    tableSummary="Observer data for this track")
  #kmlbuilder lines need pid, lat and lon
  pid=df.tracks$FISHSET_ID
  lat=df.tracks$Y
  lon=df.tracks$X
  #names, snippets and a description (containing all attributes) make the output 
  #more useful for QC
  name=df.tracks$TRIP_ID
  Snippet = c(paste0("TRIP_ID: ",name, "TRIP:", df.tracks$TRIP))
  x=cbind(pid, name, lat, lon, Snippet, description)
  x<-as.data.frame(x)
  x$num_name<-as.numeric(x$name)
  mykml$addFolder(fid = "0", name = "Observer Tracks", description=paste0("Observer data for these tracks was generated on ",format(Sys.time(), "%Y-%m-%d %H:%M %Z")), open=0) 
  
#   #add a folder named trip -- mykml$getFolder("0")$addFolder(name="100025100", fid = "1")
#   #add all of the linestrings matching that trip
folders<-unique(x$num_name)  
for (i in 1:length(folders)){
  mykml$getFolder("0")$addFolder(name=paste0("",folders[i],""), fid = paste0("",i,""))
  mykml$getFolder("0")$getFolder(paste0("",i,""))$addLineString(x[x$num_name==folders[i],], styleUrl = "linestyle")
} 

  mykml$writekml(paste0(project.datadirectory("observers"),"/",filename,sep=""))
  print(paste0("File written to ",project.datadirectory("observers"),"/",filename,sep=""))
  print("Opening preview...")
  mykml$preview()
}
#load required ecomod functions
#loadfunctions("utility/src/_Rfunctions/data.manipulation")
#observer.track.kml(test[[2]],"tow_whole")
#observer.track.kml(observer.track.selector()[[2]],"tow_tracks")