observer.track.kml<-function(df.tracks,filename="observer"){
  library(kmlbuilder)
  filename=paste0(filename,".kml")
  mykml = RKmlObject() #Create kml object
  #ugly style - should update
  mykml$addLineStyle(styleid = "linestyle", color = "#44adad", transparency = 1, width = 2)
  description=apply(df.tracks, 1, row.to.html.table, 
                    main=paste0("Observer data for this track, as of ",format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
                    tableSummary="Observer data for this track")
  #kmlbuilder lines need pid, lat and lon
  pid=df.tracks$FISHSET_ID
  lat=df.tracks$Y
  lon=df.tracks$X
  #names, snippets and a description (containing all attributes) make the output 
  #more useful for QC
  name=pid
  Snippet = c(paste0("FISHSET: ",name))
  x=cbind(pid, name, lat, lon, Snippet, description)
  mykml$addFolder(fid = "Observer Tracks", name = "Observer Tracks", description=paste0("Observer data for these tracks was generated on ",format(Sys.time(), "%Y-%m-%d %H:%M %Z")), open=0) 
  mykml$getFolder("Observer Tracks")$addLineString(x, styleUrl = "linestyle")
  mykml$writekml(paste0(project.datadirectory("observers"),"/",filename,sep=""))
  print(paste0("File written to ",project.datadirectory("observers"),"/",filename,sep=""))
  print("Opening preview...")
  mykml$preview()
}
#load required ecomod functions
#loadfunctions("utility/src/_Rfunctions/data.manipulation")
#observer.track.kml(test[[4]],"halibut")