observer.track.kml<-function(df.tracks,filename="observer_data"){
  library(kmlbuilder)
  library(plyr)
  filenamefull=paste0(filename,".kml")
  mykml = RKmlObject() #Create kml object
  #ugly style - should update
  fishpath = "http://maps.google.com/mapfiles/ms/micons/fishing.png" 
  mykml$addIconStyle(styleid = "pointstyle", href = fishpath, color = "#44adad", scale = 0.6, heading = 0, labelVisibility = 1)
  mykml$addLabelStyle(styleid="pointstyle", color = "white", transparency = 1, scale= 0.6)
  mykml$addLineStyle(styleid = "linestyle", color = "#44adad", transparency = 1, width = 2, labelVisibility = 0)
  #kmlbuilder lines need pid, lat and lon
  pid=df.tracks$FISHSET_ID
  lat=df.tracks$Y
  lon=df.tracks$X
  #names, snippets and a description (containing all attributes) make the output 
  #more useful for QC
  ord=df.tracks$ORD
  trip_id=df.tracks$TRIP_ID
  trip=df.tracks$TRIP
  cfv=df.tracks$CFV
  year=df.tracks$YEAR
  nafarea_id=df.tracks$NAFAREA_ID
  set_no=df.tracks$SET_NO
  gearcd_id=df.tracks$GEARCD_ID
  specscd_id=df.tracks$SPECSCD_ID
  vessel=paste0("<![CDATA[",df.tracks$VESSEL,"]]>")
  name=paste(df.tracks$TRIP_ID, " : ", df.tracks$SET_NO)
  Snippet = name
  description=apply(df.tracks, 1, row.to.html.table, 
                    main=paste0("Observer data for this track, as of ",format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
                    tableSummary="Observer data for this track")
  x=cbind(pid, year, name, vessel, cfv, trip_id, trip, nafarea_id, gearcd_id, specscd_id, set_no, lat, lon, ord, Snippet, description)
  x=as.data.frame(x)
  
  populate_kml<-function(x,y,z,f){
    #MMM - Jan 2016
    #You might want to display the data by nafo areas, or year, or gear type, so
    #this function was added to avoid hardcoding folder levels.  The desired 
    #folder hierarchy will correspond with the order of fields specified in 
    #vect
    #let's never speak of how long this took me to figure out
    str=paste0('mykml$getFolder("',f,'")')
    here=""
    for (i in 1:length(y)){
      #create the folder
      if (i==1) here=str
      text1=paste(here, '$addFolder(name="', x[,z[i]][1],'", fid = "', fid=x[,y[i]][1],'")',sep="")
      #print(text1)
      eval(parse(text=text1))
      #get the folder 
      here=paste(here, '$getFolder(fid="',x[,y[i]][1],'")',sep="")
      #print(here)
      eval(parse(text=here))
    }
  eval(parse(text=paste(here, '$addLineString(x, styleUrl = "linestyle")',sep="")))
  eval(parse(text=paste(here, '$addPoint(x[x$ord==min(as.numeric(unique(x$ord))),], styleUrl = "pointstyle")',sep="")))
  }
  
  
  mykml$addFolder(fid = "0", name = "Observer Tracks", description=paste0("Observer data for these tracks was generated on ",format(Sys.time(), "%Y-%m-%d %H:%M %Z")), open=1) 

  ddply(x, c("year"), 
      populate_kml, 
        c("year","cfv","trip_id"), 
        c("year","vessel","trip_id"),
        "0"
      )
  mykml$writekml(paste0(project.datadirectory("observers"),"/",filenamefull,sep=""))
  print(paste0("File written to ",project.datadirectory("observers"),"/",filenamefull,sep=""))
  print("Opening preview...")
  mykml$preview()
}

#load required ecomod functions
#loadfunctions("utility/src/_Rfunctions/data.manipulation")
#observer.track.kml(test[[2]],"tow_whole")
#observer.track.kml(observer.track.selector()[[2]],"tow_tracks")
