if (F) {
  #load required ecomod functions
  
}
observer.track.kml<-function(x, pid = "FISHSET_ID", labelFields = NULL, addLineMarkers =
                               T, folderFields = NULL,filename = "df_kml") {
  #   ###MMM - Jan 2016
  #   This function enables the plotting of data frames in Google Earth as a kml
  #   via B Cameron's kmlbuilder package
  #
  #   The resultant file will be categorized using a folder hierarchy determined
  #   by fields specified by the user, and similarly, the data will be labelled
  #   using fields specified by the user
  #
  #   Specifically, it takes:
  #     x:             a data frame
  #     pid:           an identifier for discrete objects (required for lines and
  #                    polygons, not points)
  #     labelFields:   a character vector of fields within the df for use in
  #                    labelling
  #     folderFields:  a character vector of fields within the df for use in
  #                    folder hierarchy
  #     filename:      a name for the out =put kml file
  
  library(kmlbuilder)
  library(plyr)
  filenamefull = paste0(filename,".kml")
  mykml = RKmlObject() #Create kml object
  
  #ugly style - should update
  fishpath = "http://maps.google.com/mapfiles/ms/micons/fishing.png"
  mykml$addIconStyle(
    styleid = "pointstyle", href = fishpath, color = "#44adad", scale = 0.6, heading = 0, labelVisibility = 1
  )
  mykml$addLabelStyle(
    styleid = "pointstyle", color = "white", transparency = 1, scale = 0.6
  )
  mykml$addLineStyle(
    styleid = "linestyle", color = "#44adad", transparency = 1, width = 2, labelVisibility = 0
  )
  #do description prior to name or snippet so they don't get added to the results
  x$description = apply(
    x, 1, row.to.html.table,
    main = paste0(
      "Observer data for this track, as of ",format(Sys.time(), "%Y-%m-%d %H:%M %Z")
    ),
    tableSummary = "Observer data for this track"
  )
  
  #kmlbuilder lines need pid, lat and lon
  x$pid = x[[pid]]
  x$lat = x$Y
  x$lon = x$X
  #kml files label data with names and snippets, so let's add some
  #x$name=x[[labelFields[1]]]
  for (j in 1:length(labelFields)) {
    if (j == 1) {
      x$name = x[[labelFields[j]]]
    }else{
      x$name = paste(x$name, x[[labelFields[j]]], sep = " : ")
    }
  }
  x$name = paste0("<![CDATA[",x$name,"]]>")
  x$Snippet = x$name
  
  mykml$addFolder(
    fid = "0",
    name = "Observer Tracks",
    description = paste0(
      "Observer data for these tracks was generated on ",format(Sys.time(), "%Y-%m-%d %H:%M %Z")
    ), open = 1
  )
  populate_kml <- function(x, y) {
    #MMM - Jan 2016
    #You might want to display the data by nafo areas, or year, or gear type, so
    #this function was added to avoid hardcoding folder levels.  The desired
    #folder hierarchy will correspond with the order of fields specified in
    #order of "folders" in the d_ply call
    #we will never speak of how long this took me to figure out
    base = 'mykml$getFolder("0")'
    thislevel = ""
    for (i in 1:length(y)) {
      if (i == 1) {
        thislevel = base
      }
      #'folder ids don't like special characters - replace them, but keep the
      #'original values for using as names
      this = x[,y[i]][1]
      this.clean = gsub("&","AND",gsub("'","",this))
      getF = paste(thislevel,'$getFolder(fid="', this.clean,'")',sep =
                     "")
      if (is.null(eval(parse(text = getF)))) {
        addF = paste(thislevel,'$addFolder(name="',this,'", fid="', this.clean,'")',sep =
                       "")
        eval(parse(text = addF))
      }
      thislevel = paste0(thislevel,'$getFolder(fid="', this.clean,'")')
      eval(parse(text = thislevel))
    }
    addline = paste(thislevel, '$addLineString(x, styleUrl = "linestyle")',sep =
                      "")
    eval(parse(text = addline))
    #if selected, add a single point to each line so that it will be more visible
    if (addLineMarkers) {
      #'this isn't working as anticpated.  sometimes a track does not get a point because x
      #'might have a smaller min(ord) than this particular track
      #'not clear how to extract smallest of this track
      this.ord = tapply(x$ORD, x$pid, min)
      addPoint = paste(thislevel, '$addPoint(x[x$ORD %in% this.ord,], styleUrl = "pointstyle")',sep =
                         "")
      if (addPoint == F)
        print("no 1")
      eval(parse(text = addPoint))
    }
  }
  folders = folderFields
  d_ply(x, folders, populate_kml, folders)
  mykml$writekml(paste0(project.datadirectory("observers"),"/",filenamefull,sep =
                          ""))
  print(paste0(
    "File written to ",project.datadirectory("observers"),"/",filenamefull,sep =
      ""
  ))
  print("Opening preview...")
  mykml$preview()
}

#observer.track.kml(observer.track.kml(), pid="FISHSET_ID",labelFields=c("VESSEL"),folderFields=c("YEAR", "NAFAREA_ID"),addLineMarkers=T,filename="testing")
#observer.track.kml(df, pid="FISHSET_ID",labelFields=c("VESSEL"),folderFields=c("NAFAREA_ID"),addLineMarkers=T,filename="testing")