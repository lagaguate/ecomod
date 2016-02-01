if (F) {
  # load required ecomod functions
  loadfunctions("utility/src/_Rfunctions/colours")
  loadfunctions("utility/src/_Rfunctions/data.manipulation")
}
plot.kml<-function(x, metadata=NULL, pid = "FISHSET_ID", ord=NULL, labelFields = NULL, folderFields = NULL, colourField = NULL, filename = "df_kml", drawPolys=F) {
  #'   ###MMM - Jan 2016
  #'   This function enables the plotting of data frames in Google Earth as a kml
  #'   via B Cameron's kmlbuilder package
  #'
  #'   The resultant file will be categorized using a folder hierarchy determined
  #'   by fields specified by the user, and similarly, the data will be labelled
  #'   using fields specified by the user
  #'
  #'   Specifically, it takes:
  #'     x:             a data frame
  #'     metadata:      Text that will appear when the user clicks on the layer
  #'                    in the kml file - the ideal place to put information 
  #'                    about the data 
  #'     pid:           an identifier for discrete objects (required for lines 
  #'                    and polygons, not points)
  #'     ord:           a field that can be used to determine the correct order
  #'                    for connecting points to make lines or polygons
  #'     labelFields:   a character vector of fields within the df for use in
  #'                    labelling
  #'     folderFields:  a character vector of fields within the df for use in
  #'                    folder hierarchy
  #'     colourField:  a single field within the df for use in colouring the data
  #'     filename:      a name for the out =put kml file
  library(kmlbuilder)
  library(plyr)
  
  filenamefull = paste0(filename,".kml")
  mykml = RKmlObject()

  #default styles (to be overwritten)  
  pt_style='mykml$addIconStyle(styleid = "pt_pointstyle", color = "#44adad", href = "http://maps.google.com/mapfiles/kml/shapes/target.png", scale = "1", heading = 0, labelVisibility = 0)'
  ln_style='mykml$addLineStyle(styleid = "ln_linestyle", color = "#44adad", transparency = 1, width = 2, labelVisibility = 1)'
  #poly_style='mykml$addPolyStyle(styleid = "poly_polystyle", color = "#44adad", transparency = 0.5, colorMode="normal", fill=1, outline = 1)'

  pt_labelstyle='mykml$addLabelStyle(styleid = "pt_labelstyle", color = "#44adad", transparency = 1, scale = 0.6)'
  ln_labelstyle = 'mykml$addLabelStyle(styleid = "ln_labelstyle", color = "#44adad", transparency = 1, scale = 0.6)'
  #poly_labelstyle='mykml$addLabelStyle(styleid = "poly_labelstyle", color = "#44adad", transparency = 1, scale = 0.6)'
  
  #generate custom styles as needed 
  if (!is.null(colourField)){
    #get the colours
    the.col.codes=unique(x[[colourField]])
    the.col.cols=colour.scale(type="seis", nlevels=length(the.col.codes)+1, x=100, transparency=1)$cols
    for (i in 1:length(the.col.codes)){
      eval(parse(text = gsub("44adad",the.col.cols[i,], gsub("pointstyle",the.col.codes[i], pt_style))))
      eval(parse(text = gsub("44adad",the.col.cols[i,], gsub("linestyle",the.col.codes[i], ln_style))))
      #eval(parse(text = gsub("44adad",the.col.cols[i,], gsub("polystyle",the.col.codes[i], poly_style))))
      
      eval(parse(text = gsub("44adad",the.col.cols[i,], gsub("labelstyle",the.col.codes[i], pt_labelstyle))))
      eval(parse(text = gsub("44adad",the.col.cols[i,], gsub("labelstyle",the.col.codes[i], ln_labelstyle))))
      # eval(parse(text = gsub("44adad",the.col.cols[i,], gsub("labelstyle",the.col.codes[i], poly_labelstyle))))
    }
  }else{
    eval(parse(text = this.style))
    eval(parse(text = this.label.style))
    eval(parse(text = this.line.style))
    eval(parse(text = this.label.line.style))
#     eval(parse(text = poly_style))
#     eval(parse(text = this.label.poly.style))
  }
  #do description prior to adding housekeeping fields
  x$description = apply(
    x, 1, row.to.html.table,
    main = paste0("Data for this track, as of ",format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    tableSummary = "Data for this track")
  #kmlbuilder lines need pid, lat and lon
  #if pid is defined by multiple values, concatenate them into unique identifier
  x$pid = toupper(do.call(paste, c(x[pid], sep = "")))
  x$pid = lettersToNumbers(x$pid)       

  #common naming conventions for latitude and longitudes used to identify coords
  latnames=c("LAT","SLAT","LATITUDE","Y")
  lonnames=c("LON","SLONG","LONG","LONGITUDE","X")
  x$lat = x[[intersect(latnames,toupper(names(x)))[1]]]
  x$lon = x[[intersect(lonnames,toupper(names(x)))[1]]]
  
  #If a field was provided for the drawing order, sort data appropriately
  if (length(ord %in% names(x))>0) {
    x$ord = x[[ord]] 
    x=x[with(x, order(pid,ord)), ]
  }else {
    x=x[with(x, order(pid)), ]
  } 
  
  #add fields used for display in kml
  x$name = do.call(paste, c(x[labelFields], sep = " : ")) 
  x$name = paste0("<![CDATA[",x$name,"]]>")
  x$Snippet = x$name
  
  #base folder
  mykml$addFolder(
    fid = "0",
    name = "Vessel Tracks",
    description = paste0(
      "<![CDATA[Data for these tracks was generated on ",format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
      if (!is.null(metadata)) paste0("<br>Metadata:<br><br>",metadata),
      "<br><br><hr>This file was generated by the plot.kml analytic developed by Population Ecology Division.
      Please contact <a href='mailto:mike.mcmahon@dfo-mpo.gc.ca?Subject=PED plot.kml.r'>Mike McMahon</a> (Population Ecology Division) with questions.
      or questions about this data.]]>"), open = 1
  )

  #function for adding the various features
  plot.features<-function(x, a, thislevel) {
    stylem=tapply(x[[a]], x$pid, min)
    this.ord <- as.data.frame(unique(x$pid))    
    names(this.ord)<-c("pid")
    #plot a single point for each track (for visibility)
    y=join(this.ord,x,match="first")
    addPoint = paste0(thislevel,'$addPoint(y, styleUrl = "pt_',x[x[[a]] %in% stylem,][[a]][1],'")')
    eval(parse(text = addPoint))
    
    #draw lines
    addline = paste0(thislevel, '$addLineString(x, styleUrl = "ln_',x[x[[a]] %in% stylem,][[a]][1],'")')
    eval(parse(text = addline))
    
    #find those that have 3 or more records - they can be drawn as polygons
    if (drawPolys){
        for (i in 1:nrow(this.ord)) {
          if(nrow(x[x$pid==this.ord$pid[i],])>2){
            z= x[x$pid==this.ord$pid[i],]
            addpoly = paste0(thislevel, '$addPolygon(z, styleUrl = "poly_',z[z[[a]] %in% stylem,][[a]][1],'")')
            eval(parse(text = addpoly))
          }
        }
    }
  }
  populate_kml <- function(x, y, a) {
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
      #'folder ids don't like special characters
      this = x[,y[i]][1]
      this.clean = gsub("&","AND",gsub("'","",this))
      getF = paste0(thislevel,'$getFolder(fid="', this.clean,'")')
      if (is.null(eval(parse(text = getF)))) {
        addF = paste0(thislevel,'$addFolder(name="',this,'", fid="', this.clean,'")')
        eval(parse(text = addF))
      }
      thislevel = paste0(thislevel,'$getFolder(fid="', this.clean,'")')
      eval(parse(text = thislevel))
    }
    d_ply(x, a, plot.features, a,thislevel)
  }
  folders = folderFields
  d_ply(x, folders, populate_kml, folders, colourField)
  #probably want to zip and save a kmz for dramatically smaller file
  mykml$writekml(paste0(project.datadirectory("observers"),"/",filenamefull))
  print(paste0("File written to ",project.datadirectory("observers"),"/",filenamefull))
  print("Opening preview...")
  mykml$preview()
}

#plot.kml(test[[3]], metadata=test[[1]], pid="FISHSET_ID", ord="ORD", labelFields=c("VESSEL"),folderFields=c("NAFAREA_ID"),colourField="CTRYCD_ID", filename="testing", drawPolys=F)