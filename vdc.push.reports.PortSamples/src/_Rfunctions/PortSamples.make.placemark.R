#This accepts dfs consisting of individual trips (1 or more coordinate pairs)
#and generates kml placemarks.  Single point trips get points, multipoint trips get 
#multigeometry linestrings with points at the most recent position
#source("R/Shared.Scripts/Mike.McMahon/PortSamples/PortSamples.make.kml.table.R")
#source("R/Shared.Scripts/Mike.McMahon/PortSamples/PortSamples.make.vms.vertices.R")
PortSamples.make.placemark<-function(thisTrip,vertexFields){
  dateField<-vertexFields[3]
  generic.placemark = "
  <Placemark>
  <name>
  <![CDATA[
  :__name
  ]]>
  </name>
  <!--<TimeStamp><when>:__date</when></TimeStamp>-->
  <description>
  <![CDATA[
  :__description
  ]]>
  </description>
  <visibility>1</visibility>  
  <styleUrl>:__icons</styleUrl>
  :__coordinates
  </Placemark>"
  #make a much-reduced df for use in vertex generation
  vertdf<-unique(thisTrip[,names(thisTrip) %in% vertexFields])
  #set defaults
  g<-NULL
  placemarkType="point"  
  if ((NROW(thisTrip)>1) ){
    placemarkType="line"
  }
  elev = 0
  theCoords<-NULL
  lastCoords<-NULL
  ptCoords=NULL
  description <- NULL
  theName=NULL
  theVRN=NULL
  #get all of data common to the entire trip 
  #use first record to populate vrn, and if we can get the name, we'll use that as the identifier instead
  theVRN<-thisTrip$VRN[1]
  if((is.na(thisTrip$VESSEL_NAME[1])) || (length(thisTrip$VESSEL_NAME[1])<1)|| is.null(thisTrip$VESSEL_NAME[1])){
    theName<-theVRN
  }else{
    theName<-thisTrip$VESSEL_NAME[1]
  }
  #use last (AKA most recent) trip record to catch most recent information for balloon
  lastCoords<-paste(thisTrip[NROW(thisTrip),'LON'], ",", thisTrip[NROW(thisTrip),'LAT'],",", elev, sep="")
  description <- PortSamples.make.kml.table(thisTrip[NROW(thisTrip),])
  #add links
  description<-paste(description, "\n\t<br><A href='http://nflwebfarm01/mappingapp/Mapform.aspx?&res=1680&pid=678&pname=Maritimes View'>VMS Page</a>",sep="")
  description<-paste(description, "\n\t<br><A href='http://foip.ent.dfo-mpo.ca/foip/hailouts.php?searchForm=1&page=1&VRNQuick=",theVRN,"'>Hailouts</a>",sep="")
  description<-paste(description, "\n\t<br><A href='http://foip.ent.dfo-mpo.ca/foip/hailins.php?searchForm=1&page=1&VRNQuick=",theVRN,"'>Hail In Landings</a>",sep="")
  description<-paste(description, "\n\t<br><A href='http://foip.ent.dfo-mpo.ca/foip/vesselsearch.php?vrn=",theVRN,"'>FOIP</a>",sep="")
  description<-paste(description, "\n\t<br><A href='http://foip.ent.dfo-mpo.ca/foip/licence.php?licence=",theVRN,"'>Licence Info</a>",sep="")
	
  #add caveat
  description<-paste("<em><b>* Note:</b> The coordinates and date/time below correspond with the most recent position (which is also indicated by the arrow on the map).<br><br>Click on an individual vertex for information about a particular point.</em><br>",description,sep="")
  
  #g is the default placemark, populate it with the name and description
  g = generic.placemark
  #g = sub(":__date", paste(format(strptime(thisTrip[,dateField][NROW(thisTrip)], "%Y/%m/%d %H:%M"),format="%Y-%m-%d"),"T",format(strptime(thisTrip[,dateField][NROW(thisTrip)], "%Y/%m/%d %H:%M"),format="%H:%M"),"Z",sep=""), g)
  g = sub( ":__name", theName, g )
  g = sub( ":__description", description, g )
  k = g
  k = sub( ":__icons", paste("#VMS_Normal_line",sep=""), k )
  g = sub( ":__icons", paste("#VMS_Normal",sep=""), g )
  for (m in 1:NROW(thisTrip)) { 
    #thecoords is a list of the coords of ALL records of this trip - used by "k" to draw the trackline
    theCoords = paste(theCoords, " ", thisTrip[m,'LON'], ",", thisTrip[m,'LAT'],",", elev, sep="")
    #ptcoords is a series of concatenated point objects, used by "g" to draw the vertices
    ptCoords = paste(ptCoords, paste("\t\t<Point><coordinates>", thisTrip[m,'LON'], ",", thisTrip[m,'LAT'],",", elev,"</coordinates></Point>", sep=""),sep="\n")

  }  
  
  if (placemarkType=="line"){
    #drop all of the individual point objects into a multigeometry feature - these are the vertices
    ##g = sub( ":__coordinates", paste("<MultiGeometry>",ptCoords,"\n\t\t\t</MultiGeometry>",sep=""), g )
    #draw a line using the same points (use most recent coords for the arrow)
    ##k = sub( ":__coordinates", paste("<MultiGeometry><Point>\n\t\t<coordinates>", lastCoords,"\n\t\t</coordinates>\n\t</Point><LineString>\n\t\t<coordinates>", theCoords,"</coordinates>\n\t\t</LineString></MultiGeometry>",sep=""), k )
    g = sub( ":__coordinates", paste("<MultiGeometry><Point>\n\t\t<coordinates>", lastCoords,"\n\t\t</coordinates>\n\t</Point><LineString>\n\t\t<coordinates>", theCoords,"</coordinates>\n\t\t</LineString></MultiGeometry>",sep=""), k )
    verts = PortSamples.make.vms.vertices(vertdf)
    #return the 2 placemarks as a single jobbie
    ##g= c(g, k)  
    g=c(g, verts)
  }else{
      #have a single point VMS trip - show it with arrow
      g = sub( ":__coordinates", paste("<MultiGeometry><Point>\n\t\t<coordinates>", lastCoords,"\n\t\t</coordinates>\n\t</Point></MultiGeometry>",sep=""), k )
  }
  return(g)
}