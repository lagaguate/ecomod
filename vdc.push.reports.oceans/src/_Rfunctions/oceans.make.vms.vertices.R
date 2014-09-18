#source("Oceans/oceans.make.kml.table.R")
oceans.make.vms.vertices<-function(vertdf){
  dateField<-names(vertdf[3])
  vertex = "
  <Placemark>
  <name>
  <![CDATA[
  Individual Point Data
  ]]>
  </name>
  <!--<TimeStamp><when>:__date</when></TimeStamp>-->
<description>
  <![CDATA[
  :__description
  ]]>
</description>
  <visibility>0</visibility>  
  <styleUrl>#vertex</styleUrl>
  <Point>
  <coordinates>
  :__coordinates
</coordinates>
</Point>
  </Placemark>"
  elev=0
  tripverts = NULL
  for (m in 1:NROW(vertdf)) { 
    v = vertex
    description <- oceans.make.kml.table(vertdf[m,])
    v = sub(":__description",description,v)
    #v = sub(":__date", paste(format(strptime(vertdf[m,dateField], "%Y-%m-%d %H:%M"),format="%Y-%m-%d"),"T",format(strptime(vertdf[m,dateField], "%Y-%m-%d %H:%M"),format="%H:%M"),"Z",sep=""), v)
    v = sub( ":__name", vertdf[m,dateField], v )
    v = sub(":__coordinates", paste(vertdf[m,'LON'], ",", vertdf[m,'LAT'],",", elev, sep=""),v)
    
    #  g = sub( ":__description", description, g )
    tripverts = c(tripverts, v)
  }
  return(tripverts)
}