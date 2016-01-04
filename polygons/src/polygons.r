
# Roll your own polygon via mouse interaction

# Example 1

  # load helper functions
  loadfunctions( "polygons" )  

  p = list() # start parameter list
  
  # plot background map
  # define "extents" or "bounding box" in lon/lat
  p$xlim = c(-72, -56 ) # longitudes
  p$ylim = c(42, 49) # latitudes
  p$regions = c("Canada", "USA") # library "map" polygon designations
  p$output.directory = project.datadirectory("polygons", "data","Science")
  p$output.filename = "scotia.fundy.with.buffer.dat" 
  p$output.filename = "test" 
  
  # now, using mouse interaction define region: left mouse click to register, right mouse click to finish
  polygon.db( DS="create", p=p )

  # to access the created data, you can simply read the data with read.table or read.csv
  # but to simply access the file names, first:
  
  fn = find.ecomod.gis( "scotia.fundy.with.buffer.dat"  ) 
  fn = find.ecomod.gis( "scotia.fundy.with.buffer"  )  # or, this as the extentsion *.xxx, is ignored
  scotianshelf = read.table( fn ) 
  lines( scotianshelf, col="green" )  # plot to confirm we have the right data

  # or more simply:
  scotianshelf = polygon.db( id="scotia.fundy.with.buffer" )
  polygon.db ( DS="map.background", p=p ) 
  lines( scotianshelf, col="orange" )
 
  # or to covert to a projection:
  scotianshelf = polygon.db( id="scotia.fundy.with.buffer", crs="+proj=utm +ellps=WGS84 +zone=20 +units=km" )
  
