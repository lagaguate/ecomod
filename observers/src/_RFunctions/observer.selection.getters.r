#'These are "getters" - they make dropdown lists, and allow the user to select
#'appropriate parameters.  Once values(s) have been selected, these functions
#'handle the data appropriately such that the returned values will be something
#'appropriate for a sql query
#'
#'These do NOT determine the content of the dropdown lists - that is controlled
#'by the functions in observer.selection.populaters.r.
#'
#'parameters passed to these function are passed through to the appropriate
#'populater function, where it is applied.  These currently include:
#'setcode
#'tripcode
#'date.range
#'sought
#'caught
#'gear
#'vessels
get.setcode <-
  function(setcode = NULL, tripcode = NULL, date.range = NULL, sought =
             NULL, caught = NULL, gear = NULL, vessels = NULL, coords=NULL) {
    the.setcode <-
      populate.setcode(
        setcode = setcode,
        tripcode = tripcode,
        date.range = date.range,
        sought = sought,
        caught = caught,
        gear = gear,
        vessels = vessels,
        coords=coords
      )
    setcode.GUI <-
      select.list(
        paste(the.setcode$SET_TYPE, " (", the.setcode$SETCD_ID,")",sep = ""),
        multiple = T, graphics = T,
        title = "Choose a set type"
      )
    if (length(setcode.GUI) < 1) {
      setcode <- SQL.in.noquotes(as.numeric(the.setcode[,1]))
    }else{
      setcode <-
        SQL.in.noquotes(as.numeric(gsub(
          '.+\\(([0-9]+)\\).*?$', '\\1', setcode.GUI
        )))
    }
    return(setcode)
  }

get.tripcode <-
  function(setcode = NULL, tripcode = NULL, date.range = NULL, sought =
             NULL, caught = NULL, gear = NULL, vessels = NULL, coords=NULL) {
    the.tripcode <-
      populate.tripcode(
        setcode = setcode,
        tripcode = tripcode,
        date.range = date.range,
        sought = sought,
        caught = caught,
        gear = gear,
        vessels = vessels,
        coords=coords
      )
    tripcode.GUI <-
      select.list(
        paste(
          the.tripcode$TRIP_TYPE, " (", the.tripcode$TRIPCD_ID,")",sep = ""
        ),
        multiple = T, graphics = T,
        title = "Choose a trip type"
      )
    if (length(tripcode.GUI) < 1) {
      tripcode <- SQL.in.noquotes(as.numeric(tripcode.GUI[,1]))
    }else{
      tripcode <-
        SQL.in.noquotes(as.numeric(gsub(
          '.+\\(([0-9]+)\\).*?$', '\\1', tripcode.GUI
        )))
    }
    return(tripcode)
  }

get.vessels <-
  function(setcode = NULL, tripcode = NULL, date.range = NULL, sought =
             NULL, caught = NULL, gear = NULL, vessels = NULL, coords=NULL) {
    the.vessels <- populate.vessels(
      setcode = setcode,
      tripcode = tripcode,
      date.range = date.range,
      sought = sought,
      caught = caught,
      gear = gear,
      vessels = vessels,
      coords = coords
    )
    vessels.GUI <-
      select.list(
        paste(the.vessels$VESSEL_NAME, " (", the.vessels$CFV,")",sep = ""),
        multiple = T, graphics = T,
        title = "Choose vessel(s)"
      )
    #'Some of the cfv values are NA, so I'm using the names in the SQL.  This means that
    #'I have to protect against weird characters.  So far, I've
    #'doubled up apostrophes and
    #'replaced amersands with || chr(38) ||
    vessels <-
      SQL.in(trimWhiteSpace(gsub(
        "&","' || chr(38) || '",gsub("'","''",gsub('\\([^)]*\\)', '\\1', vessels.GUI))
      )))
    return(vessels)
  }

get.gear <-
  function(setcode = NULL, tripcode = NULL, date.range = NULL, sought =
             NULL, caught = NULL, gear = NULL, vessels = NULL, coords=NULL) {
    the.gear <- populate.gear(
      setcode = setcode,
      tripcode = tripcode,
      date.range = date.range,
      sought = sought,
      caught = caught,
      gear = gear,
      vessels = vessels,
      coords=coords
    )
    gear.GUI <-
      select.list(
        paste(the.gear$DESCRIPTION, " (", the.gear$GEARCD_ID,")",sep = ""),
        multiple = T, graphics = T,
        title = "Choose gear type(s)"
      )
    gear <-
      SQL.in.noquotes(as.numeric(gsub(
        '.+\\(([0-9]+)\\).*?$', '\\1', gear.GUI
      )))
    return(gear)
  }

get.caught.species <-
  function(setcode = NULL, tripcode = NULL, date.range = NULL, sought =
             NULL, caught = NULL, gear = NULL, vessels = NULL, coords=NULL) {
    the.caught.species <- populate.caught.species(
      order = "CODE",
      setcode = setcode,
      tripcode = tripcode,
      date.range = date.range,
      sought = sought,
      caught = caught,
      gear = gear,
      vessels = vessels,
      coords=coords
    )
    caught.GUI <-
      select.list(
        paste(
          the.caught.species$CAUGHT, " (", the.caught.species$SPECCD_ID,")",sep =
            ""
        ),
        multiple = T, graphics = T,
        title = "Choose one or more species"
      )
    caught <-
      SQL.in.noquotes(as.numeric(gsub(
        '.+\\(([0-9]+)\\).*?$', '\\1', caught.GUI
      )))
    return(caught)
  }

get.sought.species <-
  function(setcode = NULL, tripcode = NULL, date.range = NULL, sought =
             NULL, caught = NULL, gear = NULL, vessels = NULL, coords=NULL) {
    the.species <- populate.species(
      setcode = setcode,
      tripcode = tripcode,
      date.range = date.range,
      sought = sought,
      caught = caught,
      gear = gear,
      vessels = vessels,
      coords=coords
    )
    sought.GUI <-
      select.list(
        paste(the.species$SOUGHT, " (", the.species$SPECSCD_ID,")",sep = ""),
        multiple = T, graphics = T,
        title = "Choose a species"
      )
    sought <-
      SQL.in.noquotes(as.numeric(gsub(
        '.+\\(([0-9]+)\\).*?$', '\\1', sought.GUI
      )))
    return(sought)
  }

get.date.range <-
  function(setcode = NULL, tripcode = NULL, date.range = NULL, sought =
             NULL, caught = NULL, gear = NULL, vessels = NULL, coords=NULL) {
    date.range.GUI <- list()
    date.range <-
      as.character(
        populate.year(
          setcode = setcode,
          tripcode = tripcode,
          date.range = date.range,
          sought = sought,
          caught = caught,
          gear = gear,
          vessels = vessels,
          coords=coords
        )
      )
    date.range.start <- select.list(
      date.range,
      multiple = F, graphics = T,
      title = "Choose the earliest year of desired data"
    )
    date.range.end <- select.list(
      date.range,
      multiple = F, graphics = T,
      title = "Choose the most recent year of desired data"
    )
    date.range.GUI <- list(date.range.start,date.range.end)
    return(date.range.GUI)
  }

get.location <- function() {
  maxLat <-
    as.numeric(readline("Enter Maximum Latitude (Northern Limit) in Decimal Degrees: "))
  minLat <-
    as.numeric(readline("Enter Minimum Latitude (Southern Limit) in Decimal Degrees: "))
  maxLon <-
    as.numeric(readline("Enter Maximum Longitude (Eastern Limit) in Decimal Degrees: "))
  minLon <-
    as.numeric(readline("Enter Minimum Longitude (Western Limit) in Decimal Degrees: "))
  if (maxLon > 0)
    maxLon = maxLon * -1
  if (minLon > 0)
    minLon = minLon * -1
  bounds = c(maxLat,minLat,maxLon,minLon)
  
  return(bounds)
}
