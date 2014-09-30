  taxonomy.keywords.flag = function( X, vname, wds=NULL ) {
    #  words to use to flag as not needing a tsn lookup
    if (is.null(wds)) { wds = c( 
      "eggs", "egg", "reserved",  "remains", "debris", "remains", "stones", "mucus", 
      "bait", "foreign", "fluid", "operculum", "crude", "water", "Unidentified Per Set", 
      "Unidentified Species", "Unid Fish And Invertebrates", "Parasites", "Groundfish", "Marine Invertebrates",
      "Sand Tube" )
  }
    for (w in wds) {
      iw = grep( paste("\\<", w, "\\>", sep=""), X[,vname], ignore.case=T ) 
      X$tolookup[iw] = FALSE
      X$flag[iw] = w
    } 
    return (X)
  }


