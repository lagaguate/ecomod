
taxonomy.keywords.remove = function( X, vname, withpunctuation=F, wds.toremove=NULL ) {
  
  if (withpunctuation) {
    # words to remove -- with punctuation
    if (is.null(wds.toremove)) { wds.toremove = c( 
      "etc\\.", "sps\\.", "unid\\.", "unid", "unidentified", "spp\\.", "sp\\.", "s\\.f\\.", 
			"sf\\.", "s\\.p\\.", "s\\.o\\.", "so\\.", "s\\.c\\.", "\\(ns\\)",  "o\\.", "f\\.", "p\\.", "c\\." 
    )}

    for (w in wds.toremove) {
      wgr =  paste("[[:space:]]+", w, sep="")
      X[,vname] = gsub( wgr, "", X[,vname], ignore.case=T ) 
    } 
  
  } else {
    # words to remove -- with no punctuation 
    if (is.null(wds.toremove)) { wds.toremove = c( 
      "unidentified", "adult", "SUBORDER", "ORDER", "etc", "sp", "spp", "so", "ns",  "c", "p", "o",
			"f", "s", "obsolete", "berried", "short", "larvae", "larval", "megalops" , "purse" 
    ) } 

    for (w in wds.toremove) {
      wgr =  paste("\\<", w, "\\>", sep="")
      X[,vname] = gsub( wgr, "", X[,vname], ignore.case=T ) 
		}
  }

  return (X)
}


