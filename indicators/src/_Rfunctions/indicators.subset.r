

  
  indicators.subset = function( X, type  ) {
    
    varlist = X[[ type ]]
    dat = X$data[, varlist ]
    
    for (i in 1:length( X$to.log)) dat[,i] = log10(dat[,i] + 1)
    
    if ( type=="keyfactors" ) {
      lkup = X$keyfactors.names
      for (i in 1:length( varlist )) {
        j = NULL
        j = which( tolower( lkup[,1]) == tolower( varlist[i]) )
        if (length(j)==1) names(dat)[i] = lkup[ j, 2]
      }
    }
    return (dat)
  }



