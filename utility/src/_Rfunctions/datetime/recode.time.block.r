 
  recode.time.block = function ( X=NULL, type="annual" ) {
    if (type=="annual") w = X$yr
    if (type=="monthly") w = X$month
    if (type=="fiveyear") w = floor( X$yr / 5 ) * 5
    if (type=="all") w = 3000
    return(w)
  }
  

