
# ------------------------------------------
  lm.mass.predict = function ( x, lm, threshold=0.75 )  {
    male=1
    female=2
    nosex = 3
    x$sex[ ! which( x$sex %in% c(male, female) ) ] = nosex
 
    species =  sort( unique( as.numeric(as.character(lm$spec ) )))
    x$mass.pred = NA
    for (i in 1:length(species)) {
      for (sx in c(male, female, nosex) ) {
        wq = which( lm$spec==species[i] & lm$sex==sx )
        if (length(wq)==1) {
          if (is.finite(lm$rsquared[wq])) {
            if (lm$rsquared[wq] >= threshold ) {
              a = which( x$spec==species[i] & x$sex==sx & is.finite(x$len) )
              x$mass.pred[a] = lm$b0[wq] + lm$b1[wq] * x$len [a]
            }
          }
        }
      }
    }
    
    return (x$mass.pred)
  }


