

# ------------------------------------------
  lm.pvalue = function ( x, lm, threshold=0.75  ) {
    male=1
    female=2
    species =  sort( unique( as.numeric(as.character(lm$spec ) )))
    x$pvalue = NA
    for (i in 1:length(species)) {
      for (sx in c(male, female) ) {
        wq =  which( lm$spec==species[i] & lm$sex==sx )
        if (length(wq)==1) {
          if (is.finite(lm$rsquared[wq])) { 
            if (lm$rsquared[wq] >= threshold ) { 
              a = which( x$spec==species[i] & x$sex==sx)
              x$pvalue [a] = pnorm(q=x$residual[a], sd=lm$sigma[wq])
            }
          }
        }
      }
    }
    return (x$pvalue)
  }



