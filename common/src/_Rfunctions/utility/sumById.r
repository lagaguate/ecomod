
   sumById = function( ee, id, idnames=c("id","x" ) ) { 
        dd = which( is.finite(ee) )
        id = as.factor( id[dd] )
        l0 = 1 / min( ee[ which(ee > 0) ], na.rm =T )
        data = as.numeric(ee[ dd ] * l0)
        WW = as.data.frame( xtabs( data ~ id) / l0)
        names( WW ) = idnames
        WW[, idnames[1]] = as.character( WW[, idnames[1]] )
        WW[, idnames[2]] = as.numeric( WW[, idnames[2]] )
        return(WW)
      }



