  
  decompose.sizes.into.sets = function ( fname ) {

    b = read.xls( file= fname,
      sheet=1, from=1, colNames=F, rowNames=F, type="data.frame",  stringsAsFactors=F
    )

    b.header = b[1,]
    b = b[ 3:nrow(b) ,]
    for (i in 1: ncol(b)) b[,i] = as.numeric(b[,i])

    # print ( dim(b) ) # make sure numbers of rows and columns are correct
    # print ( b.header ) # make sure header read in correctly

    sets.list = as.numeric( gsub("set#", "", b.header, ignore.case=T ) )
    sets.list = sets.list[ which( is.finite( sets.list ) ) ]

    nsets = length( sets.list )

    out = NULL
    for ( i in 1:nsets ){
      s = NULL
      ogroup =  2*i-1
      s = data.frame( size = b[ , ogroup ] )
      s$set = sets.list[i]
      s$group = "O-group"
      out = rbind( out, s )

      s = NULL
      large =  2*i
      s = data.frame( size = b[ , large ] )
      s$set = sets.list[i]
      s$group = "Large"
      out = rbind( out, s )
    }
    out = out [ which( is.finite( out$size ) ) , ]
    return (out )
  }


