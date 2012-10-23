
  parse.break = function( istart, iend, fishing ) {
    out = NULL
    i = istart - 1
    while ( i <= iend) {
      i = i + 1 
      if ( !is.finite( fishing[i] ) ) next()
      j = i
      while ( j <= iend)  {
        j = j + 1
        if ( !is.finite( fishing[j]) ) {
          out = cbind( i, j-1 )
          return (out)          
        }
      }
    }
    return ( NA )
  }


