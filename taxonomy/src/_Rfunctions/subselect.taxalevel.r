
  subselect.taxalevel = function( taxa.categories=c("species", "genus"), tx=NULL, spec ) {
    if (is.null( tx )) tx = taxa.db("complete")  
    i = which( tx$taxalevel %in% taxa.categories )
    isp = sort( unique( tx$spec[i] ))
    j = which( spec %in% isp )
    return (j)
  }
  

