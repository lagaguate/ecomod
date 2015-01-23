
  taxonomy.filter.taxa = function ( spcode, method=c("direct", "complete"), taxafilter="all", outtype="internalcodes" ) {
    out = data.frame(spcode=spcode, sortindex=1:length(spcode), keep=FALSE ) 
    if ( outtype %in% c( "spec_bio", "internalcodes", "parsimonious", "spec.parsimonious") ) outcode="spec.parsimonious"
    if ( outtype %in% c( "spec", "groundfishcodes") ) outcode ="spec"
    taxalist = taxonomy.codes( taxa=taxafilter,method=method, outcode=outcode )  # these are still groundfish codes
    ##taxalist = taxonomy.codes( method=taxafilter, outcode=outcode )  # making a change AMC for species composition
    f = which( is.finite( out$spcode ) )
    k = which(( out$spcode %in% taxalist) )
    keep = intersect( f, k )
    return( keep )
  }


