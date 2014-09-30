
  taxonomy.filter.taxa = function ( spcode, taxafilter="all", outtype="internalcodes" ) {
    out = data.frame(spcode=spcode, sortindex=1:length(spcode), keep=FALSE ) 
    if ( outtype %in% c( "spec_bio", "internalcodes", "parsimonious", "spec.parsimonious") ) outcode="spec.parsimonious"
    if ( outtype %in% c( "spec", "groundfishcodes") ) outcode ="spec"
    taxalist = taxonomy.codes( taxa=taxafilter, outcode=outcode )  # these are still groundfish codes
    k = which( is.finite( out$spcode ) && ( out$spcode %in% taxalist) )
    if (length(k)> 1) out$keep [k] = TRUE 
    return( which( out$keep) )
  }


