
  taxa.specid.correct = function( spec=NULL )  {
		# wrapper to return parsimonious updated spec id's	
		out = data.frame( spec=spec, sortorder=1:length(spec) )
		splist = specieslist.parsimony()
    out = merge( out, splist [, c("spec", "spec.clean")], by="spec", all.x=T, all.y=F, sort=F )
		out = out[ order( out$sortorder) , ]
    return( out$spec.clean )  
	}


