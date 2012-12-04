
  network.igraph.unpack = function( g, spec ) {
	
		# upack the attributes
		att = data.frame( index=1:length(V(g)) )  # dummy index to get the right number of rows
		for ( v in list.vertex.attributes(g) ) {
			att[,v] = get.vertex.attribute(g, v)
		}
    return (att)
  }

