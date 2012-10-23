
	children.igraph = function( gr, id ) {
		# expects "id" to be a part of the metadata		
		out = NULL
		start.node = which( V(gr)$id == id ) 
		if ( !is.null( start.node) ) {
			reachable <- ( which( is.finite( shortest.paths(gr, start.node, mode="out") ) ) ) 
			if (length( reachable) > 0) {
				terminal.nodes <- reachable[which(degree(gr, reachable, mode="out") == 0)]
				if (length( terminal.nodes)>0 ) out = V(gr)$id[terminal.nodes]
			}
		}
		return(out)
	}



