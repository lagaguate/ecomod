	

	network.statistics.igraph = function( g ) {
		# given a taxonomic network igraph object, determine a few stats about children
		require(igraph)

		ids = V(g)$id  # this assumes id exists
		V(g)$children.n = NA
		V(g)$children = NA
		
		for ( i in 1:length(ids)) {
			node.reference = ids[i]
			children = children.igraph( gr=g, id=node.reference ) 
			if ( is.null( children) ) {
				V(g)$children.n[i] = 0   	
			} else {
				V(g)$children.n[i] = length( children)    	
				V(g)$children[i] = paste( children, collapse="~" )
			}
		}

		debug= F
		if (debug) {
			
			# V(g) # extract vertices
			# E(g) # extrace edges
			#
			# determine unique child nodes
			# 
			# I presume that "terminal child nodes" means "nodes reachable from the given start point 
			# that have no further children". Assuming that the edges of the tree are directed downwards, 
			# you can run a single-source shortest path search from the start node (igraph_shortest_paths 
			# in C, Graph.shortest_paths() in Python, shortest.paths in R) and extract the nodes that
			# have a finite distance from the start node. From this set, you can simply remove those that
			# have non-zero out-degree.
		
				
			start.node = which( V(g)$tsn == 566846 )  
			start.node = which( V(g)$tsn == 80384 )  

			reachable <- ( which( is.finite( shortest.paths(g, start.node, mode="out") ) ) ) 
			V(g)[reachable]
			terminal.nodes <- reachable[which(degree(g, reachable, mode="out") == 0)]
			V(g)[terminal.nodes]

				plot(g, layout=layout.fruchterman.reingold,  vertex.label=vertices$plotlabel,
						 vertex.size=0.3, vertex.label.cex=0.7, edge.arrow.size=0.25 )
				plot(g, layout=layout.circle,  vertex.label=vertices$tsn,
						 vertex.size=0.3, vertex.label.cex=0.7, edge.arrow.size=0.25 )
				plot(g, layout=layout.mds, vertex.label=vertices$tsn,
						 vertex.size=0, vertex.label.cex=0.7, edge.arrow.size=0.25 )
				plot(g, vertex.label=vertices$tsn,
						 vertex.size=0, vertex.label.cex=0.7, edge.arrow.size=0.25 )

				print(g)
				plot(g)
				path.length.hist(g)
				average.path.length(g) # --- tentative measure of biodiversity ??? 
				decompose.graph(g)
				degree(g)
				diameter(g)
				edge.connectivity	
				edge.disjoint.paths
				get.adjacency
				minimum.spanning.tree

				# tkigrapgh()
				 V(g)$id <- seq_len(vcount(g))-1
				 roots <- sapply(decompose.graph(g), function(x) {
										 V(x)$id[ topological.sort(x)[1]+1 ] })
				 tree <- unfold.tree(g, roots=roots)

		}
		return( g )
	}
	

