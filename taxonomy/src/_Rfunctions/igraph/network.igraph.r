	

	network.igraph = function( spec=NULL, tsn=NULL, method="default", tx=taxonomy.db("complete") ) {

		# create an igraph network graph for a given set of species (spec id's or tsn's)
		# and then return some relevant data concerning each node (# children, children, etc)
	
		if ( !is.null(spec)) {
			out = data.frame( spec = sort( unique(spec) ))
			out = merge( out, tx, by="spec", all.x=T, all.y=F )
		}

		if ( !is.null(tsn)) {
			out = data.frame( itis.tsn = sort(unique( tsn ) ))
			out = merge( out, tx, by.x="tsn", by.y="itis.tsn", all.x=T, all.y=F )
		}
	
		nodata = which( !is.finite( out$itis.tsn ) )
		if (length(nodata) > 0) {
			print(" WARNING ::: data without valid tsn's are being dropped" ) 
		  print( out[nodata,c("itis.tsn", "spec", "name.scientific", "tolookup")] )
			out= out[-nodata,]
    }

		# debug
		# out=tx[ sample(200),]

	
		if (method=="default") {
			# determine vertices and edges
			fh = strsplit( out$tsn.hierarchy, "~", fixed=TRUE )
			edges = NULL
			for ( jj in 1:length(fh)) {
				x = fh[[jj]]
				nx = length(x)
				if ( nx > 1 ) {
					edges = rbind( edges, data.frame(parent=x[ 2:nx ], child=x[ 1:(nx-1) ] , stringsAsFactors=FALSE ) )
				}
			}
		}


		if (method=="direct.from.itis") {
		  stop( "# ---- not completed ... there is an error lurking, need to check -- not all matches found	")
			edges = NULL
			out$completed.traversal = F

			itaxa = itis.db( "itaxa" )
			
			for ( xx in 1:nrow(out) ) {
				if ( out$completed.traversal[xx] ) next()
				o = NULL
				o = itis.traverse( out$itis.tsn[xx], itaxa )
				if ( !is.null(o)) {
					out$completed.traversal = T
					nx = nrow(o) 
					x = o$tsn
					if ( nx > 1 ) {
						edges = rbind( edges, data.frame( parent=x[ 2:nx ], child=x[ 1:(nx-1) ] , stringsAsFactors=FALSE ) )
					}
				} 
			} 
		}

		rmov =  which( duplicated( edges ) )
		if (length(rmov)>0) edges = edges[ -rmov , ]
		vertices = data.frame( tsn=as.numeric(sort( unique( c(edges[,1], edges[,2]) ) ) ), stringsAsFactors=FALSE )

		a = taxonomy.recode( from="tsn", to="taxa", tolookup=as.numeric(vertices$tsn) )
		vertices = merge( vertices, a, by="tsn", all.x=T, all.y=F) 

		names.to.keep = which( vertices$rank <= 60 )  # 60=class, 100 =order, 140 =family
		vertices$plotlabel = ""
		vertices$plotlabel[ names.to.keep ] = vertices$sci[names.to.keep]


		require(igraph)
		g = graph.data.frame( edges, directed=TRUE, vertices=vertices )
		V(g)$id = vertices$tsn  # this is the way to assign labels !

		return(g)

	}


