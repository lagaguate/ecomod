
  gully.db = function( DS="biochem", gully=NULL  ) {
    
    if ( DS %in% c( "biochem", "biochem.redo" ) ) {
      fn.gully.data = file.path( workdir, "gully.biochem.rdata")
      
      if (DS == "biochem" ) {
        load (fn.gully.data)
        return ( gully ) 
      }

      set0 = snowcrab.db("set.complete")

      a1 = c( 185, 186 )
      a2 = c( 22, 23, 24 )
      a3 = c( 509, 52, 78 )
      a4 = c( 63, 64, 341 )
      a5 = c( 155, 152, 156 )
      a6 = c( 347, 343, 344, 303 )
      a7 = c( 211, 129, 128 )

      sampling.stations = c( a1, a2, a3, a4, a5, a6, a7 )

      vs = c("trip", "set", "station", "cfa", "lon", "lat", "towquality", "z", "t", 
        "zsd", "tsd", "yr", "chron", "no.male.all")
      set = set0[ set0$yr==2006, vs]

      set$area = NA
      set$area[ which( set$station %in% a1 ) ] = "A1"
      set$area[ which( set$station %in% a2 ) ] = "A2"
      set$area[ which( set$station %in% a3 ) ] = "A3"
      set$area[ which( set$station %in% a4 ) ] = "A4"
      set$area[ which( set$station %in% a5 ) ] = "A5"
      set$area[ which( set$station %in% a6 ) ] = "A6"
      set$area[ which( set$station %in% a7 ) ] = "A7"

      set = set[ order( set$area ) ,]

      ii=which(set$station %in% sampling.stations )
      set$sampled.for.contaminants = NA
      set$sampled.for.contaminants[ii] = T
      set1 = set[ ii, ]

      plot.set = F
      if (plot.set ) {
        plot( set$lon, set$lat, pch="*", cex=0.3 )
        points( set$lon[ii], set$lat[ii], col= "red", pch="x", cex=0.4 )
        text (  set$lon[ii], set$lat[ii], set$station[ii], pos=1, cex=0.6 )

        Pr("png", "contaminants", "contaminants")
        # write.table(set, file="contaminants.txt", sep=";")
        
        # Total # of crab = 5 X 14 stations or 5 X 21 ?
      }

      #  > > Sept 26, EP 185 and EP186
      #  > > Sept 28, EP023 tow 11;
      #  > > EP 024 tow 10;
      #  > > EP 122;
      #  > > Oct 11,
      #  > > EP052, tow 4;
      #  > > EP 078, tow 2;
      #  > > EP509 tow 5
      #  > > Oct 16, EP064, tow 10;
      #  > > EP063, tow 11, EP341, tow 9
      #  > > Oct 19, EP156, tow 2;
      #  > > EP152, tow 4;
      #  > > EP155, tow 3


      # bring in set - cluster/chemical id lookup
      SS = read.csv( file.path( gully.data, "set.clusterid.csv" ) , sep="\t" )

      # bring in biochemical data
      G = read.csv( file.path( gully.data, "snowcrab.biochemical.csv" ) )
      G = merge( set1, G, by=c("trip", "set"), all.x=F, all.y=T, suffixes=c(".set", "")  )
      G$tripset = paste(G$trip, G$set, G$crab.id, G$chem.metals.replicate, G$hepato.replicate, sep="~" )
      rownames(G) = G$tripset

      # values less than 0 code for data that are belwo the detection limits
      # must decide how to fill these in with these limits (maybe mean( 0, dl ) ? )
      # these DL 's are given in :
      dl.metals = read.csv( file.path(gully.data, "metals.detection.limits.csv" ), sep="\t"  )
      dl.metals$rname =  make.names( dl.metals$metal.species )

      dl.organics = read.csv( file.path(gully.data, "organics.detection.limits.csv" ), sep="\t"  )
      dl.organics$rname =  make.names( dl.organics$organic.species )
      
      metals = c( dl.metals$rname, "hepato.mercury.ng.g" )
      organics = dl.organics$rname 
      organics = organics[ - grep( "SURROGATE", organics, ignore.case=T )  ]

     
      for (i in 1:length(metals) ) {
        minval = dl.metals$detection.limit.ug.g[i]
        vname = metals[i]
        j = which( G[,vname] < 0 )
        if (length(j) > 0 ) G[j,vname] = minval / 2 # assume half-way between 0 and minval (detection limit)
      }

      for (i in 1:length(organics) ) {
        minval = dl.organics$detection.limit.ng.g.dry[i]
        vname = organics[i]
        j = which( G[,vname] < 0 )
        if (length(j) > 0 ) G[j,vname] = minval / 2 # assume half-way between 0 and minval
      }

      # deal with replicates :: take averages

      i = which(G$chem.metals.replicate == 1 )
      tripset.metals = paste( G$trip, G$set, G$crab.id ,sep="~")
      for ( j in i ) {
        k = which( tripset.metals == tripset.metals [j] )
        for (m in setdiff( metals, "hepato.mercury.ng.g" )  ) { 
          G[ k[1], m ] = weighted.mean( x=G[k,m], w=G[k, "chem.mass.dry.g"], na.rm=T ) 
          G[ k[ 2:length(k) ], m] = NA
        }
      }


      i = which(G$hepato.mercury.replicate.id==1)
      tripset.mercury = paste( G$trip, G$set, G$crab.id ,sep="~")
      for ( j in i ) {
        k = which( tripset.mercury == tripset.mercury [j] )
        hepato = c( "hepato.mercury.ng.g" )
        for (m in hepato ) { 
          G[ k[1], m ] = weighted.mean( x=G[k,m], w=c(1,1), na.rm=T )  # assume weights are equal
          G[ k[ 2:length(k) ], m] = NA
        }
      }


      i = which(G$hepato.replicate ==1)
      tripset.organics = paste( G$trip, G$set, G$crab.id ,sep="~")
      for ( j in i ) {
        k = which( tripset.organics == tripset.organics [j] )
        for (m in organics ) { 
          G[ k[1], m ] = weighted.mean( x=G[k,m], w=G[k, "chem.mass.dry.g"], na.rm=T ) 
          G[ k[ 2:length(k) ], m] = NA
        }
      }
     
      #code heirarchical spatial information
      G$region = "Scotian Shelf" 
      G$cluster = G$area.cluster
      G$station = G$station ## already coded
      G$individual = G$crab.id
      G$replicate = ifelse( (G$chem.metals.replicate + G$hepato.mercury.replicate.id + G$hepato.replicate) > 0, 1, 0 )


      gully = list( G=G, metals=metals, organics=organics )
      save( gully, file=fn.gully.data, compress=T )
      return ( gully )
    }


    if (DS %in% c("biochem.pca.metals") ) {
 
      b = gully$G [ , gully$metals ]
      b = b [ which(rowSums(b, na.rm=T) > 0) , ]
      b = log10(b)

      biplot( prcomp( b , cor=T ) )

      sets=rownames(b)
      vars =colnames(b)
      corel = cor(b, use="pairwise.complete.obs")
      corel[is.na(corel)] = 0
      s = svd(corel)
      scores = matrix.multiply (b, s$v)  # i.e., b %*% s$v  .. force a multiplication    ignoring NA
      evec = s$v
      eval = s$d
      x = cbind(scores[,1]/sqrt(eval[1]), scores[,2]/sqrt(eval[2]) )
      y = cbind(evec[,1]*sqrt(eval[1]) , evec[,2]*sqrt(eval[2]) )

      outscores = data.frame(x)
      outscores$x = as.character( rownames(b) )

      variance = round(eval/sum(eval)*100, 1)
      print( variance)

      varloadings = NULL
      varloadings = as.data.frame(cbind(y, vars))
      names(varloadings) = c("PC1.metals", "PC2.metals", "vars" )

      metals.pca = outscores
      names( metals.pca ) = c( "PC1.metals", "PC2.metals", "tripset" )
      
      return( list( pca=metals.pca, scores=varloadings, evec=evec, eval=eval, variance=variance  ) )
 
    }


    if (DS %in% c("biochem.pca.organics") ) {
       # organics
     
      b = gully$G [ , gully$organics]
      b = b [ which(rowSums(b, na.rm=T) > 0) , ]
      b = log10(b)

      biplot( prcomp( b , cor=T ) )

      sets=rownames(b)
      vars =colnames(b)
      corel = cor(b, use="pairwise.complete.obs")
      corel[is.na(corel)] = 0
      s = svd(corel)
      scores = matrix.multiply (b, s$v)  # i.e., b %*% s$v  .. force a multiplication    ignoring NA
      evec = s$v
      eval = s$d
      x = cbind(scores[,1]/sqrt(eval[1]), scores[,2]/sqrt(eval[2]) )
      y = cbind(evec[,1]*sqrt(eval[1]) , evec[,2]*sqrt(eval[2]) )

      outscores = data.frame(x)
      outscores$x = as.character( rownames(b) )

      variance = round(eval/sum(eval)*100, 1)
      print( variance)

      varloadings = NULL
      varloadings = as.data.frame(cbind(y, vars))
      names(varloadings) = c("PC1.organics", "PC2.organics", "vars" )

      organics.pca = outscores
      names( organics.pca ) = c( "PC1.organics", "PC2.organics", "tripset" )
  
      return( list( pca=organics.pca, scores=varloadings, evec=evec, eval=eval, variance=variance ) )
 
    }


  }



