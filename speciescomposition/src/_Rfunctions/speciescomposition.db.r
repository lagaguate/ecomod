
  speciescomposition.db = function( DS="", p=NULL, yr=NULL ) {

    ddir = file.path( project.directory("speciescomposition"), "data"  )
    dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
    
    infix = paste( p$spatial.domain,  p$taxa, p$season, sep=".")

    if (DS %in% c( "speciescomposition.ordination", "speciescomposition.ordination.redo", "pca", "ca") ) {
      
      fn.set = file.path( ddir, paste( "speciescomposition.by.set", infix, "rdata", sep=".") )
      fn.pca = file.path( ddir, paste( "pca", infix, "rdata", sep=".") )
      fn.ca  = file.path( ddir, paste( "ca",  infix, "rdata", sep=".") )
        
      if (DS=="speciescomposition.ordination") {
        set = NULL
        if (file.exists( fn.set) ) load( fn.set) 
        return ( set )
      }

      if (DS=="pca") {
        pca.out = NULL
        if (file.exists( fn.pca) ) load( fn.pca)
        return ( pca.out )
      }
  
      if (DS=="ca") {
        ca.out = NULL
        if (file.exists( fn.ca) ) load( fn.ca)
        return ( ca.out )
      }
 
      sc = bio.db( DS="cat" )  # species catch
      sc = sc[ which(is.finite( sc$zn ) ), ] 
      sc = sc[ , c("id", "spec_bio", "zn" ) ]  # zscore-transformed into 0,1
          
      set = bio.db( DS="set" ) # trip/set loc information
      set = set[ ,  c("id", "yr", "julian", "sa", "lon", "lat", "t", "z" ) ]
      set = na.omit( set ) # all are required fields
      
      # filter area
      igood = which( set$lon >= p$corners$lon[1] & set$lon <= p$corners$lon[2] 
              &  set$lat >= p$corners$lat[1] & set$lat <= p$corners$lat[2] )
      set = set[igood, ]
 
      # filter species 
      # sc$spec = taxonomy.parsimonious( spec=sc$spec )
      isc = taxonomy.filter.taxa( sc$spec_bio, taxafilter=p$taxa, outtype="internalcodes" )
      set = set[ which( set$id %in% unique( sc$id[isc]) ),]

      if ( p$season != "allseasons" ) {
        set = set[ filter.season( set$julian, period=p$season, index=T ) , ]
        sc = sc[ which( sc$id %in% unique( set$id) ), ]
      }
  
      # .. data loss due to truncation is OK 
      # ... smallest abundance adds little information to ordinations
      k = 1e3         # a large constant number to make xtabs work  but not too large as truncation is desired
      sc$zn = as.integer( sc$zn*k )
      m = xtabs( zn ~ as.factor(id) + as.factor(spec_bio), data=sc ) /k

      # remove low counts (absence) in the timeseries  .. species (cols) only
      cthreshold = 0.05 * k  # quantiles to be removed 

      finished = F
      while( !(finished) ) {
        i = unique( which(rowSums(m) == 0 ) )
        j = unique( which(colSums(m) <= cthreshold ) )
        if ( ( length(i) == 0 ) & (length(j) == 0 ) ) finished=T
        if (length(i) > 0 ) m = m[ -i , ]
        if (length(j) > 0 ) m = m[ , -j ]
      }

      # PCA
      # no need to correct for gear types/surveys .. assuming no size-specific bias .. perhaps wrong but simpler
      corel = cor( m, use="pairwise.complete.obs" ) # set up a correlation matrix ignoring NAs
      corel[ is.na(corel) ] = 0
      s = svd(corel)  # eigenanalysis via singular value decomposition
      scores = matrix.multiply (m, s$v)  # i.e., b %*% s$v  .. force a multiplication ignoring NAs
      evec = s$v
      ev = s$d
      x = cbind( scores[,1] / sqrt(ev[1] ), scores[,2] / sqrt( ev[2]) )
      y = cbind( evec[,1] * sqrt(ev[1] ) , evec[,2] * sqrt( ev[2]) )
      rownames(y) = colnames(m) 
      
      scores = data.frame( id=rownames(m), pca1=as.numeric(x[,1]), pca2=as.numeric(x[,2]), stringsAsFactors=F )
      set = merge(set, scores, by="id", all.x=T, all.y=F, sort=F)
      pca.out = list( scores=scores, eignenvectors=evec, eigenvalues=ev, cscores=y ) 
      save( pca.out, file=fn.pca, compress=T) 
      

      # Correpsondence analysis
      require(vegan)
        n = m * 0
        n[ which(m>0) ] = 1
        ord = cca( n )
        sp.sc = scores(ord)$species
        si.sc = scores(ord)$sites
        scores = data.frame( id=as.character(rownames(si.sc)), ca1=as.numeric(si.sc[,1]), ca2=as.numeric(si.sc[,2]) )
        variances=  ord$CA$eig[1:10]/sum(ord$CA$eig)*100 
        set = merge(set, scores, by="id", all.x=T, all.y=F, sort=F)
        ca.out = list( scores=scores, ca=ord, variances=variances ) 
        save( ca.out, file=fn.ca, compress=T) 
        save( set, file=fn.set, compress=T )
      
      return (fn.set) 
    }



    # -----------------------



		if (DS %in% c( "speciescomposition", "speciescomposition.redo" ) ) {

			require( chron) 
     
      fn = file.path( ddir, paste( "speciescomposition", infix, "rdata", sep=".") )

			if (DS=="speciescomposition") {
        SC = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( SC )
			}

      ks = speciescomposition.db( DS="speciescomposition.ordination", p=p )
      ks = lonlat2planar( ks, proj.type=p$internal.projection, ndigits=2 )
      ks$platplon = paste( round( ks$plat ), round(ks$plon), sep="_" )
      ks$plon = ks$plat = NULL
      ks$lon = ks$lat = NULL

      yrs = sort( unique( ks$yr ) )
      # check for duplicates
      for ( y in yrs ) {
        yy = which (ks$yr == y)
        ii = which( duplicated( ks$id[yy] ) )
        
        if (length(ii) > 0) {
          print( "The following sets have duplicated positions. The first only will be retained" )
          print( ks[yy,] [ duplicates.toremove( ks$id[yy] ) ] )
          ks = ks[ - ii,]
        }
      }

      P0 = bathymetry.db( p=p, DS="baseline" )  # prediction surface appropriate to p$spatial.domain, already in ndigits = 2
			P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )  ## TODO:: make this a generic resolution change

			SC = merge( ks, P0, by="platplon", all.x=T, all.Y=F, sort= F, suffixes=c("", ".P0"))
			SC = SC[ -which(!is.finite( SC$plon+SC$plat ) ) , ]  # a required field for spatial interpolation
      
      SC = habitat.lookup( SC, p=p, DS="environmentals" )

      save( SC, file=fn, compress=T )
			return (fn)
		}
 
  } # end function


