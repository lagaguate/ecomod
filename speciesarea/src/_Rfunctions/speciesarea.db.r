
  speciesarea.db = function( DS="", p=NULL, yr=NULL ) {
         
    ddir = file.path( project.datadirectory("speciesarea"), "data"  )
    dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
    infix = paste( p$spatial.domain, p$taxa, p$season, paste(p$data.sources, collapse="."), p$speciesarea.method, sep="." )


    if (DS %in% c("speciesarea.counts", "speciesarea.counts.ny", "speciesarea.counts.redo") ) {
      
      fn = file.path( ddir, paste( "speciesarea.counts", infix, "rdata", sep=".") )
      fn.ny = file.path( ddir, paste( "speciesarea.counts.ny", infix, "rdata", sep=".") )

      if (DS=="speciesarea.counts") {
        load( fn)
        return (SC)
      }
      if (DS=="speciesarea.counts.ny") {
        load( fn.ny)
        return (SC.ny)
      }
 
      set = bio.db (DS="set", p=p)
      scat = bio.db (DS="cat", p=p)

      p$nsets = nrow( set )
      p$nlengthscale = length(p$lengthscale)
      p$ntimescale = length(p$timescale)
     
      if (p$use.bigmemory.file.backing) {
 
        p$fn.tmp = file.path(  make.random.string("speciesarea.bigmemory.tmp" ))
        p$fn.desc = paste( p$fn.tmp, "desc", sep="." )
        p$fn.ny.tmp = file.path(  make.random.string("speciesarea.ny.bigmemory.tmp" ) )
        p$fn.ny.desc = paste( p$fn.ny.tmp, "desc", sep=".") 

        sar = big.matrix(nrow=p$nsets, ncol=p$nlengthscale*p$ntimescale, 
            type="double" , init=NA, backingfile=p$fn.tmp, descriptorfile=p$fn.desc  )  
        
        sar.ny = big.matrix(nrow=p$nsets, ncol=p$nlengthscale*p$ntimescale, 
            type="double" , init=NA, backingfile=p$fn.ny.tmp, descriptorfile=p$fn.ny.desc )
       
      } else {
         
        sar = big.matrix(nrow=p$nsets, ncol=p$nlengthscale*p$ntimescale, type="double" , init=NA  )  
        sar.ny = big.matrix(nrow=p$nsets, ncol=p$nlengthscale*p$ntimescale, type="double", init=NA )
 
      }
        
      
      p$bigmem.desc = describe(sar)
      p$bigmem.ny.desc = describe(sar.ny)  # counts the # of years of data

      p = make.list( list( nsets=1:p$nsets ), Y=p )
      parallel.run( species.count.engine, p=p, set=set, sc=scat )
  
      sar <- attach.big.matrix( p$bigmem.desc )
      sar.ny <- attach.big.matrix( p$bigmem.ny.desc )

      SC = array( data=sar[], dim=c( p$nsets, p$ntimescale, p$nlengthscale) )
      SC.ny = array( data=sar.ny[], dim=c( p$nsets, p$ntimescale, p$nlengthscale) )

      save( SC, file=fn, compress=T )
      save( SC.ny, file=fn.ny, compress=T )
      
      if (p$use.bigmemory.file.backing) {
        file.remove( p$fn.tmp )
        file.remove( p$fn.desc )
        file.remove( p$fn.ny.tmp )
        file.remove( p$fn.ny.desc )
      }

      return( fn )
    }


    if (DS %in% c("speciesarea.stats","speciesarea.stats.redo") ) {
     
      fn = file.path( ddir, paste("speciesarea.stats", infix, "rdata", sep=".") )
      
      if (DS=="speciesarea.stats") {
        load( fn)
        return ( sa )
      }
 
      SC = speciesarea.db( DS="speciesarea.counts", p=p )
      
      p$nvars = 9

      p$nsets = nrow(SC)
      rm(SC); gc()
   
      if (p$use.bigmemory.file.backing) {
        p$fn.tmp = file.path( make.random.string("speciesarea.stats.bigmemory.tmp") )
        p$fn.desc = paste( p$fn.tmp, "desc", sep="." )
        o = big.matrix(nrow=p$nsets, ncol=p$nvars, 
            type="double" , init=NA, backingfile=p$fn.tmp, descriptorfile=p$fn.desc) 
      } else {
        o = big.matrix(nrow=p$nsets, ncol=p$nvars, type="double", init=NA ) 
      }

      p$bigmem.desc = describe(o)

      p = make.list( list( nsets=1:p$nsets ), Y=p )
      parallel.run( speciesarea.statistics, p=p )
 
      o <- attach.big.matrix( p$bigmem.desc )
      o = as.data.frame(o[])
      
      names( o ) = c( "C", "Z", "T", "C.se", "Z.se", "T.se", "sar.rsq", "Npred", "Npred.se"   )
      o = factor2number( o, c( "C", "Z", "T", "C.se", "Z.se", "T.se", "sar.rsq", "Npred", "Npred.se"   ) )

      # save ( o, file=fn, compress=T )
           
      set = bio.db (DS="set", p=p)

      if ( nrow(set) != nrow(o ) ) {
        print( "Error: data merge failure" )
        stop( nrow(o) )
      }

      sa = cbind( set, o )
      save ( sa, file=fn, compress=T )

      if (p$use.bigmemory.file.backing) {
        file.remove( p$fn.tmp )
        file.remove( p$fn.desc )
      }

      return( fn )

    }
  
    
    # --------------------

    if (DS %in% c( "speciesarea", "speciesarea.redo" ) ) {
       
      fn = file.path( ddir, paste( "set.speciesarea.merged", infix, "rdata", sep="." ) )
        
      if (DS=="speciesarea") {
        SC = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( SC )
      }
     
      ks = speciesarea.db( DS="speciesarea.stats", p=p )
      ks = lonlat2planar( ks, proj.type=p$internal.projection, ndigits=2 )
      ks$platplon = paste( round( ks$plat ), round(ks$plon), sep="_" )
      ks$plon = ks$plat = NULL
      ks$lon = ks$lat = NULL
      
      yrs = sort( unique( ks$yr ) )
 
      # check for duplicates
      for ( y in p$yearstomodel ) {
        yy = which (ks$yr == y)
        ii = which( duplicated( ks$id[yy] ) )
        
        if (length(ii) > 0) {
          print( "The following sets have duplicated positions. The first only will be retained" )
          print( ks[yy,] [ duplicates.toremove( ks$id[yy] ) ] )
          ks = ks[ - ii,]
        }
      }

      P0 = bathymetry.db( p=p, DS="baseline" )  # prediction surface appropriate to p$spatial.domain, already in ndigits = 2
      P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )
 
      SC = merge( ks, P0, by="platplon", all.x=T, all.Y=F, sort= F, , suffixes=c("", ".P0") )
      oo = which(!is.finite( SC$plon+SC$plat ) )
      if (length(oo)>0) SC = SC[ -oo , ]  # a required field for spatial interpolation
      
      SC = habitat.lookup( SC, p=p, DS="environmentals" )

      save( SC, file=fn, compress=T )
      return (fn) 
    }

  }


