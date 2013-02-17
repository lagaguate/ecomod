
  speciesarea.db = function( ip=NULL, DS="", p=NULL, yr=NULL ) {
   
    if (DS %in% c("speciesarea.counts", "speciesarea.counts.ny", "speciesarea.counts.redo") ) {
      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain, p$taxa, p$season, paste(p$data.sources, collapse=".")  )
      
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
     
      fn = file.path( ddir, paste( "speciesarea.counts", "rdata", sep=".") )
      fn.ny = file.path( ddir, paste( "speciesarea.counts.ny", "rdata", sep=".") )

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
     
      require(bigmemory)

        p$fn.tmp = file.path(  make.random.string("speciesarea.bigmemory.tmp" ))
        p$fn.desc = paste( p$fn.tmp, "desc", sep="." )

        sar = big.matrix(nrow=p$nsets, ncol=p$nlengthscale*p$ntimescale, 
            type="double" , init=NA, backingfile=p$fn.tmp, descriptorfile=p$fn.desc  )  
        
        p$fn.ny.tmp = file.path(  make.random.string("speciesarea.ny.bigmemory.tmp" ) )
        p$fn.ny.desc = paste( p$fn.ny.tmp, "desc", sep=".") 

        sar.ny = big.matrix(nrow=p$nsets, ncol=p$nlengthscale*p$ntimescale, 
            type="double" , init=NA, backingfile=p$fn.ny.tmp, descriptorfile=p$fn.ny.desc )
             
        p$bigmem.desc = describe(sar)
        p$bigmem.ny.desc = describe(sar.ny)  # counts the # of years of data

      
      if ( length( p$clusters) > 1 ) {
        require(snow)
        
        cl = makeCluster( spec=p$clusters, type="SOCK" )
        ssplt = lapply( clusterSplit( cl, 1:p$nsets ), function(i){i} )
        clusterApplyLB( cl, ssplt, fun=species.count.engine, p=p, set=set, sc=scat )
        stopCluster( cl )
        
       
      } else { 
        # serial mode 
        species.count.engine ( p=p, set=set, sc=scat )
        
      }

      sar <- attach.big.matrix( p$bigmem.desc )
      sar.ny <- attach.big.matrix( p$bigmem.ny.desc )

      SC = array( data=sar[], dim=c( p$nsets, p$ntimescale, p$nlengthscale) )
      SC.ny = array( data=sar.ny[], dim=c( p$nsets, p$ntimescale, p$nlengthscale) )

      file.remove( p$fn.tmp )
      file.remove( p$fn.desc )
   
      file.remove( p$fn.ny.tmp )
      file.remove( p$fn.ny.desc )

      save( SC, file=fn, compress=T )
      save( SC.ny, file=fn.ny, compress=T )

      return( fn )
    }


    if (DS %in% c("speciesarea.stats","speciesarea.stats.redo") ) {
      
      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain,  p$taxa, p$season, paste(p$data.sources, collapse=".")  , p$speciesarea.method )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
    
      fn = file.path( ddir, paste("speciesarea.stats", "rdata", sep=".") )
      
      if (DS=="speciesarea.stats") {
        load( fn)
        return ( sa )
      }
 
      SC = speciesarea.db( DS="speciesarea.counts", p=p )
      
      p$nvars = 9

      p$nsets = nrow(SC)
      rm(SC); gc()

      # bigmemory's backingdir does not seem to be working? ... defaulting to home directory
      require(bigmemory)
      p$fn.tmp = file.path( make.random.string("speciesarea.stats.bigmemory.tmp") )
      p$fn.desc = paste( p$fn.tmp, "desc", sep="." )

      o = big.matrix(nrow=p$nsets, ncol=p$nvars, 
          type="double" , init=NA, backingfile=p$fn.tmp, descriptorfile=p$fn.desc) 
      p$bigmem.desc = describe(o)

      if ( length( p$clusters) > 1 ) {
        # parallel mode
        require(snow)
        cl = makeCluster( spec=p$clusters, type="SOCK" )
        ssplt = lapply( clusterSplit( cl, 1:p$nsets ), function(i){i} )
        clusterApplyLB( cl, ssplt, fun=speciesarea.statistics, p=p )
        stopCluster( cl )
       
      } else { 
        # serial mode 
        speciesarea.statistics ( p=p )
        
      }

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

      file.remove( p$fn.tmp )
      file.remove( p$fn.desc )
 
      return( fn )

    }
  
    
    # --------------------
    
    if (DS %in% c( "speciesarea.stats.filtered", "speciesarea.stats.filtered.redo" ) ) {
 
      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain, p$taxa, p$season, paste(p$data.sources, collapse=".")   )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.speciesarea.filtered.rdata" )
        
      if (DS=="speciesarea.stats.filtered") {
        ks = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( ks )
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
      save( ks, file=fn, compress=T )
      return (fn) 
    }
    
    # -------------------  

    if (DS %in% c( "speciesarea.stats.merged", "speciesarea.stats.merged.redo" ) ) {
      
      require(chron)

      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain, p$taxa, p$season, paste(p$data.sources, collapse=".")   )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.speciesarea.merged.rdata" )
        
      if (DS=="speciesarea.stats.merged") {
        SC = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( SC )
      }
       
      P0 = bathymetry.db( p=p, DS="baseline" )  # prediction surface appropriate to p$spatial.domain, already in ndigits = 2
      P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )

      ks = speciesarea.db( DS="speciesarea.stats.filtered", p=p )

      SC = merge( ks, P0, by="platplon", all.x=T, all.Y=F, sort= F, , suffixes=c("", ".P0") )
      
      oo = which(!is.finite( SC$plon+SC$plat ) )
      if (length(oo)>0) SC = SC[ -oo , ]  # a required field for spatial interpolation

      rm(ks); gc()


      SC$chron = as.chron( as.numeric(string2chron( paste( paste( SC$yr, "Jan", "01", sep="-" ), "12:00:00") )) + SC$julian ) # required for time-dependent lookups
  
      if (!exists( "z", SC)) SC$z = NA
      SC$z = habitat.lookup.simple( SC,  p=p, vnames="z", lookuptype="depth", sp.br=p$interpolation.distances ) 
      if (!exists( "t", SC)) SC$t = NA
      SC$t = habitat.lookup.simple( SC,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 

      save( SC, file=fn, compress=T )
      
      return (fn) 
    }

  }


