  itis.related = function( DS ) {
 
    taxadir = project.directory( "taxonomy", "data" )
    dir.create( taxadir, recursive=TRUE, showWarnings=FALSE )
 
    if (DS %in% c( "itis.oracle", "itis.oracle.redo" ) ) {
      
      ### NOT USED ??? TO DELETE ?
      
      fn.itis = file.path( taxadir, "itis.oracle.rdata" )
      if (DS=="itis.oracle" ) {  
        load( fn.itis )
        return (itis)
      }
      itis.groundfish = taxonomy.db( "itis.groundfish.redo" )
      itis.observer = taxonomy.db( "itis.observer.redo" )
      toextract = colnames( itis.observer)  # remove a few unuses vars
      itis = rbind( itis.groundfish[,toextract] , itis.observer[,toextract] )
      ii = duplicates.toremove( itis$given_spec_code )
      itis = itis[ -ii, ]
      save(itis, file=fn.itis, compress=T)
      return (itis)
    }
    
    if (DS %in% c( "itis.groundfish", "itis.groundfish.redo" ) ) {
      
      
      ### NOT USED ??? TO DELETE ?
      
      fn.itis = file.path( taxadir, "itis.groundfish.rdata" )
      if (DS=="itis.groundfish" ) {  
        load( fn.itis )
        return (itis)
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      itis = sqlQuery(connect, "select * from groundfish.itis_gs_taxon")
      odbcClose(connect)
      names(itis) =  tolower( names( itis ) )
      for (i in names(itis) ) itis[,i] = as.character( itis[,i] )
      save(itis, file=fn.itis, compress=T)
      return (itis)
    }
    
    if (DS %in% c( "itis.observer", "itis.observer.redo" ) ) {
      
      
      ### NOT USED ??? TO DELETE ?
      
      fn.itis = file.path( taxadir, "itis.observer.rdata" )
      if (DS=="itis.observer" ) {  
        load( fn.itis )
        return (itis)
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      itis = sqlQuery(connect, "select * from observer.itis_isdb_species")
      odbcClose(connect)
      names(itis) =  tolower( names( itis ) )
      for (i in names(itis) ) itis[,i] = as.character( itis[,i] )
      save(itis, file=fn.itis, compress=T)
      return (itis)
    }

  }

