
  stomach.db = function( DS="stomachs" ) {

    if ( DS %in% c("sddet", "sddet.redo"  ) ) {
      fn =  project.datadirectory("stomachs", "data", "sddet.rdata" ) 
      if ( DS == "sddet" ) {
        load( fn )
        return( sddet )
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      sddet = sqlQuery(connect, "select * from mfd_stomach.sddet" )  
      odbcClose(connect)
      names(sddet) =  tolower( names(sddet) )
      save(sddet, file=fn, compress=T)
      return (sddet)
    }
  
    if ( DS %in% c("sdinf", "sdinf.redo"  ) ) {
      fn =  project.datadirectory("stomachs", "data", "sdinf.rdata" ) 
      if ( DS == "sdinf" ) {
        load( fn )
        return( sdinf )
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      sdinf = sqlQuery(connect, "select * from mfd_stomach.sdinf" )  
      odbcClose(connect)
      names(sdinf) =  tolower( names(sdinf) )
      save(sdinf, file=fn, compress=T)
      return (sdinf)
    }
  
  
    if ( DS %in% c("sdtrips", "sdtrips.redo"  ) ) {
      fn = project.datadirectory("stomachs", "data", "sdtrips.rdata" ) 
      if ( DS == "sdtrips" ) {
        load( fn )
        return( sdtrips )
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      sdtrips = sqlQuery(connect, "select * from mfd_stomach.sdtrips" )  
      odbcClose(connect)
      names(sdtrips) =  tolower( names(sdtrips) )
      save(sdtrips, file=fn, compress=T)
      return (sdtrips)
    }
  
  
     if ( DS %in% c("sddigest", "sddigest.redo"  ) ) {
      fn = project.datadirectory("stomachs", "data", "sddigest.rdata" ) 
      if ( DS == "sddigest" ) {
        load( fn )
        return( sddigest )
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      sddigest = sqlQuery(connect, "select * from mfd_stomach.sddigest" )  
      odbcClose(connect)
      names(sddigest) =  tolower( names(sddigest) )
      save(sddigest, file=fn, compress=T)
      return (sddigest)
    }
    
    if ( DS %in% c("sdfullness", "sdfullness.redo"  ) ) {
      fn = project.datadirectory("stomachs", "data", "sdfullness.rdata" ) 
      if ( DS == "sdfullness" ) {
        load( fn )
        return( sdfullness )
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      sdfullness = sqlQuery(connect, "select * from mfd_stomach.sdfullness" )  
      odbcClose(connect)
      names(sdfullness) =  tolower( names(sdfullness) )
      save(sdfullness, file=fn, compress=T)
      return (sdfullness)
    }
    
    
    if ( DS %in% c("sditem", "sditem.redo"  ) ) {
      fn = project.datadirectory("stomachs", "data", "sditem.rdata" ) 
      if ( DS == "sditem" ) {
        load( fn )
        return( sditem )
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      sditem = sqlQuery(connect, "select * from mfd_stomach.sditem" )  
      odbcClose(connect)
      names(sditem) =  tolower( names(sditem) )
      save(sditem, file=fn, compress=T)
      return (sditem)
    }
    
    if ( DS %in% c("sdpred", "sdpred.redo"  ) ) {
      fn = project.datadirectory("stomachs", "data", "sdpred.rdata" ) 
      if ( DS == "sdpred" ) {
        load( fn )
        return( sdpred )
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      sdpred = sqlQuery(connect, "select * from mfd_stomach.sdpred" )  
      odbcClose(connect)
      names(sdpred) =  tolower( names(sdpred) )
      save(sdpred, file=fn, compress=T)
      return (sdpred)
    }
  
    if ( DS %in% c("sdsource", "sdsource.redo"  ) ) {
      fn = project.datadirectory("stomachs", "data", "sdsource.rdata" ) 
      if ( DS == "sdsource" ) {
        load( fn )
        return( sdsource )
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      sdsource = sqlQuery(connect, "select * from mfd_stomach.sdsource" )  
      odbcClose(connect)
      names(sdsource) =  tolower( names(sdsource) )
      save(sdsource, file=fn, compress=T)
      return (sdsource)
    }
  
    if ( DS %in% c("sdsto", "sdsto.redo"  ) ) {
      fn = project.datadirectory("stomachs", "data", "sdsto.rdata" ) 
      if ( DS == "sdsto" ) {
        load( fn )
        return( sdsto )
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      sdsto = sqlQuery(connect, "select * from mfd_stomach.sdsto" )  
      odbcClose(connect)
      names(sdsto) =  tolower( names(sdsto) )
      save(sdsto, file=fn, compress=T)
      return (sdsto)
    }
  
 
    if ( DS %in% c("sdtech", "sdtech.redo"  ) ) {
      fn = project.datadirectory("stomachs", "data", "sdtech.rdata" ) 
      if ( DS == "sdtech" ) {
        load( fn )
        return( sdtech )
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      sdtech = sqlQuery(connect, "select * from mfd_stomach.sdtech" )  
      odbcClose(connect)
      names(sdtech) =  tolower( names(sdtech) )
      save(sdtech, file=fn, compress=T)
      return (sdtech)
    }
  
 
  
  }

