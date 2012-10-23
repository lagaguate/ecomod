
    seabird.delete = function( con, yrs ) {
      # scan through the current database and remove any data match the year that is being added
      for ( YR in yrs ) {
        if ( dbExistsTable(con, sMeta) ) {
          rid = dbGetQuery(con, paste( "SELECT unique_id FROM ", sMeta, " WHERE yr=", YR, sep="" ) ) 
          if (nrow( rid ) > 0 ) {
            dbSendQuery( con, paste( "DELETE FROM ", sMeta, " WHERE yr=", YR, sep="") )
            if (dbExistsTable(con, sBase)) {
              dbSendPreparedQuery( con, paste( "DELETE FROM ", sBase, " WHERE unique_id=:unique_id", sep=""),  rid  )
            }
          }
        }
      }
    }
    

