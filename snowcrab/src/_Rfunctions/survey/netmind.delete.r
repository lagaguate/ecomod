
    netmind.delete = function( con, yrs ) {
      # scan through the current database and remove any data match the year that is being added
      for ( YR in yrs ) {
        if ( dbExistsTable(con, nMeta) ) {
          rid = dbGetQuery(con, paste( "SELECT unique_id FROM ", nMeta, " WHERE yr=", YR, sep="" ) )
          if (nrow( rid ) > 0 ) {
            dbSendQuery( con, paste( "DELETE FROM ", nMeta, " WHERE yr=", YR, sep="") )
            if (dbExistsTable(con, nBase)) {
              dbSendPreparedQuery( con, paste( "DELETE FROM ", nBase, " WHERE unique_id=:unique_id", sep=""),  rid  )
            }
          }
        }
      }
    }


