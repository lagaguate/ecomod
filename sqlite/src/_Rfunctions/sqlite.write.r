
#-------------------------------------------------------------------------------------------------------------------------

# Brent Cameron 
# R code Developed as part of a service contract for Fisheries and Oceans Canada  
# Created and tested with R version 2.10.1 in Microsoft Windows Vista Business SP2

# REFERENCES
# * R code provided by Jae Choi found in ecomod/common and ecomod/snowcrab/src directories  
# * Tutorials and ref card found at http://cran.r-project.org/
# * Tutorials found at http://www.sqlite.org/
# * Program used for quick database views http://sqliteman.com/

# CONTACT
# Brent Cameron
# 107 Gibbons Rd
# Huntington NS
# B1K 1V1
# Telephone: 1 902 727 2157
# Email: brentcameron1@gmail.com

# ---------------------------------------------------------------------------------------------------------------------------
  sqlite.write = function( db.name, xname, x, overwrite=TRUE, row.names=FALSE, append=FALSE ) {
    require(RSQLite)
    con <- dbConnect(dbDriver("SQLite"), db.name )
    dbSendQuery( con, " PRAGMA synchronous=OFF ;" ) # speeds up writes using buffers ... 
    dbSendQuery( con, " PRAGMA read_uncommitted = True ;" )
    dbWriteTable(con, xname, x, overwrite=overwrite, row.names=row.names, append=append)
    dbDisconnect(con)
    return (db.name)
  }



