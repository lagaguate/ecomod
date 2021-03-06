
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
        
#Query a database dbname with a desired query and return the result
#This is usefull for doing all database operations from within R 
  DBQuery = function(dbname, query){
  con  <- dbConnect(dbDriver("SQLite"), dbname)
  res <- dbSendQuery(con, query)
  x <- fetch(res)
  dbClearResult(res)
  return(x)
  }


