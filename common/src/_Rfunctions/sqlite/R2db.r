
#-------------------------------------------------------------------------------------------------------------------------

# Brent Cameron 
# R code Developed as part of a service contract for Fisheries and Oceans Canada  
# Created and tested with R version 2.10.1 in Microsoft Windows Vista Business SP2

# REFERENCES
# * R code provided by Jae Choi found in projects/common and projects/snowcrab/src directories  
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



# The following functions are simple user functions that can be called within R 
# to access and manipulate the databases examples of their use can be found in 
# 1.data2db.r

# Takes an R file from fpath and writes it as a table tablename in a database dbname
# This function is usefull for comparing past R data with current data storage
  R2db = function(fpath, dbname, tablename){
    fil = get(load(fpath))
    con <- dbConnect(dbDriver("SQLite"), dbname)
    dbWriteTable(con, tablename, fil, overwrite=T, row.names = F)
    dbDisconnect(con)
  }


