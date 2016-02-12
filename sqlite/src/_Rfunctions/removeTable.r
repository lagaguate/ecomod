
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
        


#Remove a table tablename from a database dbname. This makes use of the DBQuery function
#This is a fast way to remove tables from a database from within R
  removeTable = function(dbname, tablename){
  que = paste("drop table if exists ", tablename, sep = "")
  x = DBQuery(dbname, que)

  }


