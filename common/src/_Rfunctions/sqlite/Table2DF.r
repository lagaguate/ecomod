
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

#Takes a table tablename from a database dbname 
#This is usefull for doing R operations on a table
  Table2DF =  function(dbname, tablename){
  con <- dbConnect(dbDriver("SQLite"), dbname)
  if(dbExistsTable(con, tablename)){
    x =  dbReadTable(con, tablename)
    return(x)			
  }
  else print("No such Table name in the current working directory")			    
   }
        

