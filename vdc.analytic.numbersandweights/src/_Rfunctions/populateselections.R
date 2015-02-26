populateselections<-function(){
  chan<-odbcConnect(uid=oracle.vdc.user,pw=oracle.vdc.password,dsn=oracle.dsn,case='nochange',rows_at_time=1)
  selections<-list()
  
  the.years<-sqlQuery(chan,"SELECT DISTINCT(YEAR) FROM GROUNDFISH.GSMISSIONS ORDER BY YEAR")
  selections[["the.years"]]<-as.character(the.years$YEAR)
  
  the.strata <-sqlQuery(chan,"SELECT DISTINCT(STRAT) FROM GROUNDFISH.GSINF ORDER BY STRAT")
  selections[["the.strata"]]<-as.character(the.strata$STRAT)

  the.species <- sqlQuery(chan, "SELECT DISTINCT(SPEC), initcap(CNAME) COMM FROM GROUNDFISH.GSSPEC WHERE SPEC <> 9999 ORDER BY initcap(CNAME)")
  selections[["the.species"]]<-the.species
  
  #NOT SURE ABOUT USING SEASON AS SERIES - MAY HAVE TO USE GSSERIES
  #the.series <-sqlQuery(chan,"SELECT DISTINCT(SEASON) FROM GROUNDFISH.GSMISSIONS ORDER BY SEASON")
  the.series <-sqlQuery(chan,"SELECT DISTINCT(PK_SERIES_ID) SERIES FROM GROUNDFISH.GSSERIES ORDER BY PK_SERIES_ID")
  selections[["the.series"]]<-as.character(the.series$SERIES)
  
  close(chan)
  return(selections)
}