get.species<-function(){
  species.query="SELECT DISTINCT COMMON, SPECSCD_ID
  FROM SPECIESSOUGHTCODES
  ORDER BY COMMON"
  the.species = sqlQuery(channel, species.query)
  return(the.species)
}
get.gear<-function(){
  gear.query="SELECT ISGEARCODES.GEARCD_ID,
ISGEARCODES.DESCRIPTION
FROM ISGEARCODES
ORDER BY DESCRIPTION"
  the.gear = sqlQuery(channel,gear.query)
  return(the.gear)
}