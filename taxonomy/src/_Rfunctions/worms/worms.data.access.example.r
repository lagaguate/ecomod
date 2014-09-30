
worms.data.access.example = function() {

#-------------------------------------------
# Install and load library, process WSDL and prepare R SOAP functions
#-------------------------------------------

# install.packages("SSOAP", repos = "http://www.omegahat.org/R")
install.packages("SSOAP", repos = "http://www.omegahat.org/R", dependencies = TRUE, type = "source")
library(SSOAP)
w = processWSDL("http://www.marinespecies.org/aphia.php?p=soap&wsdl=1")
iface = genSOAPClientInterface(, w) 
AphiaID = iface@functions$getAphiaID("Solea solea",1,('http://www.marinespecies.org/aphia.php?p=soap'))
print(AphiaID)
#should output '[0] 127160'
  
# --- not working in linux environment

#-------------------------------------------
# Look up AphiaID
#-------------------------------------------

#-------------------------------------------
# Create your specieslist
#-------------------------------------------

MySpecies<-c("solea solea", "lanice conchilega", "abra alba", "Polydora audax")
MySpecies<-data.frame(MySpecies)
MySpecList<-data.frame(unique(MySpecies))

#-------------------------------------------
# Get original AphiaID's for specieslist 
#-------------------------------------------

AphiaMatch <- function(x) { 
result<-NULL
for (i in 1:length(x)) {
AphiaRecord <- iface@functions$getAphiaID(x[i],1,('http://www.marinespecies.org/aphia.php?p=soap')) 
result<-c(result, AphiaRecord)
}
return(result)
}
MySpecList$OrigTaxID<-AphiaMatch(MySpecList$MySpecies)
MySpecList

#-------------------------------------------
# Get accepted synonym AphiaID's for specieslist 
#-------------------------------------------

SynResolv <- function(x) { 
result<-NULL
for (i in 1:length(x)) {
AphiaRecord <- iface@functions$getAphiaRecordByID(x[i],('http://www.marinespecies.org/aphia.php?p=soap')) 
result<-c(result, slot(AphiaRecord, "valid_AphiaID"))
}
return(result)
}
MySpecList$AccTaxID<-SynResolv(MySpecList$OrigTaxID)
MySpecList

#---------------------------------------------------------------------
# Add full record information (classification, ranking, authority,...)
#---------------------------------------------------------------------

getFullRecord <- function(x) { 
result<-NULL
for (i in 1:length(x)) {
AphiaRecord <- iface@functions$getAphiaRecordByID(x[i],('http://www.marinespecies.org/aphia.php?p=soap')) 
slotnames <- slotNames(AphiaRecord)
slotlist <- data.frame(rbind(1:length(slotnames)))
names(slotlist) <- slotnames
for(y in slotnames) {
slotlist[1, y] <- slot(AphiaRecord,  y)
}
result<-rbind(result, slotlist)
}
return(result)
}
AphiaRecords<-getFullRecord(MySpecList$AccTaxID)
MySpecList<-cbind(MySpecList, AphiaRecords)
MySpecList

}


