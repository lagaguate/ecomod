GetLFData <- function(vessel.ids=c(), update=T){
  ##############################################################################
  # Get data from database
  ##############################################################################
  if(update==T){
   # open DB connection  .. need to be defined elsewhere .. private file
   RODBCconn <- MakeConnection( ) 
   #odbcConnect("PTRAN", uid=oracle.personal.user, pwd=oracle.personal.password)
   vms.q="MFD_OBFMI.MARFIS_VESSELS_SYN.VESSEL_NAME,
          MFD_OBFMI.MARFIS_VESSELS_SYN.LOA,
          MFD_OBFMI.VMS_POS.*
          FROM MFD_OBFMI.MARFIS_VESSELS_SYN
          INNER JOIN MFD_OBFMI.VMS_POS
          ON MFD_OBFMI.MARFIS_VESSELS_SYN.VR_NUMBER = MFD_OBFMI.VMS_POS.VRN
          WHERE (MFD_OBFMI.VMS_POS.VRN              = '101276')
          OR (MFD_OBFMI.VMS_POS.VRN                 = '101277')
          OR (MFD_OBFMI.VMS_POS.VRN                 = '101452')
          OR (MFD_OBFMI.VMS_POS.VRN                 = '133542')
          OR (MFD_OBFMI.VMS_POS.VRN                 = '176085')
          OR (MFD_OBFMI.VMS_POS.VRN                 = '664956')
          OR (MFD_OBFMI.VMS_POS.VRN                 = '817004')
          OR (MFD_OBFMI.VMS_POS.VRN                 = '817008')
          OR (MFD_OBFMI.VMS_POS.VRN                 = '140013')"
   
   vms.data <- sqlQuery(RODBCconn, vms.q)
   odbcClose(RODBCconn)

    save(vms.data,file=file.path( project.datadirectory("offshoreclams"), "data", "VMSdata.Rdata" ))
  }
  else {
  	load(file.path( project.datadirectory("offshoreclams"), "data", "VMSdata.Rdata" ))
  }
 return(vms.data)
}

