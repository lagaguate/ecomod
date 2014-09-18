oceans.send.email<-function(email.df, workdir, debug){
  source(file.path( workdir,"private","email_addresses.R"))
  emailStuff<-email.df
  #dataWindow, theTitle, theFile
  cleanDataWindow<-gsub(" ","_",email.df[1])
  if (debug==T){
    cleanDataWindow<-"Debug_Folks"
  }
  fileName <-email.df[2]
  theKmz   <-email.df[3]
  descDate <-email.df[4]
  the.SQL  <-email.df[5]
  
  #cleanDataWindow<-gsub(" ","_",dataWindow)

	
##NOTE - The mailing list has been updated as of 2014-02-28	 via
##an email sent by Tanya Koropatnick.
Lophelia_CCA<-emailgrp.Lophelia_CCA

Northeast_Channel<-emailgrp.Northeast_Channel

Gully<-emailgrp.Gully

VazellaEmerald<-emailgrp.VazellaEmerald

St_Anns_Bank_Inventory_Box<-emailgrp.St_Anns_Bank_Inventory_Box

Musquash<-emailgrp.Musquash

  Debug_Folks<-emailgrp.Debug_Folks
  
  recipients<-list(Lophelia_CCA=Lophelia_CCA,
                  Northeast_Channel=Northeast_Channel,
                  Gully=Gully,
                  VazellaEmerald=VazellaEmerald,
                  St_Anns_Bank_Inventory_Box=St_Anns_Bank_Inventory_Box,
                  Musquash=Musquash,
				  Debug_Folks=Debug_Folks)
  
metadata<- paste("Google Earth is required for these new reports.  
You can download and install it (without administrative privileges) by following these instructions: 
\t.   Find the download here -- http://www.google.com/earth/download/ge/agree.html
\t.  During installation, click 'Advanced Setup', and uncheck the box requiring Administrative rights (i.e. 'Make Google Earth available...').
To use the new surveillance tool:
\t.  Double-clicking on the kmz will open Google Earth, and zoom to the extent of the new file. 
\t.  The filename will appear as a clickable hyperlink. Clicking on it will bring up a little box of metadata.
\t.  Only the Protected area(s) and the larger 'datawindow' will be initially visible.
\t.  Use the radio buttons and checkboxes next to the item names to control their visibility. 
Features of the new system
\t.  VMS shows as a track with vertices corresponding with individual VMS pings.  
\t.  For VMS tracks, the vessel's most recent position is indicated with an arrow. 
\t.  Clicking individual vertices displays the location and time of that report.
\t.  Gear categories are broken down by target species groups (i.e., 'Longline-Pelagic' is now separated from 'Longline - Groundfish').
\t.  Gear types are now viewed via toggling buttons - different from separate map views of old reports.
\t.  Data can be easily viewed in either a 'vessel-centric' or 'data source-centric' manner via toggles. 
\t.  'Information balloons' are available for all data (just click on a data point and see!), including links directly to vessel- or license-specific data (e.g. FOIP). Data tables can be copied and pasted into word or outlook email directly for follow up.

Future Work:
.  A 'self-serve' interface is being developed that will allow you to:
\t.  Generate similar reports yourself (and specify things like date ranges and/or VRNs).
.  24-h alerts for VMS (vessel entering polygon) and log entries in closures are also on the to do list.
-------------------------------------------------------------------------
Please send questions\\comments\\suggestions related to this report, please contact:
\tMike McMahon -- ",mike.email,", ",mike.phone,"
--------------------------------------------------------------------------
")

  library(sendmailR)
  from <- mike.email
  header <- list(cc=paste(recipients[[cleanDataWindow]],collapse=";")) 
  subject <- paste("Activity - Last 30 Days - ", email.df[1], sep="")
  msg<-list(mime_part(metadata), 
            mime_part(theKmz))
	 for (to in recipients[[cleanDataWindow]]) {
      #   uncomment the following line if in an environment where you can send emails
     #    sendmail(from, to, subject, msg, headers=header, control = list(smtpServer = dfo.smtpserver))

  }
  print("emailing disabled since it's broken on local machines (perhaps the port is blocked)")
}			