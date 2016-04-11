send.email<-function(receipients = c("test@dfo-mpo.gc.ca"), 
                     from = "test@dfo-mpo.gc.ca",
                     subject = "Message from R",
                     message = "Hello World!",
                     attachment = "none"){
  #'MMM - Feb 2016
  #' This script allows you to send an email directly from R, specifiying the 
  #' sender, recipients, subject, message, and an attachment.
  #' 
  #' It is of dubious utility right now within DFO, as all of our email ports 
  #' are blocked within DFO.  If IMTS unblocks your comuter, recipients still 
  #' must have a dfo-mpo.gc.ca email address.
  #' If you still want to use this, please contact Mike McMahon for an 
  #' appropriate value for dfo.smtpserver
  #' 
  library(sendmailR)
  
  header <- list(cc=paste(recipients,collapse=";"))   
  
  if (attachment != "none"){
    msg <- list(mime_part(message), 
                mime_part(attachment)
    )
  } else {
    msg <- list(mime_part(message))
  }
  
  for (to in recipients) {
    #ports 25, 464, 587 and 2525 are closed on dfo computers
    sendmail(from, to, subject, msg, headers=header, 
             control = list(smtpServer = dfo.smtpserver, 
                            smtpPortSMTP=587))
  }
}			