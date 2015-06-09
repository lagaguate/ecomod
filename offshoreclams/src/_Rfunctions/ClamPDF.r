ClamPDF<-function(final.year, outdir=file.path( project.datadirectory("offshoreclams"), "docs" ) ){
################################################################################
## actually runs the script, calling the functions as needed.
## takes an input value of the final year - e.g. ClamPDF(2015)
################################################################################

## Construct file path for output
# fname = determine name of file to save
# fpath = full path to save the file, in the docs folder of this project (offshoreclams)
fname = paste("OffShoreClams_",final.year,".pdf",sep="")
fpath = file.path( outdir, fname)

dir.create( outdir, recursive=TRUE, showWarnings=FALSE )

# open DB connection  .. need to be defined elsewhere .. private file
RODBCconn <- MakeConnection( ) 

pdf(file = fpath,
    ## onefile = "TRUE",
    height = 11.0,
    width = 8.5,
    title = "Offshore Surfclam fishery Indices for Monitoring",
)

log.data <- GetLogData(RODBCconn)
log.data <- ProcessLogData(log.data)
## reduce to cut off year
log.data <- log.data[which(log.data$Year <= final.year), ] 

################################################################################
par(                        ## set up initial figure parameters
    mfrow = c(1,1),
    omi = c(5, 1, 1, 1),      ## Set outer margins
    mai = c(1, 1.0, 0.25, 0.25)  ## Set Figure margins (inches)
   )  

################################################################################
PlotTitlePage(Sel.Bank)
fig.num <- 1
table.num <- 1
## note: have already opened database connection and retrieved file "log.data"
##       above
## Get commercial length frequency data from database
lf.data <- sqlQuery(RODBCconn, "SELECT * FROM COM_LEN_FREQ")
lf.data <- lf.data[which(lf.data$YEAR <= final.year), ]
lf.data <- lf.data[which(is.finite(lf.data$SHELL_LEN)), ] ## remove NAs
## omit lengths 200 mm and over, they are measurement errors
lf.data <- lf.data[which(lf.data$SHELL_LEN < 200), ]  
Sel.Bank <- 1
## process main loop for Banquereau
BB.output <- LoopThroughPlots(Sel.Bank, fig.num, table.num, log.data, lf.data) 
Sel.Bank <- 2
fig.num <- 7
table.num <- 4
## process main loop for Grand Bank
GB.output <- LoopThroughPlots(Sel.Bank, fig.num, table.num, log.data, lf.data) 
################################################################################
################################################################################
## now do Appendix with public Figures and Tables
################################################################################
################################################################################
PlotAnnexPage()  ## Appendix cover page
################################################################################
## Banquereau Appendix Figures and Tables
Sel.Bank <- 1
bank.txt <- "Banquereau Bank"
cpue.table <- BB.output$table.1b ## Banquereau CPUE table
annual.values  <- BB.output$ANNUAL ## Banquereau annual CPUE values
fig.num <- 1
table.num <- 1
LoopThroughAppendix(Sel.Bank, bank.txt, cpue.table, 
                                annual.values, fig.num, table.num, log.data, lf.data)
################################################################################
## Now do Grand Bank Figures and Tables
bank.txt <- "Grand Bank"
Sel.Bank <- 2
cpue.table <- GB.output$table.1b ## Grand Bank CPUE table
annual.values  <- GB.output$ANNUAL ## Grand Bank annual CPUE values
fig.num <- 4
table.num <- 3
LoopThroughAppendix(Sel.Bank, bank.txt, cpue.table, 
                    annual.values, fig.num, table.num, log.data, lf.data)
################################################################################
## Finished 
################################################################################
dev.off()            ## Close pdf file

odbcClose(RODBCconn) ## Close database connection

}

