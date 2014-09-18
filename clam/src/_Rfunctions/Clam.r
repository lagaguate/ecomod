#MMM - this function is for the web version
doIt <-function(){
#Load up CGIwithR so that we can run R
source(file.path("C:/Program Files (x86)/Apache Group/Apache2/cgi-bin/r/CGIwithR_VDC.R"))
#MMM - get all of the form fields from the preceding page and name them as they had been named
getFormData()
cgiCall <- TRUE
	nparam <- length(formData)
  	newFormData <- list()
  for (i in 1:length(formData)) { 
	newFormData <-c(names(formData[i]),formData[i])
 }
#MMM - get the password that was sent
pw<-newFormData[["password"]]
#Load up the ROracle package and try to make a connection to BANK using the password we got
library(ROracle)
ora <- Oracle()
ROracleconn <- try(dbConnect(ora, username = 'DRODDICK', password = pw, dbname='//kamsok:1521/bank'))
#MMM - If we can't connect to the DB, we'll blame the password, and redirect the user to the password page
# adding a flag (i.e. "?pw") so that we'll be able to alert the user via javascript
if(class(ROracleconn) == "try-error") {
	cat("<script language='javascript'>window.location = '../r/custom/Clam_DB.htm?pw'</script>")
  } else {
#MMM - Password checks out - run the function provided by Dale
theFunction(ROracleconn)
  }
}
#MMM - function for displaying tables of data - facilitates formatting of the data (done seperately via CSS) 
html.table <- function(df,main="",center="F",tableSummary="")
{

  cat("<table  class='altrowstable' id='alternatecolor' summary=\"tableSummary\">")
  cat("<caption><em>")
    cat(main)
  cat("</em></caption>\n")
  cat("<tr>")
    nvar = length(df)
    for (i in 1:nvar) { 
      cat("<th>",names(df[i]),"</th>")
    }
  cat("</tr>\n")
  nrows = length(df[[1]])
  if (length(nrows)>0) {
   for (row in 1:nrows) {
     cat("<tr onmouseover='this.style.backgroundColor=&quot;#ffff66&quot' onmouseout='this.style.backgroundColor=&quot;&quot;'>")
     for (col in 1:nvar) {
      cat("<td >",df[[col]][row],"</td>")
     }
     cat("</tr>\n")
   }
  }
  cat("</table>")
}
#MMM - Dale's function - modified only so that it:
#1) Uses ROracle instead of RODBC (more protable - no ODBC connection to set up
#2) Wraps output figures in "CGIwithR_PNG_Open()" and "CGIwithR_PNG_Close()"
#3) Uses html.table to display tables
theFunction <-function (conn){
#MMM - note time so we know how long it took to run
	startTime<-Sys.time()
#MMM - write some HTML so we get a proper HTML page that loads necessary javascript and CSS
	cat("<HTML><HEAD><TITLE>")
	cat("Results")
	cat("</TITLE><META HTTP-EQUIV='CACHE-CONTROL' CONTENT='NO-CACHE'>")
	cat("<link rel='stylesheet'' type='text/css'' href='../rStyle.css'>")
	cat("</HEAD>")
	cat("<BODY>")
	cat("<p><a href='javascript:history.back();'>Go Back</a></p>")
	
#MMM - load the files necessary for running outputting images to a browser (including the defining of a temporary folder)
	source(file.path("C:/Program Files (x86)/Apache Group/Apache2/cgi-bin/r/CGIwithR_PNG_Open.R"))
	source(file.path("C:/Program Files (x86)/Apache Group/Apache2/cgi-bin/r/CGIwithR_PNG_Close.R"))
	TMPDIR="C:/Program Files (x86)/Apache Group/Apache2/tmp/"
	Sys.setenv(TMPDIR=TMPDIR)
	graphDir <<- TMPDIR
	graphURLroot <<- "/tmp/"


# R code for Offshore clam database 
# MMM - Using ROracle
## Now get data from database
log.data <- dbGetQuery(conn, "SELECT * FROM Dale_log_cpue2")
## ADD Year FIELD TO MAKE THINGS EASIER TO MANIPULATE
log.data$YEAR <- as.integer(format(log.data$RECORD_DATE,'%Y'))  ## Add Year
## Change NAs in N_TOWS and ROUND CATCH to 0s, usually valid 0's
log.data$N_TOWS[is.na(log.data$N_TOWS)] <- 0  
log.data$ROUND_CATCH[which(is.na(log.data$ROUND_CATCH))] <- 0  
################################################################################
## Assign fishing area, currently Banquereau (in NAFO 4Vsc = 1), 
##                                Grand Bank (in 3L,3O or 3N = 2) or 
##                                Outside (=0)
## Consider trips that were actually early surveys covering Scotian Shelf
################################################################################
log.data$BANK <- rep(0,dim(log.data)[1])
log.data$BANK[which(log.data$NAFO == "4VSC")] <- 1
log.data$BANK[which(log.data$NAFO  %in% c("3L","3O","3N"))] <- 2
## TOWING SPEED and TIME are fairly constant so for missing values fill 
## the average values for that vessel-trip-subtrip
##
t=which((is.na(log.data$AVE_TIME) & log.data$N_TOWS > 0))
if(length(t) > 0) {
  for(i in 1:length(t)) {
    recs = which((log.data$CFV == log.data[t[i],"CFV"]) & 
                 (log.data$TRIP_NO == log.data[t[i],"TRIP_NO"]) & 
                 (log.data$SUBTRIP_NO == log.data[t[i],"SUBTRIP_NO"]))
    log.data$AVE_TIME[t[i]] <- mean(log.data$AVE_TIME[recs],na.rm=T)
  }
}
## if any remaining fill in by CFV-trip
t=which((is.na(log.data$AVE_TIME) & log.data$N_TOWS > 0))
if(length(t) > 0) {
  for(i in 1:length(t)) {
    recs = which((log.data$CFV == log.data[t[i],"CFV"]) & 
                 (log.data$TRIP_NO == log.data[t[i],"TRIP_NO"]))
    log.data$AVE_TIME[t[i]] <- mean(log.data$AVE_TIME[recs],na.rm=T)
  }
}
## Now do same for tow speed
s=which((is.na(log.data$SPEED) & log.data$N_TOWS > 0))
if(length(s) > 0) {
  for(i in 1:length(s)) {
    recs = which((log.data$CFV == log.data[s[i],"CFV"]) & 
                 (log.data$TRIP_NO == log.data[s[i],"TRIP_NO"]) & 
                 (log.data$SUBTRIP_NO == log.data[s[i],"SUBTRIP_NO"]))
    log.data$SPEED[s[i]] <- mean(log.data$SPEED[recs],na.rm=T)
  }
}  
## if any remaining fill in by CFV-trip
s=which((is.na(log.data$SPEED) & log.data$N_TOWS > 0))
if(length(s) > 0) {
  for(i in 1:length(s)) {
    recs = which((log.data$CFV == log.data[s[i],"CFV"]) & 
                 (log.data$TRIP_NO == log.data[s[i],"TRIP_NO"]))
    log.data$SPEED[s[i]] <- mean(log.data$SPEED[recs],na.rm=T)
  }
}
## Recalculate AREA with some NA's replaced in SPEED and AVE_TIME, Use this instead of original AREA_TOWED
log.data$AREA = (log.data$SPEED*1000*log.data$AVE_TIME*log.data$B_WIDTH*log.data$N_TOWS/60.0)
log.data$AREA[which(is.na(log.data$AREA))]=0  ## change AREA NA's to 0,
 attach(log.data)
################################################################################
## Plot of CPUE for last 4 active vessels
################################################################################
## Set up matrix and Title for plotting
M_Title="CPUE for Last 4 active Surfclam vessels by Trip"
## matrix of info for last four vessles active in fleet
L4=cbind(c(101277,101276,133542,176085,000000),c("Atlantic Vigour","Atlantic Pursuit",
        "Ocean Concord","Arctic Endurance","Average all four"),
         c("red","blue","green","orange","black"))
L4Data = subset(log.data,(log.data$CFV %in% L4[,1] & log.data$BANK == 1)) ## Last 4 vessels on Banquereau Bank
#MMM - had to wrap trip_no in as.numeric to allow error-less addition of CFV and trip_no
temp4Trip=cbind(aggregate(RECORD_DATE~CFV + as.numeric(TRIP_NO),data= L4Data, mean,na.action=na.omit), 
	   aggregate(cbind(ROUND_CATCH,AREA)~CFV + as.numeric(TRIP_NO),data= L4Data,sum,na.rm=F))
	   temp4Trip$CPUE = temp4Trip$ROUND_CATCH/temp4Trip$AREA  ## CPUE calculated after summations
## plot with points for trip CPUE values by vessel
src <- CGIwithR_PNG_Open()
plot(temp4Trip[,c("RECORD_DATE","CPUE")],xlim=range(temp4Trip$RECORD_DATE),
     ylim=c(0,0.5),pch = 20,col=L4[match(temp4Trip$CFV,as.numeric(L4[,1])),3],
		 xlab="Year",ylab = "CPUE kg/msq",main = M_Title)
## Now do annual values for each vessel
temp4YEAR=aggregate(cbind(ROUND_CATCH,AREA)~CFV + YEAR,data= L4Data,sum)
temp4YEAR$YEAR = as.POSIXct(as.character(temp4YEAR$YEAR),format="%Y") ## YEAR has to be converted back to POSIXct date
temp4YEAR$CPUE = temp4YEAR$ROUND_CATCH/temp4YEAR$AREA  ## CPUE calculated after summations
for(v in 1:4){ ## add annual values as lines
  lines(temp4YEAR[which(temp4YEAR$CFV==L4[v,1]),c("YEAR","CPUE")],pch = 20,col=L4[v,3])
}
legend("topleft",L4[,2],bty="n",pch=20,col=L4[,3]) ## Add legend

## Now do overall annual CPUE
ANNUAL =aggregate(cbind(ROUND_CATCH,AREA)~YEAR,data = L4Data,sum)
ANNUAL$YEAR = as.POSIXct(as.character(ANNUAL$YEAR),format="%Y") ## YEAR has to be converted back to POSIXct date
ANNUAL$CPUE = ANNUAL$ROUND_CATCH/ANNUAL$AREA  ## CPUE calculated after summations
lines(ANNUAL[ , c("YEAR", "CPUE")], pch = 20, col = "black")
  
## Draw dashed black line for proposed lower CPUE limit
lines(matrix(c(range(temp4Trip$RECORD_DATE),c(0.06,0.06)),2,2),col="black", lty=2) 
text(as.POSIXct("1991",format="%Y"),0.05,"Trigger level for CPUE",col="black",
     pch = 20,pos=4)
src <- CGIwithR_PNG_Close(src)
##
##
################################################################################
## Now Look at footprint of the fishery, still using same data
################################################################################
BBData <- subset(log.data, log.data$BANK == 1) ## Banquereau Bank
BB_Footprint <- aggregate(cbind(ROUND_CATCH/1000, AREA/1000000)~YEAR, 
                          data = L4Data, sum)
names(BB_Footprint)[2] <-  "Catch_t"
names(BB_Footprint)[3] <- "km_sq"
## table of values
##BBData
## Now plot catch and footprint
src <- CGIwithR_PNG_Open()
par(mar=c(5,5,2,4))  ## set margines for plot
Ax_Tic=seq(range(BB_Footprint$YEAR)[1], range(BB_Footprint$YEAR)[2], 1)
plot(BB_Footprint[, c("YEAR", "Catch_t")], ylim=c(0,30000), pch = 20, type="b", 
	 xaxt = "n", col = "green", ann=FALSE, las = 1)
lines(matrix(c(1988,2005.5,30000,30000), 2, 2), 
      col = "green", lty=1,lwd=2)
lines(matrix(c(2005.5,max(BB_Footprint$YEAR), 24000,24000), 2, 2), 
       col = "green", lty=1, lwd=2)
text(1989, 30000, "TAC", pos=1, col = "green")

mtext("Year", side = 1, line = 2.5)             ## Add Axis titles
mtext("Catch (t)", side = 2, line = 3.5)
par(new = TRUE)                           ## reset for overlay plot
plot(BB_Footprint[, c("YEAR", "km_sq")], xaxt = "n", type="b", pch = 20, 
	col = "red", lty = 2, ylim=c(0,300), axes = FALSE, ann = FALSE)
axis(4)                       ## draw new y axis on right
mtext("Footprint (km sq)", side = 4, line = 2.5)  ## Add Y-axis title
legend(1987, 230, c("Catch", "Footprint"), bty="n", pch = 20, 
       col = c("green", "red"))
lines(matrix(c(range(BB_Footprint$YEAR), c(250,250)), 2, 2), 
      col = "red", lty=2, lwd=2)
text(2004, 248, "Trigger level for Footprint", pos=3, col="red")
axis(1, tcl = -0.2, labels=FALSE, at=Ax_Tic)  # minor tics for x axis
axis(1, tcl = -0.5)  ## Major tics and labels for x axis
mtext("Logged catch and Footprint for the Offshore Clam Fishery", side = 3, line = 1.)  ## Add Y-axis title
src <- CGIwithR_PNG_Close(src)
##
banquereau.area <- 10908.1 ## area within 100m contour of Banquereau Bank
BB_Footprint$percent.area <- 100.0*BB_Footprint$km_sq/banquereau.area
#MMM - had to use a data frame to allow output to HTML table function
out.table <- as.data.frame(cbind(BB_Footprint$YEAR,round(BB_Footprint$Catch_t,0),
                   round(BB_Footprint$km_sq,1),round(BB_Footprint$percent.area,2)))
colnames(out.table) <- c("YEAR","Logged Catch (t)","Area Dredged(km^2)","%Area")
#MMM - send table to html table function
html.table(out.table,main="Historic Footprint of the Offshore Clam Fishery")
################################################################################
## Now Map last 12 months log data  
## -------- Doing 3 years for now as database not up to date ---------
################################################################################
require(PBSmapping)
today = (Sys.Date())
## tt=round(difftime(today,log.data$RECORD_DATE,units="weeks"),0)  ## time difference in weeks from today
## t2=tt <= (52*3) ## Use 3 years as database not up to date
## Subset Banquereau data with non NA Latitude and Longitude
MapData = subset(log.data,log.data$BANK == 1 & (is.finite(log.data$LAT_DD) 
                                                & is.finite(log.data$LON_DD))
          ## time difference in weeks from today, use 3 years as data base not up to date
          & round(difftime(today, log.data$RECORD_DATE, units = "weeks"), 0) <= (52*3))
ln <- dim(MapData)[1] ## save number of records in file
## make into data frame for plotting
src <- CGIwithR_PNG_Open()
fish.points <- data.frame(PID = seq(1, ln),POS = seq(1, ln), X = MapData$LON_DD, 
                          Y = MapData$LAT_DD)
fish.points <- as.PolyData(fish.points) ## Chage to PolyData for plotting
attr(fish.points, "projection") <- "LL"
## Make polyProps file = Properties file for plotting
PID <- seq(1, ln)
pch <- rep(20, ln)
col <- rep("red", ln)
cex <- rep(0.3, ln)
## Now draw map of Banquereau, add NAFO lines and labels (nafo = 'all'),
## add Bank labels (banks=T)
Points_Par <- data.frame(PID, pch = I(pch), col = I(col), cex = I(cex))
attr(Points_Par,"projection") <- "LL"
#MMM - Correct path
source("Clams/ClamMap.r", local = T)
ClamMap(area = 'custom', ylim = c(44., 45.25),xlim = c(-60.083, -57),title = '',
        banks = T,nafo = "all", boundries = '', isobath = 'quick',
			  points.lst = list(fish.points, polyProps = Points_Par),
			  lines.lst = NULL, poly.lst = NULL, image.lst = NULL, 
        color.fun = tim.colors, color.adj=c(1, 100),
			  zlim = NA, res = 'high', bathcol = rgb(0, 0, 1, 0.5), grid = NULL)
mtext("Logged effort for last 3 Years", side = 3, line = 0.5, cex = 1.5)
src <- CGIwithR_PNG_Close(src)
##
###################################################################################
################################################################################
## Calculate and plot percentage of clams over 120 mm in the commercial samples
## Have to get new data from commercial samples
################################################################################
#MMM - add required packages individually
require(reshape)
require(plotrix) 
## get new data from open connection
len.data <- dbGetQuery(conn, "SELECT * FROM BANQUEREAU_COM_LEN_FREQ")
len.data <- len.data[which(is.finite(len.data$SHELL_LEN)), ] ## remove nulls
## reformat into columns
len.mat <- cast(len.data, SHELL_LEN~YEAR, sum, value = "NUM", fill = 0) 
## calculate percentages
p_mat <- prop.table(as.matrix(len.mat), margin = 2) * 100  
out <- p_mat
for (i in 1:ncol(out)){out[ ,i] = cumsum(out[ ,i])}      ## get cummulative %
## y values = shell lengths
P120 <- 100 - out[which(as.integer(unlist(dimnames(out)[1])) == 120), ]  
src <- CGIwithR_PNG_Open()
plot(cbind(as.integer(names(P120)), P120), ylim = c(0,15), pch = 20, col = "red",
     lty = 2, ylab = "Percent Shell Length >120 mm")
thigmophobe.labels(as.integer(names(P120)), P120,labels = colSums(len.mat))
low.fit <- lowess(cbind(as.integer(names(P120)), P120))  ## lowess fit to data
lines(low.fit, col = "red") 
abline(h = 1, col = "black", lty = 2)
text(2002, low.fit$y[4], "lowess trend fit", pos = 3,col = "red")
text(2001, 1, "trigger level for % > 120 mm", pos = 1)
xs <- as.integer(names(P120))                  ## set up for sub tic marks
ys1 <- rep(par("usr")[3], length(P120))
ys2 <- rep(par("usr")[3] - 0.2, length(P120))
segments(xs, ys1, xs, ys2, xpd = NA)
mtext("Pecent of unsorted clams over 120 mm shell length", side = 3, line = 0.5, cex = 1.5)
src <- CGIwithR_PNG_Close(src)
## Now make table #############################################################
out.table2 <- as.data.frame(cbind(as.integer(names(P120)), colSums(len.mat), P120))
colnames(out.table2) <- c("YEAR","n Unsorted","% > 120 mm")
#MMM - send table to html table function
html.table(out.table2,main="Pecent of unsorted clams over 120 mm shell length")
##write.table2(out.table, "data.txt",row.names=FALSE)
################################################################################
## New Plot
################################################################################
## Now do plot of length frequencies
windows(width=7.0, height=9, pointsize=12) ## change default plot window size <<<<<<<<<
yy <- as.integer(names(P120))
old.par<- par() ## save global parameters
src <- CGIwithR_PNG_Open()
par(mfrow=c(13,1),mar=c(0,5,0,1),oma=c(5,0,3,0)) ## set up for series of plots on page <<<<<<<<
t <- as.integer(rownames(p_mat))
t2 <- trunc((t-1)/5)*5+2.5
for(i in 1:dim(out.table2)[1]) {
  t3 <- aggregate(p_mat[,i ],by=list(as.factor(t2)),FUN=sum)
  plot(as.numeric(as.character(t3$Group.1)),t3$x, xlim=c(0,200), ylim = c(0,30),
      type="l", col = "black",lty = 2, xaxt = 'n', ann = FALSE)
  abline(v = 120,lty = 2, col = "red")
  text(175,15,label = yy[i]) ## add year label
  text(10,15,label = paste("n = ",as.character(out.table2[i,2]),sep="")) ## add n
  if(i == 7){mtext("Frequency %", side = 2, line = 2.5)}  ## Add Y-axis title)
}
axis(1)
mtext("Length frequency of unsorted commercial samples",outer=TRUE, side = 3, line = 1.5)  ## Add Y-axis title <<<<<<<<<<
mtext("Shell Length (mm)", side = 1, line = 2.5)  ## Add Y-axis title
src <- CGIwithR_PNG_Close(src)
## reset parameters
par <- old.par  ## reset global parameters
##dev.off( ) ## close  current graphics device     <<<<<<<<<<< not sure of interaction with HTML

cat("<a href='http://mcmahonm/r/custom/Clam_DB.htm'>Go Back</a>")
    cat("<p><I>VDC R: ", date(),"Elapsed time ",format(Sys.time()-startTime,digits=3)," seconds, "); 
    cat("max. memory used ",memory.size(max=TRUE)," MB</I></p>")
	cat("</HTML>")
}
#MMM - start off the whole script by calling the initial function which can check the password
#and get the data
result<-doIt()