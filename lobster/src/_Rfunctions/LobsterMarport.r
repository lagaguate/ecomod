LobsterMarport <- function(file) {
	#reading in and manipulating the marport data from lobster survey
	con = file(description=file, open="r")
	com = paste("wc -l ", file, " | awk '{ print $1 }'", sep="")
	  n = system(command=com, intern=TRUE)
 

		jj=0
		out.sensors = NULL
		out.gps = NULL
		Collector = NULL


for(j in 1:n) {
		  tmp <- scan(file=file, skip=jj, nlines=1, quiet=TRUE,what='complex')
  		if(!is.na(tmp[4]) & length(tmp)>0) {
  				if(tmp[4]=='INFO') {
  					if(tmp[10] %in% c('CAPACITY_VOLTA','PITCH','BEAM','TEMPERATURE','DEPTH','DISTANCE',"ROLL")) {
  						out.sensors = rbind(out.sensors,tmp)
  					}		
  					if(!tmp[10] %in% c('CAPACITY_VOLTA','PITCH','BEAM','TEMPERATURE','DEPTH','DISTANCE',"ROLL")) {
  						Collector = c(Collector,tmp[10])
  						}
	  				
			  	}
			  if(any(strsplit(tmp[4],",")[[1]] %in% c('$GPGGA','$GPRMC'))) {
	  					out.gps = rbind(out.gps,c(tmp[1:3],strsplit(tmp[4],",")[[1]]))
	  				}	
  				}
				jj=jj+1  
				print(jj)
			}	
		
		out.sensors = data.frame(out.sensors)
		out.gps = data.frame(out.gps)
		out.sensors$X1 = do.call(rbind,strsplit(out.sensors$X1,"\\."))[,1]
		out.gps$X1 = do.call(rbind,strsplit(out.gps$X1,"\\."))[,1]

		out.sensors$X1 = strptime(out.sensors$X1,"%H:%M:%S")
		out.gps$X1 = strptime(out.gps$X1,"%H:%M:%S")
		
		out.sensors = out.sensors[,c(1,10,15,16)]
		names(out.sensors) = c('Time','Measure','X1','X2')
		out.sensors$X1 = as.numeric(out.sensors$X1)
		out.sensors$X2 = as.numeric(out.sensors$X2)
		out.sensors$Station = strsplit(strsplit(file,"/")[[1]],"\\.")[[9]][1]

		out.gps = out.gps[,c(1,7,9)]
		names(out.gps) = c('Time','Y','X')
		out.gps$X = convert.dd.dddd(as.numeric(out.gps$X))*-1
		out.gps$Y = convert.dd.dddd(as.numeric(out.gps$Y))
		out.gps$Station = strsplit(strsplit(file,"/")[[1]],"\\.")[[9]][1]		
		return(list(out.gps, out.sensors))
		}