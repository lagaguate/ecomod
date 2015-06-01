loadfunctions('BIOsurvey')
sca.data <- dget(file.path(project.datadirectory('BIOsurvey'),'data','Scallopsurveydata.txt'))
sca.str <- dget(file.path(project.datadirectory('BIOsurvey'),'data','Scallopstrata.txt'))

sca.data <- Prepare.strata.data(sca.data)
sca.str <- Prepare.strata.file(sca.str)

#Simple Stratification
	ss <- Stratify(sca.data,sca.str,sca.data$Commercial)
	summary(ss)

#boot strapped estimates
	bs <- boot.strata(ss,nresamp=1000,method='BWR')
	sbs <- summary(bs,CI.method='BC')

#habitat associations

Association.plot(sca.data, hydro="DEPTH", strata.group=sca.str,
xlab="Depth (m)")

#add line for Commercial size scallops and Depth.

lines(Association.plot(sca.data, hydro="DEPTH", strata.group=sca.str,
species="Commercial",plot=FALSE),lty=2)
