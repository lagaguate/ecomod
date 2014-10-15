#R version of STRANAL
#Mark Fowler Aug 2014
#Interactive scripting, jumping around between SQL and R
#Current example is 4X Haddock in 1974. This was truthed against the original APL version of STRANAL by Stratis Gavaris.
#Intention is to add STRANAL to SurveyScope as a web application. This scripting file serves to document the methods, and may
# suffice if the APL version is lost to us before incorporation into SurveyScope. Apparently IT will not maintain the original.

#Arithmetic precision differs between APL and R. For example we see differences in stratified means beginning about the 3rd decimal
# place. After all the math and rounding and bumping by trawlable units, we see annual totals of 89563863 from The APL STRANAL and
#89563906 from the R STRANAL, a difference of 0.0000005%.

#Replace DSN [if necessary], UserID and PassWord values here. Hopefully I remembered
# to x my pw out before putting this on github.

UserDSN=oracle.dsn
UserID=oracle.personal.user
PassWord=oracle.personal.password

#You need to edit query syntax to match your own account. All below refer to the FOWLER account, for
# example dsquery="SELECT * FROM fowler.alk". A global edit of 'fowler.' to your own
# account name in this text file will cover this.

#Stock identification (species, strata) is hard-coded so must be edited as required for several of the SQL steps.

#Sex and bin width stipulations are the 'official' defaults pulled from groundfish.gsspec. If you want to change these
#go to the SQL step that creates stock_all_raw_age and follow instructions in comments there.

#Encountered a conceivable error in the APL STRANAL, but could also be a known and disregarded issue. This is discussed in comments
# associated with replicating the Length Weight sheet.

#Some scripting is redundant with SurveyScope, but included here so STRANAL can be achieved as a stand-alone job.

	library(RODBC)
	options(stringsAsFactors = FALSE)

#Age Length Key
	dsquery="SELECT * FROM fowler.alk"
	channel<-odbcConnect(uid=UserID,pw=PassWord,dsn=UserDSN,case='nochange',rows_at_time=1)
	alk<-sqlQuery(channel,dsquery)
	names(alk) = c("age","flen","cage")
	alktable=as.data.frame(tapply(alk$cage,list(alk$flen,alk$age),sum))
	alktable[is.na(alktable)]=0
	agetotal=rowSums(alktable)
	#proportions at age per length from ALK
	propage=alktable/agetotal
	alktable=cbind(alktable,agetotal)
	lentotal=colSums(alktable)
	#display the ALK
	displayalk=rbind(alktable,lentotal)
	rownames(displayalk)[dim(displayalk)[1]]="TOTAL"
	displayalk

#Age Table
	dsquery="SELECT * FROM fowler.LF"
	channel<-odbcConnect(uid=UserID,pw=PassWord,dsn=UserDSN,case='nochange',rows_at_time=1)
	lfdata<-sqlQuery(channel,dsquery)
	names(lfdata) = c("vessel","mission","strat","tunits","setno","flen","cage","slen","sets")
	lfdata$strataset=paste(lfdata$vessel,lfdata$mission,lfdata$strat,lfdata$setno,sep="")
	lfdata$stratnumlen=lfdata$slen/lfdata$sets
#apply proportions at length per age from aged data to LF
	abundanceatlength=tapply(lfdata$stratnumlen,list(lfdata$flen),sum)
	lenvec=as.numeric(rownames(propage))
	agevec=as.numeric(names(propage))
	agetable=abundanceatlength*propage
	meanlenvec=rep(NA,length(agevec))
	for(c in 1:dim(agetable)[2])meanlenvec[c]=weighted.mean(lenvec,agetable[,c])
	displayagetable=rbind(agetable,meanlenvec)
	rownames(displayagetable)[dim(displayagetable)[1]]="Avg. Lgths"
	displayagetable

#Age Length Weight
	#DOCUMENTING A POSSIBLE ERROR HERE. NO DOCUMENTATION WITH STRANAL TO VERIFY.
	#The Avg Wgt values in the margins of the Excel sheet are weighted means, the weights being the abundances from the Age Table. The
	# scripting below replicates the Excel sheet. However it is not actually correct. A problem with weight data is the scale to which
	# our instruments can reach, very small fish (new recruits) posing a problem. These can obtain weights of zero at the precision of
	# shipboard scales. This is particularly evident on older cruises (eventually we adopted two scales to address this). The STRANAL
	# Excel output does not include weights of zero in computations. Trouble is these can be associated with a lot of fish, so the Avg
	# Wgt can be exaggerated by this approach. On the other hand the zero weights are also incorrect, so including them would under-estimate
	# the Avg Wgt. However I suspect the over-estimation is of larger magnitude than the potential under-estimation. Could be an oversight,
	# or could have been known and disregarded. Only matters at young ages (e.g. 1-2 for haddock in 1974), and might have been investigated
	# and deemed insignificant.
	dsquery="SELECT * FROM fowler.alw"
	channel<-odbcConnect(uid=UserID,pw=PassWord,dsn=UserDSN,case='nochange',rows_at_time=1)
	alw<-sqlQuery(channel,dsquery)
	names(alw) = c("age","flen","meanwt")
	ALWsheet=tapply(alw$meanwt,list(alw$flen,alw$age),mean)
	#The commented-out altALWsheet lines include fish of weight '0', the active lines exclude them
	avgwgtbyagevec=rep(NA,length(agevec))
	avgwgtbylenvec=rep(NA,length(agevec))
	for(c in 1:dim(agetable)[2])avgwgtbyagevec[c]=weighted.mean(ALWsheet[,c],agetable[,c],na.rm=T)
	for(c in 1:dim(agetable)[1])avgwgtbylenvec[c]=weighted.mean(ALWsheet[c,],agetable[c,],na.rm=T)
	#altALWsheet=ALWsheet
	#altALWsheet[is.na(altALWsheet) & agetable>0]=0
	#for(c in 1:dim(agetable)[2])avgwgtbyagevec[c]=weighted.mean(altALWsheet[,c],agetable[,c],na.rm=T)
	#for(c in 1:dim(agetable)[1])avgwgtbylenvec[c]=weighted.mean(altALWsheet[c,],agetable[c,],na.rm=T)
	displayalw=rbind(cbind(ALWsheet,avgwgtbylenvec),c(avgwgtbyagevec,NA))
	rownames(displayalw)[dim(displayalw)[1]]="Avg. Wgt."
	colnames(displayalw)[dim(displayalw)[2]]="Avg. Wgt."
	displayalw

#Length By Set
	dsquery="SELECT * FROM fowler.lset"
	channel<-odbcConnect(uid=UserID,pw=PassWord,dsn=UserDSN,case='nochange',rows_at_time=1)
	lset<-sqlQuery(channel,dsquery)
	names(lset) = c("strat","slat","slong","unitarea","mission","setno","flen","cage")
	setvar=paste(lset$strat,lset$slat,lset$slong,lset$unitarea,lset$mission,lset$setno,sep=" ")
	LbS=tapply(lset$cage,list(setvar,lset$flen),mean)
	LbS[is.na(LbS)]=0
	LbSsheet=cbind(LbS,rowSums(LbS))
	LbSsheet

#Length Mean sheet; mean number at length by strata, marginal means weighted by trawlable units
	LMsheet=tapply(lfdata$cage/lfdata$sets,list(lfdata$strat,lfdata$flen),sum)
	LMsheet[is.na(LMsheet)]=0
	tunitsvec=tapply(lfdata$tunits,list(lfdata$strat),mean)
	stratabunbylenvec=rep(NA,length(lenvec))
	for(c in 1:dim(LMsheet)[2])stratabunbylenvec[c]=weighted.mean(LMsheet[,c],tunitsvec)
	displayLM=rbind(LMsheet,stratabunbylenvec)
	displayLM=cbind(displayLM,c(rowSums(LMsheet),sum(stratabunbylenvec)))
	#The TOTAL label for the bottom row might be misleading, as the bottom row is the stratified mean
	rownames(displayLM)[dim(displayLM)[1]]="TOTAL"
	colnames(displayLM)[dim(displayLM)[2]]="TOTAL"
	displayLM

#Length Mean SE sheet
	#shortcut trick with sets per strata for mean numbers does not allow for error measures (no zero counts); also zaps all-zero strata
	#want a filled length by strata by set array but R does not do ragged arrays (conveniently) so do a dataframe
	stratavec=as.numeric(rownames(LMsheet))
	setvec=as.numeric(tapply(lfdata$sets,list(lfdata$strat),mean,na.rm=T))
	setmat=tapply(lfdata$sets/lfdata$sets,list(lfdata$strat,lfdata$strataset),sum)
	fillzero=rep(0,length(lenvec)*sum(setvec))
	LFSD=data.frame(strat=fillzero,tunits=fillzero,flen=fillzero,set=fillzero,meannum=fillzero,stratnum=fillzero)
	inc=0
	for(s in 1:length(stratavec)) {
		for(l in 1:length(lenvec)) {
			ns=setvec[s]
			for(nss in 1:ns) {
				inc=inc+1
				LFSD$strat[inc]=stratavec[s]
				LFSD$tunits[inc]=tunitsvec[s]
				LFSD$sets[inc]=setvec[s]
				LFSD$flen[inc]=lenvec[l]
				curset=names(setmat[as.numeric(rownames(setmat))==stratavec[s],!is.na(setmat[as.numeric(rownames(setmat))==stratavec[s],])])[nss]
				LFSD$set[inc]=curset
				seekvalue=lfdata$cage[lfdata$strat==stratavec[s] & lfdata$flen==lenvec[l] & lfdata$strataset==curset]
				seekstratvalue=lfdata$stratnumlen[lfdata$strat==stratavec[s] & lfdata$flen==lenvec[l] & lfdata$strataset==curset]
				if(length(seekvalue)>0) {
					if(!is.na(seekvalue))LFSD$meannum[inc]=seekvalue
					if(!is.na(seekvalue))LFSD$stratnum[inc]=seekstratvalue
				}
			}
		}
	}
	yyy=tapply(LFSD$meannum,list(LFSD$strat,LFSD$flen),sd)
	LMSE=yyy/sqrt(setvec)
	stratabunbylenerrmat=matrix(NA,length(stratavec),length(lenvec))
	for(s in 1:length(stratavec)) {
		for(c in 1:length(lenvec))stratabunbylenerrmat[s,c]=var(LFSD$meannum[LFSD$strat==stratavec[s] & LFSD$flen==lenvec[c]])
	}
	StratLenSE=rep(NA,length(lenvec))
	for(c in 1:length(lenvec))StratLenSE[c]=sqrt(sum((((tunitsvec * (tunitsvec - setvec))/sum(tunitsvec)^2.) * stratabunbylenerrmat[,c])/setvec))
	NumByStratSet=aggregate(LFSD$meannum, by=list(strat=LFSD$strat,tunits=LFSD$tunits,set=LFSD$set,sets=LFSD$sets),sum)
	StratSE=rep(NA,length(stratavec))
	for(s in 1:length(stratavec)) {
		StratSE[s]=sqrt(var(NumByStratSet$x[NumByStratSet$strat==stratavec[s]]))/sqrt(setvec[s])
	}
	StratVar=rep(NA,length(stratavec))
	for(s in 1:length(stratavec)) {
		StratVar[s]=var(NumByStratSet$x[NumByStratSet$strat==stratavec[s]])
	}
	TotalSE=sqrt(sum((((tunitsvec * (tunitsvec - setvec))/sum(tunitsvec)^2.) * StratVar)/setvec))
	displayLMSE=rbind(LMSE,StratLenSE)
	displayLMSE=cbind(displayLMSE,c(StratSE,TotalSE))
	rownames(displayLMSE)[dim(displayLMSE)[1]]="TOTAL"
	colnames(displayLMSE)[dim(displayLMSE)[2]]="TOTAL"
	displayLMSE
#StratSE and TotalSE are the same for the Age SE sheet

#Length Total sheet; total number at length, bumped by trawlable units
	LTsheet=tapply(lfdata$slen/lfdata$sets,list(lfdata$strat,lfdata$flen),sum)
	LTsheet[is.na(LTsheet)]=0
	displayLT=rbind(LTsheet,colSums(LTsheet))
	displayLT=cbind(displayLT,rowSums(displayLT))
	rownames(displayLT)[dim(displayLT)[1]]="TOTAL"
	colnames(displayLT)[dim(displayLT)[2]]="TOTAL"
	displayLT

#Length Total SE sheet
	LFSDtot=data.frame(strat=fillzero,tunits=fillzero,flen=fillzero,set=fillzero,meannum=fillzero,stratnum=fillzero)
	inc=0
	for(s in 1:length(stratavec)) {
		for(l in 1:length(lenvec)) {
			ns=setvec[s]
			for(nss in 1:ns) {
				inc=inc+1
				LFSDtot$strat[inc]=stratavec[s]
				LFSDtot$tunits[inc]=tunitsvec[s]
				LFSDtot$sets[inc]=setvec[s]
				LFSDtot$flen[inc]=lenvec[l]
				curset=names(setmat[as.numeric(rownames(setmat))==stratavec[s],!is.na(setmat[as.numeric(rownames(setmat))==stratavec[s],])])[nss]
				LFSDtot$set[inc]=curset
				seekvalue=lfdata$cage[lfdata$strat==stratavec[s] & lfdata$flen==lenvec[l] & lfdata$strataset==curset]*lfdata$tunits[lfdata$strat==stratavec[s] & lfdata$flen==lenvec[l] & lfdata$strataset==curset]
				if(length(seekvalue)>0) {
					if(!is.na(seekvalue))LFSDtot$meannum[inc]=seekvalue
				}
			}
		}
	}
	yyy=tapply(LFSDtot$meannum,list(LFSDtot$strat,LFSDtot$flen),sd)
	LTSE=yyy/sqrt(setvec)
	stratabunbylentoterrmat=matrix(NA,length(stratavec),length(lenvec))
	for(s in 1:length(stratavec)) {
		for(c in 1:length(lenvec))stratabunbylentoterrmat[s,c]=var(LFSDtot$meannum[LFSDtot$strat==stratavec[s] & LFSDtot$flen==lenvec[c]])
	}
	StratLenSEtot=rep(NA,length(lenvec))
	for(c in 1:length(lenvec))StratLenSEtot[c]=sqrt(sum(stratabunbylentoterrmat[,c]/setvec))
	NumByStratSetTot=aggregate(LFSDtot$meannum, by=list(strat=LFSDtot$strat,tunits=LFSDtot$tunits,set=LFSDtot$set,sets=LFSDtot$sets),sum)
	StratSEtot=rep(NA,length(stratavec))
	for(s in 1:length(stratavec)) {
		StratSEtot[s]=sqrt(var(NumByStratSetTot$x[NumByStratSetTot$strat==stratavec[s]]))/sqrt(setvec[s])
	}
	StratVarTot=rep(NA,length(stratavec))
	for(s in 1:length(stratavec)) {
		StratVarTot[s]=var(NumByStratSetTot$x[NumByStratSetTot$strat==stratavec[s]])
	}
	TotalSEtot=sqrt(sum(StratVarTot/setvec))
	displayLTSE=rbind(LTSE,StratLenSEtot)
	displayLTSE=cbind(displayLTSE,c(StratSEtot,TotalSEtot))
	rownames(displayLTSE)[dim(displayLTSE)[1]]="TOTAL"
	colnames(displayLTSE)[dim(displayLTSE)[2]]="TOTAL"
	displayLTSE
#StratSEtot and TotalSEtot are the same for the Age Total SE sheet

#Age-related sheets

#age by set sheet
	adjabundanceatlengthbyset=tapply(lfdata$cage,list(lfdata$strataset,lfdata$flen),sum)
	adjabundanceatlengthbyset[is.na(adjabundanceatlengthbyset)]=0
	adjabundanceatagebyset=matrix(0,dim(adjabundanceatlengthbyset)[1],length(agevec))
	for(s in 1:dim(adjabundanceatagebyset)[1])adjabundanceatagebyset[s,]=as.numeric(colSums(LbS[s,]*propage))
	AFbyset=cbind.data.frame("STRATA SLAT SLONG AREA CRUISE SET"=rownames(LbS),adjabundanceatagebyset)
	AFbyset=cbind(AFbyset,TOTAL=rowSums(adjabundanceatagebyset))
	AFbyset

#Age Mean sheet; mean number at age by strata, not bumped by trawlable units
	adjabundanceatagebystrata=matrix(0,length(stratavec),length(agevec))
	for(s in 1:dim(adjabundanceatagebystrata)[1])adjabundanceatagebystrata[s,]=colSums(LMsheet[s,]*propage)
	colnames(adjabundanceatagebystrata)=agevec
	rownames(adjabundanceatagebystrata)=stratavec
	stratabunbyagevec=rep(NA,length(agevec))
	for(c in 1:dim(adjabundanceatagebystrata)[2])stratabunbyagevec[c]=weighted.mean(adjabundanceatagebystrata[,c],tunitsvec)
	displayAM=rbind(adjabundanceatagebystrata,stratabunbyagevec)
	displayAM=cbind(displayAM,c(rowSums(adjabundanceatagebystrata),sum(stratabunbyagevec)))
	#The TOTAL label for the bottom row might be misleading, as the bottom row is the stratified mean
	rownames(displayAM)[dim(displayAM)[1]]="TOTAL"
	colnames(displayAM)[dim(displayAM)[2]]="TOTAL"
	displayAM


#Age Mean SE sheet
	AFbysetstrat=substr(AFbyset[,1],1,3)
	numericset=as.numeric(substr(AFbyset[,1],28,30))
	strataset=paste(substr(AFbyset[,1],18,27),AFbysetstrat,numericset,sep="")
	AMSD=matrix(0,length(stratavec),length(agevec))
	for(a in 1:length(agevec))AMSD[,a]=tapply(AFbyset[,a+1],list(AFbysetstrat),sd)
	AMSE=AMSD/sqrt(setvec)
	#need detailed set-specific equivalent to lfdata for numbers at age
	totrecs=sum((adjabundanceatagebyset/adjabundanceatagebyset),na.rm=T)
	setrecs=rowSums((adjabundanceatagebyset/adjabundanceatagebyset),na.rm=T)
	#number records is number of age records (totrecs) plus number of void sets
	voidsets=dim(AFbyset[AFbyset$TOTAL==0.0,])[1]
	fillna=rep(NA,(totrecs+voidsets))
	afdata=data.frame(strat=fillna, tunits=fillna,strataset=fillna,sets=fillna,age=fillna,fage=fillna,sage=fillna)
	i=0
	is=0
	for(t in 1:length(setvec)) {
		for(s in 1:setvec[t]) {
			is=is+1
			if(setrecs[is]==0) {
			i=i+1
			afdata[i,]=c(AFbysetstrat[is],tunitsvec[t],strataset[is],setvec[t],"NA","NA","NA")
			next
			}
			for(r in 1:length(agevec)) {
				i=i+1
				if(adjabundanceatagebyset[is,r]<=0.0) {
					i=i-1
					next
				}
				afdata[i,]=c(AFbysetstrat[is],tunitsvec[t],strataset[is],setvec[t],agevec[r],adjabundanceatagebyset[is,r],adjabundanceatagebyset[is,r]*tunitsvec[t])
			}
		}
	}
	fillzero=rep(0,length(agevec)*sum(setvec))
	AFSD=data.frame(strat=fillzero,tunits=fillzero,age=fillzero,set=fillzero,meannum=fillzero,stratnum=fillzero)
	inc=0
	for(s in 1:length(stratavec)) {
		for(l in 1:length(agevec)) {
			ns=setvec[s]
			for(nss in 1:ns) {
				inc=inc+1
				AFSD$strat[inc]=stratavec[s]
				AFSD$tunits[inc]=tunitsvec[s]
				AFSD$sets[inc]=setvec[s]
				AFSD$age[inc]=agevec[l]
				curset=names(setmat[as.numeric(rownames(setmat))==stratavec[s],!is.na(setmat[as.numeric(rownames(setmat))==stratavec[s],])])[nss]
				curset=substr(curset,4,nchar(curset))
				AFSD$set[inc]=curset
				seekvalue=afdata$fage[afdata$strat==stratavec[s] & afdata$strataset==curset & afdata$age==agevec[l]]
				if(length(seekvalue)>0) {
					if(!is.na(seekvalue))AFSD$meannum[inc]=seekvalue
				}
			}
		}
	}
	AFSD$meannum[AFSD$meannum=="NA"]=0
	AFSD$meannum=as.numeric(AFSD$meannum)
	yyy=tapply(AFSD$meannum,list(AFSD$strat,AFSD$age),sd)
	AMSE=yyy/sqrt(setvec)
	stratabunbyageerrmat=matrix(NA,length(stratavec),length(agevec))
	for(s in 1:length(stratavec)) {
		for(c in 1:length(agevec))stratabunbyageerrmat[s,c]=var(AFSD$meannum[AFSD$strat==stratavec[s] & AFSD$age==agevec[c]])
	}
	StratAgeSE=rep(NA,length(agevec))
	for(c in 1:length(agevec))StratAgeSE[c]=sqrt(sum((((tunitsvec * (tunitsvec - setvec))/sum(tunitsvec)^2.) * stratabunbyageerrmat[,c])/setvec))
	NumAgeByStratSet=aggregate(AFSD$meannum, by=list(strat=AFSD$strat,tunits=AFSD$tunits,set=AFSD$set,sets=AFSD$sets),sum)
	StratSEAge=rep(NA,length(stratavec))
	for(s in 1:length(stratavec)) {
		StratSEAge[s]=sqrt(var(NumAgeByStratSet$x[NumAgeByStratSet$strat==stratavec[s]]))/sqrt(setvec[s])
	}
	StratVarAge=rep(NA,length(stratavec))
	for(s in 1:length(stratavec)) {
		StratVarAge[s]=var(NumAgeByStratSet$x[NumAgeByStratSet$strat==stratavec[s]])
	}
	TotalSEAge=sqrt(sum((((tunitsvec * (tunitsvec - setvec))/sum(tunitsvec)^2.) * StratVarAge)/setvec))
	displayAMSE=rbind(AMSE,StratAgeSE)
	displayAMSE=cbind(displayAMSE,c(StratSEAge,TotalSEAge))
	rownames(displayAMSE)[dim(displayAMSE)[1]]="TOTAL"
	colnames(displayAMSE)[dim(displayAMSE)[2]]="TOTAL"
	displayAMSE
#StratSEAge and TotalSEAge are the same for the Length SE sheet

#Age Total sheet; total number at age, bumped by trawlable units
	afdata$sage=as.numeric(afdata$sage)
	afdata$sets=as.numeric(afdata$sets)
	ATsheet=tapply(afdata$sage/afdata$sets,list(afdata$strat,afdata$age),sum)
	ATsheet[is.na(ATsheet)]=0
	displayAT=rbind(ATsheet,colSums(ATsheet))
	displayAT=cbind(displayAT,rowSums(displayAT))
	rownames(displayAT)[dim(displayAT)[1]]="TOTAL"
	colnames(displayAT)[dim(displayAT)[2]]="TOTAL"
	displayAT

#Age Total SE sheet
	fillzero=rep(0,length(agevec)*sum(setvec))
	afdata$fage=as.numeric(afdata$fage)
	afdata$fage[is.na(afdata$fage)]=0
	afdata$strat=as.numeric(afdata$strat)
	afdata$age=as.numeric(afdata$age)
	afdata$tunits=as.numeric(afdata$tunits)
	AFSDtot=data.frame(strat=fillzero,tunits=fillzero,age=fillzero,set=fillzero,meannum=fillzero,stratnum=fillzero)
	inc=0
	for(s in 1:length(stratavec)) {
		for(l in 1:length(agevec)) {
			ns=setvec[s]
			for(nss in 1:ns) {
				inc=inc+1
				AFSDtot$strat[inc]=stratavec[s]
				AFSDtot$tunits[inc]=tunitsvec[s]
				AFSDtot$sets[inc]=setvec[s]
				AFSDtot$age[inc]=agevec[l]
				curset=names(setmat[as.numeric(rownames(setmat))==stratavec[s],!is.na(setmat[as.numeric(rownames(setmat))==stratavec[s],])])[nss]
				curset=substr(curset,4,nchar(curset))
				AFSDtot$set[inc]=curset
				seekvalue=afdata$fage[afdata$strat==stratavec[s] & afdata$age==agevec[l] & afdata$strataset==curset]*afdata$tunits[afdata$strat==stratavec[s] & afdata$age==agevec[l] & afdata$strataset==curset]
				if(length(seekvalue)>0) {
					if(!is.na(seekvalue))AFSDtot$meannum[inc]=seekvalue
				}
			}
		}
	}
	AFSDtot$meannum[AFSD$AFSDtot=="NA"]=0
	AFSDtot$meannum=as.numeric(AFSDtot$meannum)
	yyy=tapply(AFSDtot$meannum,list(AFSDtot$strat,AFSDtot$age),sd)
	ATSE=yyy/sqrt(setvec)
	stratabunbyagetoterrmat=matrix(NA,length(stratavec),length(agevec))
	for(s in 1:length(stratavec)) {
		for(c in 1:length(agevec))stratabunbyagetoterrmat[s,c]=var(AFSDtot$meannum[AFSDtot$strat==stratavec[s] & AFSDtot$age==agevec[c]])
	}
	StratAgeSEtot=rep(NA,length(agevec))
	for(c in 1:length(agevec))StratAgeSEtot[c]=sqrt(sum(stratabunbyagetoterrmat[,c]/setvec))
	NumAgeByStratSetTot=aggregate(AFSDtot$meannum, by=list(strat=AFSDtot$strat,tunits=AFSDtot$tunits,set=AFSDtot$set,sets=AFSDtot$sets),sum)
	StratSEtotAge=rep(NA,length(stratavec))
	for(s in 1:length(stratavec)) {
		StratSEtotAge[s]=sqrt(var(NumAgeByStratSetTot$x[NumAgeByStratSetTot$strat==stratavec[s]]))/sqrt(setvec[s])
	}
	StratVarTotAge=rep(NA,length(stratavec))
	for(s in 1:length(stratavec)) {
		StratVarTotAge[s]=var(NumAgeByStratSetTot$x[NumAgeByStratSetTot$strat==stratavec[s]])
	}
	TotalSEtotAge=sqrt(sum(StratVarTotAge/setvec))
	displayATSE=rbind(ATSE,StratAgeSEtot)
	displayATSE=cbind(displayATSE,c(StratSEtotAge,TotalSEtotAge))
	rownames(displayATSE)[dim(displayATSE)[1]]="TOTAL"
	colnames(displayATSE)[dim(displayATSE)[2]]="TOTAL"
	displayATSE
	#StratSEtotAge and TotalSEtotAge are the same for the Length Total SE sheet

#Weight sheets
	dsquery="SELECT * FROM fowler.setwgt"
	channel<-odbcConnect(uid=UserID,pw=PassWord,dsn=UserDSN,case='nochange',rows_at_time=1)
	setwgt<-sqlQuery(channel,dsquery)
	names(setwgt) = c("strat","slat","slong","area","mission","setno","sets","tunits","areanm","setwt") 
	setwgt$setwt[is.na(setwgt$setwt)]=0
#display set weights table
	setwgt[,c(1:6,10)]
#Weight Mean
	WM=tapply(setwgt$setwt/setwgt$sets,list(setwgt$strat),sum)
	xxx=cbind(STRATA=stratavec,TOTAL=WM)
	displayWM=rbind(xxx,c("TOTAL",weighted.mean(WM,tunitsvec)))
	displayWM
#Weight Mean SE
	WMSE=tapply(setwgt$setwt,list(setwgt$strat),sd)/sqrt(setvec)
	WMvar=tapply(setwgt$setwt,list(setwgt$strat),var)
	StratWSE=sqrt(sum((((tunitsvec * (tunitsvec - setvec))/sum(tunitsvec)^2.) * WMvar)/setvec))
	displayWMSE=rbind(cbind(STRATA=stratavec,TOTAL=WMSE),c("TOTAL",StratWSE))
	displayWMSE
#stratified weight by strata
	WMtot=tapply(setwgt$setwt*setwgt$tunits/setwgt$sets,list(setwgt$strat),sum)
	displayWMtot=rbind(cbind(STRATA=stratavec,TOTAL=WMtot),c("TOTAL",sum(WMtot)))
	displayWMtot
#stratified weight SE
	WMSEtot=tapply(setwgt$setwt*setwgt$tunits,list(setwgt$strat),sd)/sqrt(setvec)
	WMtotvar=tapply(setwgt$setwt*setwgt$tunits,list(setwgt$strat),var)
	WMgrandtotSE=sqrt(sum(WMtotvar/setvec))
	displaySWMSE=rbind(cbind(STRATA=stratavec,TOTAL=WMSEtot),c("TOTAL",WMgrandtotSE))
	displaySWMSE

#Area sheets
#proportion by area sheet
	setwgt$gotone=0
	setwgt$gotone[setwgt$setwt>0]=1
	areaprop=tapply(setwgt$gotone/setwgt$sets,list(setwgt$strat),sum)
	displayArea=rbind(cbind(STRATA=stratavec,TOTAL=areaprop),c("TOTAL",weighted.mean(areaprop,tunitsvec)))
	displayArea
#proportion by area se sheet
	areapropse=tapply(setwgt$gotone,list(setwgt$strat),sd)/sqrt(setvec)
	areapropvar=tapply(setwgt$gotone,list(setwgt$strat),var)
	StratAreaSE=sqrt(sum((((tunitsvec * (tunitsvec - setvec))/sum(tunitsvec)^2.) * areapropvar)/setvec))
	displayAreaSE=rbind(cbind(STRATA=stratavec,TOTAL=areapropse),c("TOTAL",StratAreaSE))
	displayAreaSE

#proportion of strata in nm^2 with fish
	stratarea=tapply(setwgt$areanm*setwgt$gotone,list(setwgt$strat),mean)
	totarea=sum(stratarea)
	displayTotArea=rbind(cbind(STRATA=stratavec,TOTAL=stratarea),c("TOTAL",totarea))
	displayTotArea
#SE of proportion of strata in nm^2 with fish
	stratarease=tapply(setwgt$areanm*setwgt$gotone,list(setwgt$strat),sd)/sqrt(setvec)
	areapropvarnm=tapply(setwgt$gotone*setwgt$areanm,list(setwgt$strat),var)
	areaproptotse=sqrt(sum(areapropvarnm/setvec))
	displayTotAreaSE=rbind(cbind(STRATA=stratavec,TOTAL=stratarease),c("TOTAL",areaproptotse))
	displayTotAreaSE

###DONE###

#scripting for downloading the core table if desired.
#We do not actually have to download this table, as we do STRANAL by extracting pieces of it in various ways.
	dsquery="SELECT * FROM fowler.stock_agelen where year=1974 and to_number(strat)>=470 and to_number(strat)<=495"
	channel<-odbcConnect(uid=UserID,pw=PassWord,dsn=UserDSN,case='nochange',rows_at_time=1)
	stock<-sqlQuery(channel,dsquery)
	names(stock) = c("series","mission","vessel","year","cruno", "setno", "strat", "slat", "slong", 
		"depth", "dmin", "dmax", "loctime", 
		"sdepth", "temp", "sal", "dist","tunits","sampwgt","rawtotwgt","sizeclass",
		"fsex","bysex","binwidth","flen","age","fwt","cage")

