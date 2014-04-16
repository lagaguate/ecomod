# winbugs code (?)
#


model

{

#		k~dunif(0,1)
			k~dbeta(1,1)
		for(i in 1:sncatch){
			ppI[i]<-1-exp(-k*effort[i])
			ppI2[i]<-1-ppI[i]
			}
	
			pp[1]<-ppI[1]
			for(j in 2:sncatch){
 		 pp[j]<-ppI[j]*prod(ppI2[1:(j-1)])
			}
			
			PP<-sum(pp[])
			
			for (i in 1:sncatch){
			 np[i]<-pp[i]/PP
			}
			
		
			ncatch[1:sncatch]~dmulti(np[],Xsp1)
	
		
			Xsp1~dbin(PP,Npop)
		
			N.trial~dunif(Xsp1,8000)
			Npop<-round(N.trial)
			Npopscale<-100*Npop
			
			
		

}
 


Bayesian example from WinBugs:

Example 1: p_=1-exp(-k*effort)

Bayesian version of Gould and Pollock catch-effort model (1997).  Simple case with both likelihoods

model

{

#		Prior
		k~dunif(0,1)

		for(i in 1:sncatch){
			ppI[i]<-1-exp(-k*effort[i])
			}
	#Need to automate this somehow --- node reassignment errors when for loop used.
			pp[1]<-ppI[1]
			pp[2]<-ppI[2]*(1-ppI[1])
			pp[3]<-ppI[3]*(1-ppI[2])*(1-ppI[1])
			pp[4]<-ppI[4]*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[5]<-ppI[5]*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[6]<-ppI[6]*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[7]<-ppI[7]*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[8]<-ppI[8]*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[9]<-ppI[9]*(1-ppI[8])*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[10]<-ppI[10]*(1-ppI[9])*(1-ppI[8])*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[11]<-ppI[11]*(1-ppI[10])*(1-ppI[9])*(1-ppI[8])*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[12]<-ppI[12]*(1-ppI[11])*(1-ppI[10])*(1-ppI[9])*(1-ppI[8])*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[13]<-ppI[13]*(1-ppI[12])*(1-ppI[11])*(1-ppI[10])*(1-ppI[9])*(1-ppI[8])*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
	
			
			PP<-sum(pp[])
			
			for (i in 1:sncatch){
			 np[i]<-pp[i]/PP
			}
			
#		        Conditional likelihood (L2)
			ncatch[1:sncatch]~dmulti(np[],Xsp1)
	
#		        Likelihood (L1)
		
			Xsp1~dbin(PP,Npop)
		
			N.trial~dunif(Xsp1,8000)
			Npop<-round(N.trial)
			Npopscale<-100*Npop
			
#                         Catches were scaled by 100.			
		

}

data

list(sncatch=13,Xsp1=3618, effort = c(33.664, 27.743, 17.254, 14.764, 11.19, 16.263, 14.757, 32.922, 45.519, 43.523, 37.478, 43.367, 37.96), 
ncatch = c(604, 495, 282, 207, 119, 156, 132, 254, 299, 325, 247, 276, 222))


init

list(k=0.01,N.trial=4000)

list(k=0.00001,N.trial=7200)

Results:

Run on WinBugs14 at work.  
 	 node	 mean	 sd	 MC error	2.5%	median	97.5%	start	sample
	Npop	4758.0	99.23	3.122	4578.0	4754.0	4964.0	5001	11000
	Npopscale	475800.0	9923.0	312.2	457800.0	475400.0	496400.0	5001	11000
	PP	0.7607	0.01451	4.607E-4	0.7312	0.761	0.7876	5001	11000
	k	0.003804	1.609E-4	5.107E-6	0.00349	0.003803	0.004116	5001	11000

Dbar = post.mean of -2logL; Dhat = -2LogL at post.mean of stochastic nodes
	Dbar	Dhat	pD	DIC	
Xsp1	9.593	8.618	0.976	 10.569	
ncatch	192.535	191.542	0.993	193.527	
total	         202.128       200.160	  1.968	      204.096	



Example 2: logit model (effort only)


Bayesian version of Gould and Pollock catch-effort model (1997). logit case.

model

{

	alpha~dnorm(0,0.0001)
	beta~dnorm(0,0.0001)

		for(i in 1:sncatch){
			logit(ppI[i])<-alpha+beta*effort[i]
			}
	
			pp[1]<-ppI[1]
			pp[2]<-ppI[2]*(1-ppI[1])
			pp[3]<-ppI[3]*(1-ppI[2])*(1-ppI[1])
			pp[4]<-ppI[4]*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[5]<-ppI[5]*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[6]<-ppI[6]*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[7]<-ppI[7]*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[8]<-ppI[8]*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[9]<-ppI[9]*(1-ppI[8])*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[10]<-ppI[10]*(1-ppI[9])*(1-ppI[8])*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[11]<-ppI[11]*(1-ppI[10])*(1-ppI[9])*(1-ppI[8])*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[12]<-ppI[12]*(1-ppI[11])*(1-ppI[10])*(1-ppI[9])*(1-ppI[8])*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
			pp[13]<-ppI[13]*(1-ppI[12])*(1-ppI[11])*(1-ppI[10])*(1-ppI[9])*(1-ppI[8])*(1-ppI[7])*(1-ppI[6])*(1-ppI[5])*(1-ppI[4])*(1-ppI[3])*(1-ppI[2])*(1-ppI[1])
	
			
			PP<-sum(pp[])
			
			for (i in 1:sncatch){
			 np[i]<-pp[i]/PP
			}
			
		
			ncatch[1:sncatch]~dmulti(np[],Xsp1)
	
		 Xsp1~dbin(PP,Npop)
		
			N.trial~dunif(Xsp1,8000)
			Npop<-round(N.trial)
			
		

}

data

list(sncatch=13,Xsp1=3618, effort = c(33.664, 27.743, 17.254, 14.764, 11.19, 16.263, 14.757, 32.922, 45.519, 43.523, 37.478, 43.367, 37.96), ncatch = c(604, 495, 282, 207, 119, 156, 132, 254, 299, 325, 247, 276, 222))


init

list(alpha=1,beta=0.1,N.trial=4000)

list(alpha=5,beta=0.001,N.Trial=6000)

Results:

WinBugs14 version. 	 
	                   node	 mean	 sd	 MC error	2.5%	median	97.5%	start	sample
(need to rescale)	Npop	4730.0	110.1	4.746	4530.0	4722.0	4961.0	5001	11000
	                   PP	0.7653	0.01667	7.48E-4	0.7309	0.7661	0.7966	5001	11000
		alpha	-3.296	0.05257	0.00304	-3.402	-3.296	-3.197	5001	11000
		beta	0.03704	0.001782	1.14E-4	0.03353	0.03707	0.04076	5001	11000
	
	Dbar = post.mean of -2logL; Dhat = -2LogL at post.mean of stochastic nodes
	Dbar	Dhat	pD	DIC	
Xsp1	9.592	8.591	1.001	10.593	
ncatch	238.381	236.454	1.926	240.307	
total	        247.973        245.046	  2.927	     250.900



