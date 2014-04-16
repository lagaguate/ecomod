
# Example 1: p_i= 1-exp(-k*effort)

 
# GouldPollockML(ncatch=test.data$ncatch,effort=test.data$effort,Var.est=T,)
#$Nhat:
#[1] 475485.6

#$k.opt:
#[1] 0.003801499

#$n.pred:
# [1] 75064.41 55034.64 31412.66 25291.10 18245.05 25170.39 21531.22 43895.99 52316.39 42229.37 31165.85
#[12] 30935.00 23193.58

#$mean.NHAT.sim:
#[1] 475548.2

#$SE.NHAT.sim:
#[1] 921.1146


#Code for example 1:

GouldPollockML = function(ncatch,effort,Var.est=F,reps=500)
{
	data.ML = data.frame(ncatch=ncatch,effort=effort)
	Xsp1 = sum(ncatch)
#	min.obj = ms(~GouldPollockML.prog(k,ncatch,effort),data=data.ML,start=list(k=0.0001))
	min.obj = optim(fn=~GouldPollockML.prog(k,ncatch,effort), data=data.ML, par=list(k=0.0001))
	k.opt = as.numeric(min.obj$parameters)
	pp = ppI = rep(0,sncatch = length(ncatch))
		for(i in 1:sncatch){
			ppI[i] = pp[i] = 1-exp(-k.opt*effort[i])		
			}
		    for(i in 2:sncatch){
			for(j in 1:(i-1)){
				pp[i] = pp[i]*(1-ppI[j])
			}}			
		PP = sum(pp)
		pp = pp/PP
		out = list(Nhat=Xsp1/PP,k.opt=k.opt,n.pred=pp*Xsp1/PP)
		if(Var.est){
			par.boot = rep(NA,reps)
			data.sim = data.frame(ncatchsim=out$n.pred,effort=effort)
			pp.sim = pp
		for(i in 1:reps){
			
			data.sim = data.frame(ncatchsim=rmultinom(Xsp1,pp.sim),effort=effort)

		temp.k = as.numeric(ms(~GouldPollockML.prog(k,ncatchsim,effort),data=data.sim,start=list(k=0.0001))$parameters)
    	pp = ppI = rep(0,sncatch = length(ncatch))
		for(i in 1:sncatch){
			ppI[i] = pp[i] = 1-exp(-temp.k*effort[i])		
			}
		    for(i in 2:sncatch){
			for(j in 1:(i-1)){
				pp[i] = pp[i]*(1-ppI[j])
			}}			
		PP = sum(pp)
		pp = pp/PP
		par.boot[i] = Xsp1/PP
		  }
		out$mean.NHAT.sim = mean(par.boot)
		   out$SE.NHAT.sim = sqrt(var(par.boot))

		}
		out
}


GouldPollockML.prog = function(k,ncatch,effort)
	{
		pp = ppI = rep(0,sncatch = length(ncatch))
		for(i in 1:sncatch){
			ppI[i] = pp[i] = 1-exp(-k*effort[i])
		}
		    for(i in 2:sncatch){
			for(j in 1:(i-1)){
				pp[i] = pp[i]*(1-ppI[j])				
			}}
			PP = sum(pp)
			pp = pp/PP
				out = 0
			
			 out = -sum(ncatch*log(pp))
	
				out
	}

		# Generate single random Multinomial(n,pr)
rmultinom = function(n,pr, long=F) {
  k_length(pr)
  if (abs(1-sum(pr)) > 0.000001)
   stop("rmultinom: parameter pr must be the k probabilities (summing to 1)")

  if(long) {
    y_runif(n, 0, 1)
    p_cumsum(pr)
    Seq_1:n
    x_sapply(y, function(y, Seq, p) {Seq[y <= p][1]}, Seq=Seq, p=p)
  } else {
    x_rep(NA,k)
    p_pr/c(1,(1-cumsum(pr[1:(k-1)])))
    for (i in 1:(k-1)) {
      if (n==0) {
        x[i]_0
        if (i==k-1) x[k]_0
        next
      }
      y_rbinom(1,n,p[i])
      x[i]_y
      if (i==k-1) x[k]_n-y
      n_n-y
    }
  }
  return(x)
}

---------------------------

# Example 2: logit example with effort only


#> GouldPollockML.logit(ncatch=test.data$ncatch,effort=test.data$effort)
#$Nhat:
#[1] 472263.2

#$k.opt:
#    alpha       beta 
# 3.293645 0.03701984

#$n.pred:
# [1] 53986.51 39287.42 24894.80 21334.74 17696.73 19997.48 17773.83 30935.40 41090.16 32180.38 22398.45
#[12] 23509.03 16715.07



# ----
# Code for example 2

GouldPollockML.logit = function(ncatch,effort)
{   # still have to add variance estimation and ability to add
    # more covariates.
	data.ML = data.frame(ncatch=ncatch,effort=effort)
	Xsp1 = sum(ncatch)
	
   min.obj = ms(~GouldPollockML.log.prog(alpha,beta,ncatch,effort),data=data.ML,start=list(alpha=3,beta=0.001))

	k.opt = as.numeric(min.obj$parameters)
	pp = ppI = rep(0,sncatch = length(ncatch))
	
		for(i in 1:sncatch){
			ppI[i] = pp[i] = exp(-k.opt[1]+k.opt[2]*effort[i])/(1+exp(-k.opt[1]+k.opt[2]*effort[i]))
		}
		    for(i in 2:sncatch){
			for(j in 1:(i-1)){
				pp[i] = pp[i]*(1-ppI[j])
			}}
		PP = sum(pp)
		list(Nhat=Xsp1/PP,k.opt=min.obj$parameters,n.pred=pp*Xsp1/PP)
	
}

GouldPollockML.log.prog = function(alpha,beta,ncatch,effort)
	{
		pp = ppI = rep(0,sncatch = length(ncatch))
		
		for(i in 1:sncatch){
			ppI[i] = pp[i] = exp(-alpha+beta*effort[i])/(1+exp(-alpha+beta*effort[i]))
		}
		    for(i in 2:sncatch){
			for(j in 1:(i-1)){
				pp[i] = pp[i]*(1-ppI[j])
			}}
			PP = sum(pp)
			pp = pp/PP
			out = 0
			out = -sum(ncatch*log(pp))
				out
	}


