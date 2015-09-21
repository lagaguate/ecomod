
  spacetime.inla.call = function( FM, DATA, SPDE ) {
    #\\ call inla using spacetime expected data objects: 
    #\\   FM is the model formula
    #\\   DATA is the "stacked" data
    #\\   SPDE is the SPDE spatial covariance data object (Matern)
    #\\ NOTE, SPDE needs to be sent, even though it does not SEEM to be required by the call to 'inla'
    #\\   but it is still required ... don't ask why  :(
    inputstack = inla.stack.data(DATA)
    A = inla.stack.A(DATA) 

    itmpdir=tempfile()
    
    RES = try( inla( FM, data = inputstack, 
          control.compute=list(dic=TRUE, config=TRUE), # return linear predictors to compute predictions quickly
          # control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
          control.predictor=list (A=A, compute=TRUE ), # compute=TRUE on each data location 
          control.inla = list(h =1e-2), # h=0.01 is default step length for gradient calc of hyper params 
          working.directory= itmpdir, keep=FALSE, 
          verbose=FALSE
      ), silent=TRUE )
 
    # clean up local files
      fns = list.files( dirname( itmpdir), all.files=TRUE, full.names=TRUE, recursive=TRUE, include.dirs=TRUE )  
      oo = grep( basename(itmpdir), fns )
      if(length(oo)>0) file.remove( sort(fns[oo], decreasing=TRUE) )

      if ("try-error" %in% class(RES)) {
        RES = try( inla( FM, data = inputstack, 
          control.compute=list(dic=TRUE, config = TRUE), 
          # control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
          control.predictor=list (A=A, compute=TRUE ), 
          control.inla = list(h = 1e-3, tolerance=1e-8, restart=3), # restart a few times in case posteriors are poorly defined
          working.directory=itmpdir, keep=FALSE, 
          verbose=FALSE
        ), silent=TRUE )
           
        # clean up local files
      
        fns = list.files( dirname( itmpdir), all.files=TRUE, full.names=TRUE, recursive=TRUE, include.dirs=TRUE )  
        oo = grep( basename(itmpdir), fns )
        if(length(oo)>0) file.remove( sort(fns[oo], decreasing=TRUE) )

      }

      if ("try-error" %in% class(RES)) return( NULL ) # give up 
      
      if ( RES$mode$mode.status > 0) {  # make sure Eignevalues of Hessian are appropriate (>0)
        RES = try( inla( FM, data = inputstack, 
          control.compute=list(dic=TRUE, config = TRUE), 
          #control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
          control.predictor=list (A=A, compute=TRUE ), 
          control.inla = list( h=1e-4, tolerance=1e-10), # increase in case values are too close to zero 
          control.mode = list( restart=TRUE, result=RES ), # restart from previous estimates
          working.directory=itmpdir, keep=FALSE, 
          verbose=FALSE
        ), silent=TRUE )
            # clean up local files
        
        fns = list.files( dirname( itmpdir), all.files=TRUE, full.names=TRUE, recursive=TRUE, include.dirs=TRUE )  
        oo = grep( basename(itmpdir), fns )
        if(length(oo)>0) file.remove( sort(fns[oo], decreasing=TRUE) )

      }

      if ("try-error" %in% class(RES)) return (NULL) 
     
      if ( RES$mode$mode.status > 0) {  # make sure Eignevalues of Hessian are appropriate (>0)
         RES = try( inla( FM, data = inputstack, 
          control.compute=list(dic=TRUE, config = TRUE), 
#          control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
          control.predictor=list (A=A, compute=TRUE ), 
          control.inla = list( h=1e-6, tolerance=1e-12), # increase in case values are too close to zero 
          control.mode = list( restart=TRUE, result=RES ), # restart from previous estimates
          working.directory=itmpdir, keep=FALSE, 
          verbose=FALSE
        ), silent=TRUE )
          # clean up local files
          
        fns = list.files( dirname( itmpdir), all.files=TRUE, full.names=TRUE, recursive=TRUE, include.dirs=TRUE )  
        oo = grep( basename(itmpdir), fns )
        if(length(oo)>0) file.remove( sort(fns[oo], decreasing=TRUE) )

       }

      # if still hessian problems accept the solution .. it should be close enough
      if ("try-error" %in% class(RES)) RES=NULL
      return( RES )
  }


