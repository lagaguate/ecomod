
  model.pde.parameters = function( p=NULL ) {
    
    if (is.null(p)) p = list()
  
    p$eps  = 1e-6   # A in units of t/km^2 -- number below which abundance can be considered zero ~ 1 kg/ km^2 = 1g / m^2
    p$atol = 1e-9  # atol -- absolute error tolerance for lsoda
    p$rtol = 1e-9  # rtol -- relative error tolerance for lsoda

    p$habitatthreshold = 0.05

    h = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="habitat.mean" ) 
    p$inothabitat = which( h < p$habitatthreshold )
    
    # Reaction-related parameters

    if ( any( grep( "reaction.r", p$parmeterizations, ignore.case=TRUE)) ) {

        r.l = 0.2
        r.u = 1.5
        r.m = 1
    
        r = matrix( nrow=p$nr, ncol=p$nc, data=0 )
    
        if ( "reaction.r.constant"  %in% p$parmeterizations ) {
          r[] = r.m
        }

        if ( "reaction.r.random.uniform"  %in% p$parmeterizations ) {
          r[] = runif( p$nrc, min=r.l, max=r.u ) 
        }

        if ( "reaction.r.random.normal"  %in% p$parmeterizations ) {
          r[] = rnorm( p$nrc, mean=r.m, sd=r.m/10) 
        }
       
        if ( "reaction.r.snowcrab.r1"  %in% p$parmeterizations ) {
          r1 = model.pde.external.db( p=p, method="snowcrab.male.R1", variable="abundance.mean" ) 
          r = r / max(r1) * r.u
          print( "should develop a long-term mean here or alternate proxy")
        }
      
        r[ which( r <= p$eps) ] = p$eps
        r[ p$inothabitat ] = p$eps
        iifin = which (!is.finite( r) ) 
        if (length(iifin)>0) r[iifin] = p$eps

        p$r = r

    }


    # ------
     
    
    if ( any( grep( "reaction.K", p$parmeterizations, ignore.case=TRUE) ) ) {
      
      # carrying capacity 

      K = matrix( nrow=p$nr, ncol=p$nc, data=0 )
      
      A = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" ) 
      Kq =  quantile( A, probs=c(0.01, 0.5, 0.99),na.rm=TRUE )

        if ( "reaction.K.constant"  %in% p$parmeterizations ) {
          K[] = K[3]
        }

        if ( "reaction.K.random.uniform"  %in% p$parmeterizations ) {
          K[] = runif( p$nrc, min=Kq[1], max=Kq[3] ) 
        }

        if ( "reaction.K.random.normal"  %in% p$parmeterizations ) {
          K[] = rnorm( p$nrc, mean=Kq[2], sd=Kq[3]/10) 
        }
       
        if ( "reaction.K.snowcrab.mature"  %in% p$parmeterizations ) {
          K = A
          print( "should develop a long-term mean here or alternate proxy")
        }

        K[ which( K <= p$eps) ] = p$eps

        iifin = which (!is.finite( K) ) 
        if (length(iifin)>0) K[iifin] = p$eps
        
        K[ p$inothabitat ] = p$eps

        p$K = K
    }


    # Diffusion related parameters
           
    if ( any( grep( "diffusion", p$parmeterizations, ignore.case=TRUE) ) ) {
   
      estimate.Da = FALSE
        if (estimate.Da) {
          # magnitudes from ~ 50 to 2 km^2/day ~ time interval, etc...
          estimate.bulk.diffusion.coefficient( DS="snowcrab" ) 
        }

        Da.l = 0.1
        Da.m = 10
        Da.u = 100
  
        Da = matrix( ncol=p$nc, nrow=p$nr, data=0 ) 

        if ( "diffusion.constant.median" %in% p$parmeterizations ) {
          Da[] = Da.m
        }
    
        if ( "diffusion.constant.min" %in% p$parmeterizations ) {
          Da[] = Da.l
        }
   
        if ( "diffusion.constant.max" %in% p$parmeterizations ) {
          Da[] = Da.u
        }
   
        if ( "diffusion.random.uniform"  %in% p$parmeterizations ) {
         Da[] = runif( p$nrc, min=Da.l, max=Da.u ) 
        }

        if ( "diffusion.random.normal"  %in% p$parmeterizations ) {
          Da[] = rnorm( p$nrc, mean=Da.m, sd=Da.m/10 )
        }

        if ( "diffusion.snowcrab.mature"  %in% p$parmeterizations ) {
          # Da =  ... ? 
        
        }

        if ( "diffusion.temperature.based"  %in% p$parmeterizations ) {
          # Da =  ... ? 
        
        }
        
        debug = FALSE
        if (debug) {
      
          # Boundaries have no diffusion .. dim can be n-1 but keeping the smae shape for simplicity  
          Da[c(1,p$nr),] = 0
          Da[,c(1,p$nc)] = 0
          
          Da[ which( Da <= p$eps) ] = 0

          iifin = which (!is.finite( Da ) ) 
          if (length(iifin)>0) Da [iifin] = 0

          Da[ p$inothabitat ] = 0
         
        } else {
          
          # Boundaries have no diffusion .. dim can be n-1 but keeping the smae shape for simplicity  
          Da[c(1,p$nr),] = p$eps^2
          Da[,c(1,p$nc)] = p$eps^2
          
          Da[ which( Da <= p$eps) ] = p$eps^p$eps^2

          iifin = which (!is.finite( Da ) ) 
          if (length(iifin)>0) Da [iifin] = p$eps^2

          Da[ p$inothabitat ] = p$eps^2
          
        }
        
      p$Da = Da

    }

    if ( any( grep( "advection", p$parmeterizations, ignore.case=TRUE) ) ) {
       # km/day -- bulk average instantaneous (short-term) velocity
       
        Va.l = 0.1
        Va.m = 1
        Va.u = 10
  
        Va = matrix( ncol=p$nc, nrow=p$nr, data=0 ) 

        if ( "advection.constant.median" %in% p$parmeterizations ) {
          Va[] = Va.m
        }
    
        if ( "advection.constant.min" %in% p$parmeterizations ) {
          Va[] = Va.l
        }
   
        if ( "advection.constant.max" %in% p$parmeterizations ) {
          Va[] = Va.u
        }
   
        if ( "advection.random.uniform"  %in% p$parmeterizations ) {
          Va[] = runif( p$nrc, min=-Va.u, max=Va.u ) 
        }

        if ( "advection.random.normal"  %in% p$parmeterizations ) {
          Va[] = rnorm( p$nrc, mean=0, sd=Va.m/5 )
        }

        if ( "advection.snowcrab.mature"  %in% p$parmeterizations ) {
          Va = model.pde.external.db( p=pars, method="habitat", variable="habitat.mean"  )
        }
 
        if ( "advection.temperature.based" %in% p$parmeterizations ) {   
          Va = model.pde.external.db( p=pars, method="habitat", variable="t" )
          # .... other manipulations to make it work required here ...

        }
       
        # Boundaries have no (minimal) advection 
        Va[c(1,p$nr),] = p$eps^2
        Va[,c(1,p$nc)] = p$eps^2

        iifin = which (!is.finite( Va ) ) 
        if (length(iifin)>0) Va [iifin] = p$eps^2

        Va[ p$inothabitat ] = p$eps^2
        
        p$Va = Va
         
    }
    
    return(p)
    
  }



