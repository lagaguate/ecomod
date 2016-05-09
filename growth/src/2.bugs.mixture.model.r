#model originally from T. Surrette DFO Gulf
  require(rjags)
  rjags::load.module("dic")
  rjags::load.module("glm")

  
  loadfunctions( "bayesian" )

  n.adapt = 4000 # burn-in  .. 4000 is enough for the full model but in case ...
  n.iter = 3000 
  n.chains = 3
  n.thin = 100 # use of uniform distributions causes high autocorrelations ? 
  n.iter.final = n.iter * n.thin
  fnres = file.path( project.datadirectory("growth"), "R", paste( "example.mixture", "rdata", sep=".") )

 



data   <- list(
   N = 1074,  # Total number of lobsters measured.
   n = 121,   # Number of length categories.
   
   # 
   x =  c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,
					8.5,9,9.5,10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,
					15,15.5,16,16.5,17,17.5,18,18.5,19,19.5,20,20.5,
					21,21.5,22,22.5,23,23.5,24,24.5,25,25.5,26,26.5,
					27,27.5,28,28.5,29,29.5,30,30.5,31,31.5,32,32.5,
					33,33.5,34,34.5,35,35.5,36,36.5,37,37.5,38,38.5,
					39,39.5,40,40.5,41,41.5,42,42.5,43,43.5,44,44.5,
					45,45.5,46,46.5,47,47.5,48,48.5,49,49.5,50,50.5,
					51,51.5,52,52.5,53,53.5,54,54.5,55,55.5,56,56.5,
					57,57.5,58,58.5,59,59.5,60),
   
   y = c(0,0,0,0,0,0,0,1,2,0,8,12,13,10,18,33,20,16,6,
					16,13,11,9,5,7,2,4,6,7,7,7,10,13,7,12,14,20,20,
					21,21,26,12,34,30,21,25,29,30,24,24,32,18,20,22,
					20,18,21,19,11,11,14,14,12,7,19,15,12,6,9,7,3,8,
					13,8,5,10,10,7,1,5,7,6,7,4,6,3,5,5,7,0,4,1,2,0,
					1,2,3,2,3,0,0,0,1,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,
					0,0,0)
)


  m = jags.model( file=file.path(project.codedirectory('growth'),'src','bugs','mixtureLN.bugs'), data=data, n.chains=n.chains, n.adapt=n.adapt ) # recruitment + spring/summer q's + all observed CVs


  tomonitor =  c( 'pi','P','mu','sigma','theta','xi','yoy','mu.yoy')
  tomonitor = intersect( variable.names (m), tomonitor )
  coef(m)
  

  # ----------------

  dic.samples(m, n.iter=n.iter ) # pDIC

  
  # ----------------
  y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior
 
