  
  # estimate metabolic demand, given size structure

	loadlibraries ( c("chron", "fields", "mgcv", "sp")) 

  ### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db" 


  # ----->  ALSO, uses the lookup function ,,, 
  #   i.e., be careful with dependency order as metabolism will 
  #   eventually need a lookup method too !!!


  p = list()
  p$init.files = loadfunctions( c(
	  "common", "habitat", "bathymetry", "bio", "temperature", "taxonomy", "metabolism"
	) )
  
	p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$taxa = "alltaxa"   # do not use any other category
  p$season = "allseasons"
  p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 )  
  
  # choose:
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep( "localhost", 2 )
   p$clusters = rep( "localhost", 4 )
  # p$clusters = rep( "localhost", 8 )
  # p$clusters = rep( "localhost", 24 )
  # p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("kaos", 24 ) )
  # p$clusters = c( rep( "kaos.beowulf", 6), rep("nyx.beowulf", 24))
  # p$clusters = c( rep("tartarus.beowulf", 24), rep("kaos", 17 ) )

  p$varstomodel = c( "mr", "smr", "Pr.Reaction" , "Ea", "A", "zn", "zm", "qn", "qm", "mass", "len"  )
    # p$varstomodel = c( "mr", "smr", "Pr.Reaction" , "Ea", "A" )
    # p$varstomodel = c( "zn", "zm", "qn", "qm", "mass", "len"  )
  
  p$yearstomodel = 1970:2013
  p$habitat.predict.time.julian = "Sept-1" # Sept 1
 
  p$spatial.knots = 100


  # p$mods = c("simple","simple.highdef", "complex", "full" )  # model types to attempt
  # p$mods = c("simple","simple.highdef" )  # model types to attempt
  p$mods = "complex.no.years"


  # prepare data
  metabolism.db( DS="metabolism.redo", p=p )
   
  
  # model the data ~ 14GB/ variable
  p = make.list( list(vars= p$varstomodel, modtype=p$mods, years=p$yearstomodel), Y=p ) 
  metabolism.model( p=p, DS="redo" ) 
  # RAM requirements are large ... single processing only 
  # parallel.run( clusters=p$clusters[1:p$nruns], n=p$nruns, metabolism.model, p=p, DS="redo" ) 


  # predict data: gridded extrapolations to full domain  
  # ~ 5 GB / process
  p$clusters = c( rep( "nyx.beowulf", 12), rep("tartarus.beowulf", 12), rep("kaos", 12 ) )
  p$clusters =  "localhost"
  
  p = make.list( list( yrs=p$yearstomodel, modtype=p$mods), Y=p )
  # parallel.run( clusters=p$clusters, n=p$nruns, metabolism.interpolate, p=p, DS="redo" ) 
  metabolism.interpolate( p=p, DS="redo" ) 
  



  # map everything
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel, modtype=p$mods), Y=p )
  # parallel.run( clusters=p$clusters, n=p$nruns, metabolism.map, p=p, type="annual"  ) 
  metabolism.map ( p=p, type="annual" )


summary(models)

Family: gaussian 
Link function: log 

Formula:
mr ~ as.factor(yr) + s(plon, plat, by = as.factor(yr), k = 100, 
    bs = "ts") + s(julian) + s(t, k = 3, bs = "ts") + s(tmean, 
    k = 3, bs = "ts") + s(tamp, k = 3, bs = "ts") + s(wmin, k = 3, 
    bs = "ts") + s(z, k = 3, bs = "ts") + s(dZ, k = 3, bs = "ts") + 
    s(substrate.mean, k = 3, bs = "ts")
<environment: 0x66f3678>

Parametric coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)        2.29958    0.11289  20.370  < 2e-16 ***
as.factor(yr)1971 -0.53381    0.18898  -2.825 0.004738 ** 
as.factor(yr)1972 -0.68386    0.14752  -4.636 3.59e-06 ***
as.factor(yr)1973  0.21167    0.17201   1.231 0.218487    
as.factor(yr)1974  0.05689    0.14450   0.394 0.693806    
as.factor(yr)1975  0.27487    0.18120   1.517 0.129309    
as.factor(yr)1976  0.06387    0.15057   0.424 0.671420    
as.factor(yr)1977  0.19560    0.15199   1.287 0.198141    
as.factor(yr)1978 -0.45015    0.28275  -1.592 0.111401    
as.factor(yr)1979 -0.28295    0.13694  -2.066 0.038824 *  
as.factor(yr)1980 -0.18587    0.12973  -1.433 0.151973    
as.factor(yr)1981 -0.11699    0.13788  -0.848 0.396193    
as.factor(yr)1982 -0.12248    0.22003  -0.557 0.577772    
as.factor(yr)1983 -0.34729    0.19409  -1.789 0.073590 .  
as.factor(yr)1984  0.12391    0.12334   1.005 0.315111    
as.factor(yr)1985  0.05107    0.14715   0.347 0.728560    
as.factor(yr)1986 -0.37265    0.13401  -2.781 0.005430 ** 
as.factor(yr)1987 -0.60364    0.14203  -4.250 2.15e-05 ***
as.factor(yr)1988 -0.28012    0.14179  -1.976 0.048224 *  
as.factor(yr)1989 -0.40833    0.14151  -2.886 0.003913 ** 
as.factor(yr)1990 -0.50427    0.16615  -3.035 0.002409 ** 
as.factor(yr)1991 -0.61089    0.15981  -3.822 0.000133 ***
as.factor(yr)1992 -1.07317    0.17773  -6.038 1.59e-09 ***
as.factor(yr)1993 -0.64326    0.16886  -3.809 0.000140 ***
as.factor(yr)1994 -0.96843    0.20752  -4.667 3.09e-06 ***
as.factor(yr)1995 -0.90505    0.16715  -5.414 6.23e-08 ***
as.factor(yr)1996 -0.34003    0.13855  -2.454 0.014127 *  
as.factor(yr)1997 -0.24071    0.12477  -1.929 0.053729 .  
as.factor(yr)1998 -0.33673    0.13859  -2.430 0.015124 *  
as.factor(yr)1999 -0.33542    0.13069  -2.567 0.010279 *  
as.factor(yr)2000 -0.41803    0.12719  -3.287 0.001016 ** 
as.factor(yr)2001 -0.43031    0.12834  -3.353 0.000802 ***
as.factor(yr)2002 -0.47717    0.12575  -3.795 0.000148 ***
as.factor(yr)2003 -0.70550    0.13287  -5.310 1.11e-07 ***
as.factor(yr)2004 -0.88787    0.15258  -5.819 6.03e-09 ***
as.factor(yr)2005 -0.73648    0.12468  -5.907 3.55e-09 ***
as.factor(yr)2006 -0.41944    0.12663  -3.312 0.000927 ***
as.factor(yr)2007 -0.29275    0.12698  -2.305 0.021152 *  
as.factor(yr)2008 -0.33291    0.12942  -2.572 0.010110 *  
as.factor(yr)2009 -0.57585    0.15816  -3.641 0.000272 ***
as.factor(yr)2010 -0.22516    0.12700  -1.773 0.076272 .  
as.factor(yr)2011 -0.35380    0.12764  -2.772 0.005581 ** 
as.factor(yr)2012 -0.36381    0.12688  -2.867 0.004144 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Approximate significance of smooth terms:
                                   edf Ref.df       F  p-value    
s(plon,plat):as.factor(yr)1970 26.1299 99.000   0.726 9.93e-08 ***
s(plon,plat):as.factor(yr)1971 22.8552 99.000   0.916 9.69e-13 ***
s(plon,plat):as.factor(yr)1972  3.4478 99.000   0.114 0.008231 ** 
s(plon,plat):as.factor(yr)1973 32.5150 99.000   1.184 1.77e-14 ***
s(plon,plat):as.factor(yr)1974 16.9527 99.000   0.349 0.001571 ** 
s(plon,plat):as.factor(yr)1975 38.3179 99.000   1.060 5.94e-10 ***
s(plon,plat):as.factor(yr)1976  9.7839 99.000   0.125 0.201356    
s(plon,plat):as.factor(yr)1977 36.0859 99.000   1.126 5.03e-12 ***
s(plon,plat):as.factor(yr)1978 76.8937 99.000   2.922  < 2e-16 ***
s(plon,plat):as.factor(yr)1979 37.6527 99.000   1.078 7.59e-11 ***
s(plon,plat):as.factor(yr)1980 52.7128 99.000   2.595  < 2e-16 ***
s(plon,plat):as.factor(yr)1981 53.1122 99.000   2.172  < 2e-16 ***
s(plon,plat):as.factor(yr)1982 76.2655 99.000   2.335  < 2e-16 ***
s(plon,plat):as.factor(yr)1983 77.7296 99.000   1.652 7.30e-09 ***
s(plon,plat):as.factor(yr)1984 47.8821 99.000   1.274 2.59e-11 ***
s(plon,plat):as.factor(yr)1985 25.1666 99.000   0.518 0.000229 ***
s(plon,plat):as.factor(yr)1986  7.0942 99.000   0.134 0.036226 *  
s(plon,plat):as.factor(yr)1987 24.3202 99.000   0.457 0.001283 ** 
s(plon,plat):as.factor(yr)1988 36.7720 99.000   1.041 2.08e-10 ***
s(plon,plat):as.factor(yr)1989 12.1173 99.000   0.531 5.54e-09 ***
s(plon,plat):as.factor(yr)1990 38.5545 99.000   1.569  < 2e-16 ***
s(plon,plat):as.factor(yr)1991 12.1637 99.000   0.294 0.000714 ***
s(plon,plat):as.factor(yr)1992 16.8114 99.000   0.678 2.58e-10 ***
s(plon,plat):as.factor(yr)1993 18.8004 99.000   0.631 1.61e-08 ***
s(plon,plat):as.factor(yr)1994 31.6518 99.000   1.466  < 2e-16 ***
s(plon,plat):as.factor(yr)1995 30.4932 99.000   1.301  < 2e-16 ***
s(plon,plat):as.factor(yr)1996 37.1622 99.000   1.173 8.47e-13 ***
s(plon,plat):as.factor(yr)1997 44.9275 99.000   2.376  < 2e-16 ***
s(plon,plat):as.factor(yr)1998 48.3236 99.000   2.595  < 2e-16 ***
s(plon,plat):as.factor(yr)1999 76.4635 99.000   4.523  < 2e-16 ***
s(plon,plat):as.factor(yr)2000 59.1527 99.000   2.965  < 2e-16 ***
s(plon,plat):as.factor(yr)2001 55.1635 99.000   4.460  < 2e-16 ***
s(plon,plat):as.factor(yr)2002 63.1172 99.000   3.488  < 2e-16 ***
s(plon,plat):as.factor(yr)2003 47.8817 99.000   2.838  < 2e-16 ***
s(plon,plat):as.factor(yr)2004 67.5941 99.000   4.098  < 2e-16 ***
s(plon,plat):as.factor(yr)2005 69.9216 99.000   2.851  < 2e-16 ***
s(plon,plat):as.factor(yr)2006 73.4027 99.000   2.813  < 2e-16 ***
s(plon,plat):as.factor(yr)2007 71.4119 99.000   3.397  < 2e-16 ***
s(plon,plat):as.factor(yr)2008 71.8592 99.000   5.501  < 2e-16 ***
s(plon,plat):as.factor(yr)2009 84.4896 99.000   4.013  < 2e-16 ***
s(plon,plat):as.factor(yr)2010 60.6700 99.000   3.840  < 2e-16 ***
s(plon,plat):as.factor(yr)2011 79.8225 99.000   3.582  < 2e-16 ***
s(plon,plat):as.factor(yr)2012 69.4563 99.000   3.986  < 2e-16 ***
s(julian)                       8.6474  8.936 141.234  < 2e-16 ***
s(t)                            1.1059  2.000   3.779 0.004173 ** 
s(tmean)                        1.9314  2.000  18.639 5.69e-10 ***
s(tamp)                         1.4716  2.000  29.354  < 2e-16 ***
s(wmin)                         0.5113  2.000   0.000 1.000000    
s(z)                            1.9941  2.000 160.646  < 2e-16 ***
s(dZ)                           1.9099  2.000 115.705  < 2e-16 ***
s(substrate.mean)               1.9920  2.000  18.460 1.24e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

R-sq.(adj) =  0.411   Deviance explained = 47.4%
GCV score = 53.809  Scale est. = 48.005    n = 18597


