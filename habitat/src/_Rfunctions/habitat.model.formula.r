

habitat.model.formula = function( YY="presence", modeltype="complex", indicator="default", spatial.knots=100, additional.terms=NULL ) {
   
  formu = NULL
  
  if (indicator %in% c( "default", "metabolism", "speciesarea", "speciescomposition", "condition", "sizespectrum" ) ) { 
    
    formu = switch(  modeltype,
      
      debug = paste( 
        ' as.factor(yr) + s(plon,plat, by=as.factor(yr) )' 
      ),

      simple = paste( 
        ' s(plon,plat )  ' 
      ),

      simple.highdef = paste(
        ' as.factor(yr) + s(plon,plat, k=spatial.knots, by=as.factor(yr) ) + s(dyear,k=3)' 
      ),

      time.invariant = paste(
        ' as.factor(yr) + s(plon,plat, k=spatial.knots, by=as.factor(yr) ) + s(dyear, k=3) + s(z, k=4 , bs="ts" ) ', 
        ' + s(dZ, k=4, bs="ts" ) + s(substrate.mean, k=4, bs="tp" ) ' 
      ),

      complex = paste( 
        ' as.factor(yr) + s(plon,plat, by=as.factor(yr), k=spatial.knots, bs="tp"  ) + s(dyear, k=3, bs="tp") + s(t, bs="tp" ) ',
        ' + s(tmean, bs="tp") + s(tamp, bs="tp" ) + s(wmin, bs="tp" )',
        ' + s(z, bs="tp" ) + s(dZ, bs="tp" ) + s(substrate.mean, bs="tp" ) ' 
      ),
   
      complete = paste( 
        ' as.factor(yr) + s(plon,plat, k=spatial.knots, by=as.factor(yr) ) + s(dyear ) + s(tmean, k=3, bs="ts")',
        ' + s(dt.annual, k=3, bs="ts" ) + s(dt.seasonal, k=3, bs="ts" ) ', 
        ' + s(tamp, k=3, bs="ts" ) + s(wmin, k=3 , bs="ts" ) + s(z, k=3 , bs="ts" )', 
        ' + s(dZ, k=3, bs="ts" ) + s(substrate.mean, k=3, bs="ts" ) ' 
      ), # same as full

      full = paste( 
        ' as.factor(yr) + s(plon,plat, k=spatial.knots, by=as.factor(yr) ) + s(dyear ) + s(tmean, k=3, bs="ts")',
        ' + s(dt.annual, k=3, bs="ts" ) + s(dt.seasonal, k=3, bs="ts" ) ', 
        ' + s(tamp, k=3, bs="ts" ) + s(wmin, k=3 , bs="ts" ) + s(z, k=3 , bs="ts" )', 
        ' + s(dZ, k=3, bs="ts" ) + s(substrate.mean, k=3, bs="ts" ) ' 
      )
    )
  }
  
  if ( !is.null(additional.terms)) formu = paste( formu, '+', additional.terms )

  out = formula ( paste( YY, "~", formu ) )
  
  return(out)

}


