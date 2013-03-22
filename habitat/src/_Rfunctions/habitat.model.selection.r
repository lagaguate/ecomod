habitat.model.selection = function( ww, modeltype, indicator="default" ) {
  
  if (indicator %in% c( "default", "metabolism", "speciesarea", "speciesomposition", "sizespectrum" ) ) {

    formu = switch(  modeltype,
      
      simple = paste( 
        ' ~ as.factor(yr) + s(plon,plat, by=as.factor(yr) ) + s(julian, k=3) ' 
      ),

      simple.highdef = paste(
        ' ~ as.factor(yr) + s(plon,plat, k=400, by=as.factor(yr) ) + s(julian,k=3)' 
      ),

      time.invariant = paste(
        ' ~ as.factor(yr) + s(plon,plat, k=400, by=as.factor(yr) ) + s(julian, k=3) + s(z, k=4 , bs="ts" ) ', 
        ' + s(dZ, k=4, bs="ts" ) + s(substrate.mean, k=4, bs="tp" ) ' 
      ),

      complex = paste( 
        ' ~ as.factor(yr) + s(plon,plat, by=as.factor(yr), k=100, bs="ts"  ) + s(julian ) + s(t, k=3 , bs="ts" ) ',
        ' + s(tmean, k=3, bs="ts") + s(tamp, k=3, bs="ts" ) + s(wmin, k=3 , bs="ts" )',
        ' + s(z, k=3 , bs="ts" ) + s(dZ, k=3, bs="ts" ) + s(substrate.mean, k=3, bs="ts" ) ' 
      ),

      full = paste( 
        ' ~ as.factor(yr) + s(plon,plat, k=400, by=as.factor(yr) ) + s(julian ) + s(tmean, k=3, bs="ts")',
        ' + s(dt.annual, k=3, bs="ts" ) + s(dt.seasonal, k=3, bs="ts" ) ', 
        ' + s(tamp, k=3, bs="ts" ) + s(wmin, k=3 , bs="ts" ) + s(z, k=3 , bs="ts" )', 
        ' + s(dZ, k=3, bs="ts" ) + s(substrate.mean, k=3, bs="ts" ) ' 
      )
    )
  }


  out = formula ( paste( ww, formu ) )
  return(out)
}


