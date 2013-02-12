habitat.model.selection = function( ww, modeltype, indicator="default" ) {
  
  if (indicator %in% c( "default", "metabolism", "speciesarea", "speciesomposition", "sizespectrum" ) ) {

    formu = switch(  modeltype,
      
      simple = paste( 
        ' ~ s(plon,plat) + as.factor(yr) + s(julian, k=3) ' 
      ),

      simple.highdef = paste(
        ' ~ s(plon,plat, k=400) + as.factor(yr) + s(julian,k=3)' 
      ),

      time.invariant = paste(
        ' ~ s(plon,plat, k=400) + as.factor(yr) + s(julian, k=3) + s(z, k=4 , bs="ts" ) ', 
        ' + s(dZ, k=4, bs="ts" ) + s(substrate.mean, k=4, bs="ts" ) ' 
      ),

      complex = paste( 
        ' ~ s(plon,plat, k=400) + s(julian ) + as.factor(yr) + s(t, k=3 , bs="ts" ) ',
        ' + s(tmean, k=3, bs="ts") + s(tamp, k=3, bs="ts" ) + s(wmin, k=3 , bs="ts" )',
        ' + s(z, k=3 , bs="ts" ) + s(dZ, k=3, bs="ts" ) + s(substrate.mean, k=3, bs="ts" ) ' 
      ),

      full = paste( 
        ' ~ s(plon,plat, k=400) + as.factor(yr) + s(julian ) + s(tmean, k=3, bs="ts")',
        ' + s(dt.annual, k=3, bs="ts" ) + s(dt.seasonal, k=3, bs="ts" ) ', 
        ' + s(tamp.annual, k=3, bs="ts" ) + s(wmin.annual, k=3 , bs="ts" ) + s(z, k=3 , bs="ts" )', 
        ' + s(dZ, k=3, bs="ts" ) + s(substrate.mean, k=3, bs="ts" ) ' 
      )
    )
  }


  out = formula ( paste( ww, formu ) )
  return(out)
}


