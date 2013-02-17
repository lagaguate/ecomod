
    model.formula = function( V ) {
      
      # basic model ..  everything with no tweaking
      out = formula( Y ~  as.factor(yr) + s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s(wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" )  + s(substrate.mean, k=4, bs="ts" )             
            + s(ca1, k=4, bs="ts" ) + s(ca2, k=4, bs="ts" ) 
            + s(Npred, k=4 , bs="ts") + s(Z, k=4, bs="ts" ) + s(smr, k=4, bs="ts" ) + s(mr, k=4, bs="ts" ) 
            + s(plon, plat, k=200, bs="tp" )  ) 
          
			if ( V=="simple" ) {
        # simple model in case of failures in other attempts
        out = formula( Y ~  as.factor(yr) + s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(z) + s(dZ, k=4, bs="ts" ) + s(substrate.mean, k=4, bs="ts" )             
            + s(plon, plat, k=250, bs="tp") ) 
      }
      
			if ( V=="R0.mass" ) {
        # enough data for a high spatial resolution of data variability
        out = formula( Y ~  as.factor(yr) + s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s( wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" )  + s(substrate.mean, k=4, bs="ts" )             
            + s(ca1, k=4, bs="ts" ) + s(ca2, k=4, bs="ts" ) 
            + s(Npred, k=4 , bs="ts") + s(Z, k=4, bs="ts" ) + s(smr, k=4, bs="ts" ) + s(mr, k=4, bs="ts" ) 
            + s(plon, plat, k=250, bs="tp") ) 
      }
     
      if ( V=="R0.mass.old" ) {
        # enough data for a high spatial resolution of data variability
        out = formula( Y ~ s(yr) + s(weekno, k=4) + s( t-t.annual ) + s(tmean) + s(t.annual-tmean) + s( tamp)+ s( wmin) 
            + s( plon, plat, k=400 ) + s( z ) + s(substrate.mean ) + s(ddZ) + s(dZ) ) 
      }

      if ( V=="R0.mass.2011" ) {
        # enough data for a high spatial resolution of data variability
        out = formula( Y ~  s(yr) + s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s( wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" )  + s(substrate.mean, k=4, bs="ts" )             
            + s(ca1, k=4, bs="ts" ) + s(ca2, k=4, bs="ts" ) 
            + s(Npred, k=4 , bs="ts") + s(Z, k=4, bs="ts" ) + s(smr, k=4, bs="ts" ) + s(mr, k=4, bs="ts" ) 
            + s(plon, plat, k=250, bs="tp") ) 
      }
    
      if ( V=="R0.mass.environmentals.only" ) {
        # enough data for a high spatial resolution of data variability
        out = formula( Y ~ s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s( wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" ) + s(ddZ, k=4, bs="ts" )   + s(substrate.mean, k=4, bs="ts" )             
            + s(plon, plat, k=250, bs="tp") ) 
      }


    
      if ( V=="R0.mass.2009" ) {
        # enough data for a high spatial resolution of data variability
        out = formula( Y ~ s(yr, bs="ts" ) + s(weekno, bs="ts" ) 
            + s( dt.seasonal, bs="ts" ) + s(tmean, bs="ts" ) + s(dt.annual, bs="ts" ) 
            + s( tamp, bs="ts" ) + s( wmin, bs="ts"  ) 
            + s( z , bs="ts" ) + s(substrate.mean, bs="ts" ) + s(ddZ, bs="ts" ) + s(dZ, bs="ts"  ) 
            + s( plon, plat, bs="ts", k=200 )  ) 
      }
      return (out)
    }



