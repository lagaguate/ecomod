
    model.formula = function( V="default", variant="" ) {
      
      if ( V %in% c("default") ) {
        if ( variant=="") {
          # basic model -- less spatial density .. all covariates
          out = formula( Y ~ s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s(wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" )  + s(substrate.mean, k=4, bs="ts" )             
            + s(ca1, k=4, bs="ts" ) + s(ca2, k=4, bs="ts" ) 
            + s(Npred, k=4 , bs="ts") + s(Z, k=4, bs="ts" ) + s(smr, k=4, bs="ts" ) + s(mr, k=4, bs="ts" ) 
            + s(plon, plat, k=100, bs="tp", by=as.factor(yr) ) + as.factor(yr)  ) 
        }
        if (variant=="no.years") {
          # basic model -- less spatial density and no years 
          out = formula( Y ~ s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s(wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" )  + s(substrate.mean, k=4, bs="ts" )             
            + s(ca1, k=4, bs="ts" ) + s(ca2, k=4, bs="ts" ) 
            + s(Npred, k=4 , bs="ts") + s(Z, k=4, bs="ts" ) + s(smr, k=4, bs="ts" ) + s(mr, k=4, bs="ts" ) 
            + s(plon, plat, k=100, bs="tp" )   ) 
        }
      }

      if ( V %in% c("default.simple") ) {
        if ( variant=="") {
          # basic model -- less spatial density 
          out = formula( Y ~ s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s(wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" )  + s(substrate.mean, k=4, bs="ts" )             
            + s(plon, plat, k=100, bs="tp", by=as.factor(yr) ) + as.factor(yr)  ) 
        }
        if (variant=="no.years") {
          # basic model -- less spatial density and no years 
          out = formula( Y ~ s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s(wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" )  + s(substrate.mean, k=4, bs="ts" )             
            + s(plon, plat, k=100, bs="tp" )   ) 
        }
      }

      
      if ( V %in% c("R0.no", "R0.mass", "totmass.com") ) {
        #  same as basic model but with higher spatial densities
        if ( variant=="") {
          out = formula( Y ~ s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s(wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" )  + s(substrate.mean, k=4, bs="ts" )             
            + s(ca1, k=4, bs="ts" ) + s(ca2, k=4, bs="ts" ) 
            + s(Npred, k=4 , bs="ts") + s(Z, k=4, bs="ts" ) + s(smr, k=4, bs="ts" ) + s(mr, k=4, bs="ts" ) 
            + s(plon, plat, k=200, bs="tp", by=as.factor(yr) ) + as.factor(yr)  ) 
        }
        if (variant=="no.years") {
          out = formula( Y ~ s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s(wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" )  + s(substrate.mean, k=4, bs="ts" )             
            + s(ca1, k=4, bs="ts" ) + s(ca2, k=4, bs="ts" ) 
            + s(Npred, k=4 , bs="ts") + s(Z, k=4, bs="ts" ) + s(smr, k=4, bs="ts" ) + s(mr, k=4, bs="ts" ) 
            + s(plon, plat, k=100, bs="tp" ) ) 
        }
      }
      

      if ( V=="all" ) {
        out = formula( Y ~ s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s(wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" )  + s(substrate.mean, k=4, bs="ts" )             
            + s(ca1, k=4, bs="ts" ) + s(ca2, k=4, bs="ts" ) 
            + s(Npred, k=4 , bs="ts") + s(Z, k=4, bs="ts" ) + s(smr, k=4, bs="ts" ) + s(mr, k=4, bs="ts" ) 
            + s(plon, plat, k=200, bs="tp", by=as.factor(yr) ) + as.factor(yr)  ) 
      }

			if ( V=="simple" ) {
        # simple model in case of failures in other attempts
        out = formula( Y ~  s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(z) + s(dZ, k=4, bs="ts" ) + s(substrate.mean, k=4, bs="ts" )             
            + s(plon, plat, bs="tp", by=as.factor(yr)) + as.factor(yr) ) 
      }
			
      if ( V=="simple" & variant=="no.years" ) {
        # simple model in case of failures in other attempts
        out = formula( Y ~  s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(z) + s(dZ, k=4, bs="ts" ) + s(substrate.mean, k=4, bs="ts" )             
            + s(plon, plat, bs="tp") ) 
      }

			if ( V=="R0.mass.2012" ) {
        # enough data for a high spatial resolution of data variability
        out = formula( Y ~ s(weekno, k=4, bs="ts" ) 
            + s(tmean) + s(dt.annual, k=4, bs="ts" ) + s(dt.seasonal, k=4, bs="ts" ) 
            + s(tamp, k=4, bs="ts" )+ s( wmin, k=4 , bs="ts" ) 
            + s(z) + s(dZ, k=4, bs="ts" )  + s(substrate.mean, k=4, bs="ts" )             
            + s(ca1, k=4, bs="ts" ) + s(ca2, k=4, bs="ts" ) 
            + s(Npred, k=4 , bs="ts") + s(Z, k=4, bs="ts" ) + s(smr, k=4, bs="ts" ) + s(mr, k=4, bs="ts" ) 
            + s(plon, plat, bs="ts", k=100, by=as.factor(yr)) + as.factor(yr) ) 
      }
     
      if ( V=="R0.mass.old" ) {
        # enough data for a high spatial resolution of data variability
        out = formula( Y ~  s(weekno, k=4) + s( t-t.annual ) + s(tmean) + s(t.annual-tmean) + s( tamp)+ s( wmin) 
            + s( plon, plat, k=400, by=as.factor(yr) ) + s( z ) + s(substrate.mean ) + s(ddZ) + s(dZ) ) 
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
            + s(plon, plat, k=250, bs="tp", by=as.factor(yr)) + as.factor(yr) ) 
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



