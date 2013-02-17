

habitat.model.lookup = function( modeltype ) {
  gam.model.pa = NULL
  if ( modeltype=="complete") {
    gam.model.pa = formula( presence ~  s( yr ) + s(month, k=3) 
        + s( dt.seasonal ) + s( dt.annual ) + s( tmean ) + s( tamp) +  s( wmin )
        + s( plon, plat, k=400) + s( z ) + s( substrate.mean) + s( dZ ) + s(ddZ) 
        + s( Npred) + s(smr)  + s(ca1) +s(ca2) 
      )
  }

  if ( modeltype=="testing") {
    gam.model.pa = formula( presence ~  s( yr ) + s(month, k=3) 
        + s( dt.seasonal ) + s( dt.annual ) + s( tmean ) + s( tamp) +  s( wmin)
        + s( plon, plat, k=400) + s( z ) + s( substrate.mean) + s( dZ ) + s(ddZ) 
        + s( Npred) + s(smr)  + s(ca1) +s(ca2) 
      )
  }

  if (modeltype=="simple") {
    gam.model.pa = formula( presence ~  s( yr ) + s(month, k=3) 
        + s( dt.seasonal ) + s( dt.annual ) + s( tmean) + s( tamp) +  s( wmin)
        + s( plon, plat, k=400) + s( z ) + s( substrate.mean) + s( dZ ) + s(ddZ) 
        + s( Npred) + s(smr)  + s(ca1) +s(ca2) 
      )
  }

  return ( gam.model.pa )
}

