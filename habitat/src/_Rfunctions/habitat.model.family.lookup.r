  habitat.model.family.lookup = function( vname ) {
    
    fmly = gaussian()  # default

    if ( vname %in% c("smr", "zn", "zm", "qn", "qm", "Pr.reaction", "Ea", "A" ) ) {
      fmly = gaussian() 
    }
    if ( vname %in% c("Npred" ) ) {
      fmly = gaussian("log") 
    }
     

    return (fmly)
  }


