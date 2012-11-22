

lookup.biological.filter = function( ss, additional.data=NULL ) {
  
  # a list of sex, maturity, codes, etc  to define det subsets
  # sex codes defined in bio.db.r ( female=1, male=0, ?=2 )
  # mat codes defined in bio.db.r ( imm=0, mat=1, ?=2 )

  out = list()
  
  if (ss == "len") {
    out$len = additional.data
  }

  if (ss == "mass") {
    out$mass = additional.data
  }

  if (ss == "sex") {
    out$sex = additional.data
  }

  if (ss == "male") {
    out$sex = 0
  }

  if (ss == "female") {
    out$sex = 1
  }


  if (ss == "mat") {
    out$mat = additional.data
  }

  if (ss == "mature") {
    out$mat = 1
  }

  if (ss == "immature") {
    out$mat = 0
  }

  if (ss == "female.mature" ) {
    out$sex = 1 
    out$mat = 1 
  }

  if (ss == "female.immature" ) {
    out$sex = 1 
    out$mat = 0 
  }

  if (ss == "male.mature" ) {
    out$sex = 0 
    out$mat = 1 
  }

  if (ss == "male.immature" ) {
    out$sex = 0 
    out$mat = 0 
  }

 
  # snow crab specific -- size based

  if (ss == "snowcrab.female.small" ) {
    out$sex = 1 
    out$len = c( 0, 44.9 ) 
  }

  if (ss == "snowcrab.female.large" ) {
    out$sex = 1 
    out$len = c( 45, 120 ) 
  }

  if (ss == "snowcrab.male.small" ) {
    out$sex = 2 
    out$len = c( 0, 44.9 ) 
  }
 
  if (ss == "snowcrab.male.legal" ) {
    out$sex = 2 
    out$len = c( 45, 175 ) 
  }

  if (ss == "snowcrab.male.legal" ) {
    out$sex = 2 
    out$len = c( 95, 175 ) 
  }



  return (out)

}


