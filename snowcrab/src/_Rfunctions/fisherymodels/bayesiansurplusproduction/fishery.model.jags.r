
  fishery.model.jags = function( DS="" ) {
    out = NULL
    
    if (DS=="biomass.dynamic" ) {
      out = file.path( project.directory("snowcrab"), "src", "bugs", "biomassdynamic.bugs" )    
    }
    if (DS=="biomass.dynamic.illegal") {
      out = file.path( project.directory("snowcrab"), "src", "bugs", "biomassdynamic.illegal.bugs" )    
    }
    if (DS=="delay.difference") {
      out = file.path( project.directory("snowcrab"), "src", "bugs", "delaydifference.bugs" )   
    }
    if (DS=="delay.difference.illegal") {
      out = file.path( project.directory("snowcrab"), "src", "bugs", "delaydifference.illegal.bugs" )   
    }

    return(out)
  }



