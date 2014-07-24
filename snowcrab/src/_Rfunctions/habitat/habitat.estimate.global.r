## incomplete 
 
habitat.estimate.global = function( DS="gam" ) {
   
#loadlibraries( "mgcv", "chron", "lattice"  ) 
#loadfunctions( "snowcrab", functionname="initialise.local.environment.r") 

 
  # estimate a global surface that is potentially habitat
  outdir = project.directory( "snowcrab", "R", "gam", "habitat.global" )
  dir.create(path=outdir, recursive=T, showWarnings=F)
   
  if ( DS %in% c("gam", "gam.redo")  ) {
    outfile = file.paste( outdir, "habitat.surface.gam.rdata" )
    if (DS=="gam") {
      PS = NULL
      if (file.exists( outfile) ) load (outfile)
      return (PS)
    }

    set = habitat.model.db( DS="basedata", p=p, v=v )
 

  }

}
