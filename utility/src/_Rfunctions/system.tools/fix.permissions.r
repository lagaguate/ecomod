  fix.permissions = function( loc=ecomod.datadirectory, file.perm="ug+rw,o+r", dir.perm=" ug+rwX,o+rX" ) {
    #\\ File permissions can get messed up easily in a shared environment
    #\\ make permissions sensisble recursively inside directory "loc"
    if ( tolower( Sys.info()$sysname ) != "linux" ) stop( "This is for unix systems only" )
    system ( paste( "find", file.path(loc, ""), 
      "\\( -type f -exec chmod", file.perm, " {} \\; \\) ,",
      "\\( -type d -exec chmod", dir.perm, " {} \\; \\) " 
    ))
  }



