xkill = xk = function( d=NULL ) {
  if (is.null(device)) {
    while ((which <- dev.cur()) != 1) dev.off(which)
  } else if (device==0 ) {
    dev.off( dev.cur )
  } else {
    try( dev.off( device ) )
    dev.list() 
  }
}

