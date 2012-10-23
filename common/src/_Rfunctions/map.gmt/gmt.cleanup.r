
  gmt.cleanup = function( locations=tmpdir, pattern="^\\.gmt.*\\.ps$", time.filter=10 ) {
  #gmt.cleanup = function( locations=tmpdir, pattern="^.gmt.*.ps$", time.filter=10 ) {
    files.to.delete = list.files( path=tmpdir, pattern=pattern, all.files=T, 
      full.names=T, recursive=F)
    dtime = which( 
        difftime( Sys.time(), file.info(files.to.delete)$mtime, units="mins") > time.filter 
    ) # "completed files"
    remove.files ( files.to.delete[dtime] ) 
    return(files.to.delete[dtime])
  }


