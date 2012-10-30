
	loadfunctions = function( projectname, directorypattern=NULL, functionname=NULL, keydirectories=c("r", "\\.r", "\\_r", "rfunctions", "\\.rfunctions", "\\_rfunctions" ), filepattern="\\.r$" ) {
	
    # used to load local functions conveniently
    # sequence slightly important ... modify with care
    # the  '\\' used in the above are escape sequences used with regex
    
    fs = .Platform$file.sep
    keydirectories = paste( "\\", fs, keydirectories, "\\", fs, sep="") # make sure only match directories and not file names

    filestosource = NULL

    for (pn in projectname) {

      projectdirectory = project.directory( name=pn )

      for (searchdirectory in c( file.path( projectdirectory, "src" ), projectdirectory ) ) {  # first try in "src" and then the project if not found in first pass  

        projectfiles = NULL
        projectfiles = list.files( path=searchdirectory, pattern=filepattern, full.names=T, recursive=T,  ignore.case=T, include.dirs=F )
    
        # filter on directory pattern
        if (!is.null(directorypattern)) {
          keep = grep ( directorypattern, projectfiles, ignore.case =T )
          if (length(keep) == 0) {
            # not found, try an approximate match 
            keep = agrep( directorypattern, projectfiles, ignore.case =T )
          }
          if (length(keep)>0) projectfiles = projectfiles[ keep ]
        }

        # functionname filter
        if (!is.null(functionname)) {
          keep = grep ( functionname, projectfiles, ignore.case =T )
          if (length(keep) == 0) {
            # if no match try approximate matching
            keep = agrep( functionname, projectfiles, ignore.case =T )
          }
          if (length(keep)>0) {
            for ( nm in keep ) source( file= projectfiles[nm] )
            return( projectfiles[keep] )  # this breaks out of the loops as it is a single file-search and source call
          }
        } 
      

        # remove archived functions, etc.
        toremove = c("retired", "_archive", "archive", "test", "orphan", "request", "example" )
        rem  = NULL
        for (i in toremove) {
          rem0 = grep ( i, projectfiles,  ignore.case =T ) 
          if (length( rem0)>0 ) rem = c( rem, rem0)
        }
        if ( length(rem)>0 ) {
          rem = unique(rem)
          projectfiles = projectfiles[-rem]
        }

        # filter on key directories 
        # determine correct directory .. first look for a set of key directories, if not then scan all files
        md = NULL
        for ( kd in keydirectories) {
          md = c( md, grep ( kd, projectfiles, ignore.case =T ) )
          md = unique( md )
        }
        if (length(md) > 0 ) {
          projectfiles = projectfiles[ md ]
          filestosource = c( filestosource, projectfiles )
          break()  # no need to continue with the loop
        }
      }

    }

    if ( length( filestosource) > 0 ) {
      for ( nm in filestosource ) source( file=nm )
      return( filestosource )
    }
 
	}



