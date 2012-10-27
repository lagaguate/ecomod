
	loadfunctions = function( projectname, directorypattern=NULL, functionname=NULL, keydirectories = NULL, filepattern=NULL ) {
	
    # used to load local functions conveniently
    # sequence slightly important ... modify with care
    
		projectdirectory = project.directory( name=projectname )

		if (is.null( filepattern ) ) filepattern="\\.r$"
		
    # determine directories to search to speed up the file searching
		if (is.null(directorypattern )) {

      if (is.null(keydirectories)) {
			  # determine correct directory .. first look for a set of key directories, if not then scan all files
			  keydirectories = c("r", "\\.r", "\\_r", "rfunc", "rfunctions", ".rfunctions", "\\_rfunctions" )
			  keydirectories = paste( "\\<", keydirectories, "\\>", sep="")
		  }
      searchdirectories = file.path( projectdirectory, "src" )
			searchdirectories = list.dirs(path=searchdirectories, full.names=TRUE, recursive=TRUE)
			md = NULL
			for ( kd in keydirectories) {
				md = c( md, grep ( kd, searchdirectories, ignore.case =T ) )
				md = unique( md )
			}
			if (length(md) > 0 ) searchdirectories = searchdirectories[ md ]
		
    } else {

      ## i.e. simpler but a full search of the project directory and not just under "src" .. can be slow if many directories
      searchdirectories = file.path( projectdirectory )
			searchdirectories = list.dirs(path=searchdirectories, full.names=TRUE, recursive=TRUE ) 
     	keep = grep ( directorypattern, searchdirectories, ignore.case =T )
			if (length(keep) == 0) {
        # not found, try an approximate match 
        keep = agrep( directorypattern, searchdirectories, ignore.case =T )
      }
			if (length(keep)>0) {
 	      searchdirectories = searchdirectories[ keep ]
      }
    }


    # determine files that need to be sourced
		if (!is.null(functionname)) {
			projectfiles = list.files( path=searchdirectories, pattern=filepattern, 
				full.names=T, recursive=T,  ignore.case=T, include.dirs=F )
			keep = grep ( functionname, projectfiles, ignore.case =T )
			if (length(keep) == 0) {
        # if no match try approximate matching
        keep = agrep( functionname, projectfiles, ignore.case =T )
      }
			if (length(keep)>0) {
				projectfiles = projectfiles[keep]
  			for ( nm in projectfiles ) source( file=nm )
				return( projectfiles )
			}
			return( paste( "File not found", functionname ) ) 
		
    } else { 
      
      projectfiles = NULL
      projectfiles = list.files( path=searchdirectories, pattern=filepattern, full.names=T, 
        recursive=T,  ignore.case=T, include.dirs=F )
      projectfiles = unique( projectfiles )


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

      if ( length(projectfiles) > 0 ) {
        for ( nm in projectfiles ) source( file=nm )
        return(projectfiles ) 
      }
    }
	}



