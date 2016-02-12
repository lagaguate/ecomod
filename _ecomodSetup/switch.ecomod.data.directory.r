switch.ecomod.data.directory = function(x=1) {
  #\\ NOTE:: this can also be done on the fly by manually the directory associated with a git branch 	
	if(x==1) ecomod.datadirectory <<- ecomod.datadirectory1
  if(x==2) ecomod.datadirectory <<- ecomod.datadirectory2
}
