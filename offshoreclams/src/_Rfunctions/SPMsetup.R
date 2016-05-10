SPMsetup = function(vmslogdata,grid.out,vmspoly,subpolys,effort.min=100000,r=5,n.min=7){
  
  if(missing(subpolys)){

    # find centre points of gridded data to act as a node for estimating parameters
    useGrids = with(Totalgrid.out,subset(grid,paste(PID,SID)%in%with(subset(grid.polyData$effort[[1]],Z>effort.min),paste(PID,SID))))
    gridPoints = calcCentroid(useGrids)
    nnodes = nrow(gridPoints)

    # create circles of radius r(km) to include data around node
    subpolys = data.frame(PID=sort(rep(1:nnodes,100)),POS=rep(1:100,nnodes))
    for (i in 1:nnodes) {
      bufcircs = bufferCircle(c(gridPoints$X[i],gridPoints$Y[i]),r)
      subpolys$X[1:100+100*(i-1)] = bufcircs$lon[-101]
      subpolys$Y[1:100+100*(i-1)] = bufcircs$lat[-101]
    }
  }
  else {
    nnodes = length(unique(subpolys$PID))
  }

  # map of data points
  pdf(file.path( project.datadirectory("offshoreclams"), "figures","SPMdatapoints.pdf"),11,8)
  ClamMap2("Ban")
  addPolys(subpolys,col=rgb(1,0,0,0.1),border=rgb(0,0,0,0.1))
  with(vmslogdata,points(X,Y,pch=16,cex=0.2,col=rgb(0,0,0,.1)))
  dev.off()

  # identify data points within nodes
  key = findPolys(vmslogdata,subpolys,maxRows=1e+07)

  # assign dataset to each node
  SPdata = list()
  n = c()
  for (i in 1:nnodes){
    cat(i)
    # calculate clam habitat
    clamhabitat = joinPolys(vmspoly,subset(subpolys,PID==i),operation="INT")
    if(!is.null(clamhabitat)){
      attr(clamhabitat,"projection") = "LL"
      clamhabitatarea = calcArea(clamhabitat,1)$area # area of clam habitat with the circle

      # select data points
      tmpdata =  subset(vmslogdata,EID%in%subset(key,PID==i)$EID)
      yrs = min(tmpdata$year):max(tmpdata$year)
      C = with(tmpdata,tapply(C,year,sum))/1000 # catch in tons
      E = with(tmpdata,tapply(A,year,sum)) / 10^6  # effort (area swept in km2)
      O = C / E * clamhabitatarea # catch per unit effort in t / area of clam habitat
      n[i] = length(C)
      SPdata[[i]] = merge(data.frame(yrs=yrs,H=clamhabitatarea),data.frame(yrs=as.numeric(names(C)),C=C,O=O),all=T)
      SPdata[[i]]$C[is.na(SPdata[[i]]$C)] = 0
    }
    else print("No habitat!")

  }

  # select only dataset with more than n.min years of data
  SPdata = SPdata[which(n>=n.min)]
 
  return(SPdata)

}
