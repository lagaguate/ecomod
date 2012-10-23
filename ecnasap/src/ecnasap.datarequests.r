

   source(file.path(ecnasapdir, "load.ecnasap.environment.r"))

# ----------------------
# load main data table
#  ecnasap = get.cat.ecnasap (source="redo")
#  ecnasap = get.cat.ecnasap (source="file")
   
# ----------------------
# create set-level summaries of catches of classes of fish
#  set = ecnasap.catches(ecnasap, source="redo")
   set = ecnasap.catches(source="file")
     
   
# ------------------------
# 1. Species lists by area ..  with sums of numbers caught per tow
  
  e = get.cat.ecnasap (source="file")
 
  if (subset=="all") { }
  if (subset=="ken.groupings") e = e[ which(e$pred1==1 | e$pred2==1 | e$prey==1), ]
 
  e = e[ , c("lon", "lat", "spid", "commonname", "scientificname", "totno")]
  e = e[ is.finite(e$totno) , ]
  
  areas = c("nafo.2j3kl", "nafo.3no", "nafo.3p", "nafo.4rs", "nafo.4t", 
            "nafo.4vw", "nafo.4x", "nafo.5y", "nafo.5zew" )
  regions = c("2J3KL", "3NO", "3P", "4RS", "4T", "4VW", "4X", "5Y", "5Zew" )
  sps = sort(unique(e$spid))
  
  spnames = data.frame(spid=sps, common=NA, sciname=NA)
  for (i in 1:nrow(spnames)) {
    j = which(e$spid == spnames$spid[i])[1]
    spnames[i,] = e[j, c("spid", "commonname", "scientificname") ]
  }
  
  outlist = matrix(NA, nrow=nrow(spnames), ncol=length(areas))
  for (a in 1:length(areas)) {
    qa = filter.region.polygon(e, areas[a])
    
    for (s in 1:length(spnames$spid)) {
      qs = which( e$spid == spnames$spid[s] )
      q = sort(unique(intersect(qa, qs)))
      outlist[s,a] = sum( e[q,"totno"] )
  }}

  out = ifelse(outlist>0, 1,0)
  names(out) = regions 
  spnames.counts = cbind(spnames, outlist)
  spnames.bin = cbind(spnames, out)
   
  write.table(spnames.counts, file="sps.list.nafo.csv", sep=";")

    
# ------------------------
# 2. Species lists by area and by time (yr) 
  
  e = get.cat.ecnasap (source="file")
  
  if (subset=="all") { }
  if (subset=="ken.groupings") e = e[ which(e$pred1==1 | e$pred2==1 | e$prey==1), ]

  e = e[ , c("yr", "lon", "lat", "spid", "commonname", "scientificname", "totno")]
  e = e[ is.finite(e$totno) , ]
  
  areas = c("nafo.2j3kl", "nafo.3no", "nafo.3p", "nafo.4rs", "nafo.4t", 
            "nafo.4vw", "nafo.4x", "nafo.5y", "nafo.5zew" )
  regions = c("2J3KL", "3NO", "3P", "4RS", "4T", "4VW", "4X", "5Y", "5Zew" )
  sps = sort(unique(e$spid))
  yrs = sort(unique(e$yr))
  
  spnames = expand.grid( spid=sps, yr=yrs)
  attr(spnames, "out.attrs") = NULL
  spnames$common=NA
  spnames$sciname=NA
  for (i in 1:nrow(spnames)) {
    j = which(e$spid==spnames$spid[i] )
    k = e[  j[1]  ,]
    spnames$common[i] = k$commonname
    spnames$sciname[i] = k$scientificname
  }
  
  outlist = matrix(NA, nrow=nrow(spnames), ncol=length(areas))
  for (a in 1:length(areas)) {r
    qa = filter.region.polygon(e, areas[a])
    for (s in 1:length(spnames$spid)) {
      qs = which( e$spid == spnames$spid[s] & e$yr==spnames$yr[s])
      q = sort(unique(intersect(qa, qs)))
      outlist[s,a] = mean( e[q,"totno"], na.rm=T )
  }}

  out = ifelse(outlist>0, 1,0)
  colnames(out) = regions 
  colnames(outlist) = regions 
  
  spnames.counts = cbind(spnames, outlist)
  spnames.bin = cbind(spnames, out)
  
  spnames.counts$common  = gsub("[\"]", "", spnames.counts$common)
  spnames.counts$sciname  = gsub("[\"]", "", spnames.counts$sciname)

  write.table(spnames.counts, file="sps.list.nafo2.csv", sep=";", quote=F)



