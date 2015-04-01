
# ----------------------
# ecansap data access example and some simple analyses of species composition, etc
# ecnasapdir = "/home/jae/src/ecnasap"; defined in home/jae/src/startup.r

  loadfunctions( "ecnasap", functionname="load.ecnasap.environment.r" )
 
# ----------------------
# load main data table
#  ecnasap = get.cat.ecnasap (source="redo")
#  ecnasap = get.cat.ecnasap (source="file")
   
# ----------------------
# create set-level summaries of catches of classes of fish
#  set = ecnasap.catches(ecnasap, source="redo")
   set = ecnasap.catches(source="file")
     
# -----------------------
# eventually need to massage in RV data from 4VWX

  source (file.path(grdfishdir, "groundfish.functions.r") )
  varstokeep = c("yr", "lon", "lat", "totno", "grd", "pel", "shark", "ntaxa", "ngrd", "npel", "nshark" )
  vwx = get.set(source="sql", save=F)
  vwx = vwx[, varstokeep]
  

# -------------------------
# DFA on species composition contrasting low/high NAO years

  library(MASS)
  
  a = get.cat.ecnasap (source="file")
  a$yr =a$yr+1900
  xa = xtabs(totno ~ as.factor(yr) + as.factor(commonname), a)
  
  xb = xa
  attributes(xb) = NULL
  xc = as.data.frame(matrix(data=xb, nrow=dim(xa)[1], ncol=dim(xa)[2] ))
    
  years = as.numeric(rownames(xa))
  yrs = data.frame(i = c(1:length(years)), yr=years)
  yrs = recode.time(yrs, "nao")
   
  
  # first analysis .. all data
    
    filter = which(colSums(xc) > 0)  
    xb = xc [, filter ]
    xb = log(xb+1)
    data = cbind(xb, nao=as.character(yrs$yr))
    z= lda(nao ~ ., data)
    plot(z)
  
 
  # second analysis
  
    filter = which( rank(colSums(xc)) > dim(xc)[2] * 0.918)  
      # filters the top 23 species .. seems to give the best resolution between years
    
    xb = xc [, filter ]
    xb = log(xb+1)
    data = cbind(xb, nao=as.character(yrs$yr))
    z= lda(nao ~ ., data)
    plot(z)
    z$svd
  
    z.coef = as.data.frame(coef(z))
    z.coef$sp = colnames(xa)[filter]
    write.table(z.coef, file="z.coef", quote=F, sep=";")
 

# -------------------------
# correspondence analysis
    
    a = get.cat.ecnasap (source="file")
    a$yr =a$yr+1900
    xa = xtabs(totno ~ as.factor(yr) + as.factor(commonname), a)
  
    xb = xa
    attributes(xb) = NULL
    xc = as.data.frame(matrix(data=xb, nrow=dim(xa)[1], ncol=dim(xa)[2] ))
   
    x = a[ is.finite(x$totno) & x$totno>0  ,]
    k = 1e3         # a large constant number to make xtabs work
    threshold = 0
    
    m = xtabs( as.integer(totno*k) ~ as.factor(setid) + as.factor(spid), data=x ) / k
    i = unique(c(which(rowSums(m)/dim(m)[1] < threshold ), which( rowSums(m)==0 ) ))
    j = unique(c(which(colSums(m)/dim(m)[1] < threshold ), which( colSums(m)==0 ) ))

      if (length(i) > 0 ) m = m[ -i , ]
      if (length(j) > 0 ) m = m[ , -j ]

      m = log10(m + 1)
      ord = cca( m )
      sp.sc = scores(ord)$species
      si.sc = scores(ord)$sites

      scores = data.frame( id=rownames(si.sc), ca1=as.numeric(si.sc[,1]), ca2=as.numeric(si.sc[,2]) )
      scores$id = as.character(scores$id)

    plot(ord, display="sp", type="text")
    plot(ord, display="wa", type="points")
    print( ord$CA$eig[c(1,2)]/sum(ord$CA$eig)*100 )


# -------------------------
# ordination of diversity

  d = read.table("avgfishby_1deg_latitude.csv", sep=";")
  data =   d[-1,-1]
  xnames = d[-1, 1]
  ynames = t(as.matrix(d[1,-1]))

  colnames (data) = ynames
  rownames (data) = xnames
  
  data = as.matrix(data)
  data = log (data+1)
  data = data[rowSums(data, na.rm=T)>0 ,]
  data = data[, colSums(data, na.rm=T)>0]
    
  x = t(data)
  
  x = scale(x, center=T, scale=T)
  y = pca.analyse(x, yrange=ynames, title="Longitude")

 
