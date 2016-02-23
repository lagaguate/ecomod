model.summaries = function( DS, yr=NULL, p=NULL ) {
  #function to make a table out of all the model summaries for components 
  #"speciesarea", "sizespectrum", "metabolism","speciescomposition", "condition", "biochem" 
  library("memisc")
  library("Hmisc")

  #vars = c("speciesarea", "sizespectrum", "metabolism","speciescomposition", "condition")
  vars = c("speciesarea", "metabolism","speciescomposition", "condition")
  vars = c("condition")
  
  p$season = "allseasons"
  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$modtype = "complex"
  p$movingdatawindow = 0  # this signifies no moving window ... all in one model
  
  
  for (var in vars){
    if (var == "speciesarea"){
      p$project.name = var
      p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )
      vns = c( "C", "Z", "T", "Npred" )
      for (vn in vns){
        o= habitat.model (p=p, vn = vn)
        summary(o)$r.sq
        summary(o)$dev.expl
        varname <- paste(var, vn, sep=" ")
        out1 <- capture.output(latex(summary(o)$s.table, file = "modelresults.tex", append=TRUE, digits=3, caption=varname))
        out2 <- capture.output(latex(summary(o)$r.sq, file="modelresults.tex", append=TRUE, digits=3, caption="r.sq"))
        out3 <- capture.output(latex(summary(o)$dev.expl, file = "modelresults.tex", append=TRUE, digits=3, caption="dev. expl"))
      }
    }
    
    if (var == "speciescomposition"){
      p$project.name = var
      p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )
      vns = c( "ca1", "ca2", "pca1", "pca2" )
      for (vn in vns){
        o= habitat.model (p=p, vn = vn)
        summary(o)$r.sq
        summary(o)$dev.expl
        varname <- paste(var, vn, sep=" ")
        out1 <- capture.output(latex(summary(o)$s.table, file = "modelresults.tex", append=TRUE, digits=3, caption=varname))
        out2 <- capture.output(latex(summary(o)$r.sq, file="modelresults.tex", append=TRUE, digits=3, caption="r.sq"))
        out3 <- capture.output(latex(summary(o)$dev.expl, file = "modelresults.tex", append=TRUE, digits=3, caption="dev. expl"))
      }
    }
    
    if (var == "metabolism"){
      p$project.name = var
      p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )
      vns = c( "mr", "smr", "Pr.Reaction" , "Ea", "A", "zn", "zm", "qn", "qm", "mass", "len")
      for (vn in vns){
        o= habitat.model (p=p, vn = vn)
        summary(o)$r.sq
        summary(o)$dev.expl
        varname <- paste(var, vn, sep=" ")
        out1 <- capture.output(latex(summary(o)$s.table, file = "modelresults.tex", append=TRUE, digits=3, caption=varname))
        out2 <- capture.output(latex(summary(o)$r.sq, file="modelresults.tex", append=TRUE, digits=3, caption="r.sq"))
        out3 <- capture.output(latex(summary(o)$dev.expl, file = "modelresults.tex", append=TRUE, digits=3, caption="dev. expl"))
      }
    }
    
    if (var == "condition"){
      p$project.name = var
      p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )
      #vns = c("coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic", "coSmallPelagic", "coLargePelagic", "coSmallDemersal",   "coLargeDemersal" )
      vns = c("coAll", "coFish", "coGadoid", "coDemersal", "coPelagic", "coSmallPelagic", "coLargePelagic", "coSmallDemersal",   "coLargeDemersal" )
      
      for (vn in vns){
        o= habitat.model (p=p, vn = vn)
        summary(o)$r.sq
        summary(o)$dev.expl
        varname <- paste(var, vn, sep=" ")
        out1 <- capture.output(latex(summary(o)$s.table, file = "modelresults.tex", append=TRUE, digits=3, caption=varname))
        out2 <- capture.output(latex(summary(o)$r.sq, file="modelresults.tex", append=TRUE, digits=3, caption="r.sq"))
        out3 <- capture.output(latex(summary(o)$dev.expl, file = "modelresults.tex", append=TRUE, digits=3, caption="dev. expl"))
      }
    }
    
    if (var == "sizespectrum"){
      p$project.name = var
      p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )
      vns = c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon" )
      
      for (vn in vns){
        o= habitat.model (p=p, vn = vn)
        summary(o)$r.sq
        summary(o)$dev.expl
        varname <- paste(var, vn, sep=" ")
        out1 <- capture.output(latex(summary(o)$s.table, file = "modelresults.tex", append=TRUE, digits=3, caption=varname))
        out2 <- capture.output(latex(summary(o)$r.sq, file="modelresults.tex", append=TRUE, digits=3, caption="r.sq"))
        out3 <- capture.output(latex(summary(o)$dev.expl, file = "modelresults.tex", append=TRUE, digits=3, caption="dev. expl"))
      }
    }
    
  }
}
  
  

 
  