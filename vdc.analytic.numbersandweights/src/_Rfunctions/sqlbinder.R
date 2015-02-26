binder<-function(sselect, paramlist){
  #if any values contain letters, we might need quotes - this checks
  has.letter <- function(x) grepl("[[:alpha:]]", x)
  #MMM 2015
  #This takes a SQL statement and a list of variable names with value(s)
  #and binds them together into a runnable query
  #Typically, multiple values are strung together with commas for use in IN
  #statements.  If the values are characters, they will also need to be
  #contained in apostrophes
  #However, this is not always the case, so the variable names can include flags 
  #that either force or prohibit the addition of quotes
  #(i.e. "_forcequote" and "_noquote")

  for (i in 1:length(names(paramlist))){
    lettercheck<-has.letter(paramlist[[i]])
    lettercheck<-is.element(TRUE,lettercheck)

    if (grepl("_noquote",names(paramlist[i]))){
      #quotes are forbidden
      if (length(paramlist[[i]])>1){
        var<-paste(paramlist[[i]], collapse=",")
      } else {
        var<-paramlist[[i]]
      }
    }else if (grepl("_forcequote",names(paramlist[i]))) {
      #quotes must be present 
      if (length(paramlist[[i]])>1){
        var<-paste("'",paste(paramlist[[i]], collapse="','"),"'",sep="")
      } else {
        var<-paste("'",paramlist[[i]],"'",sep="")
      }
    } else {
      #determine quotes automatically
      if (length(paramlist[[i]])>1){
        if (lettercheck){
          var<-paste("'",paste(paramlist[[i]], collapse="','"),"'",sep="")
        }else{
          var<-paste(paramlist[[i]], collapse=",")
        }
      }else{
        if (lettercheck){
          var<-paste("'",paramlist[[i]],"'",sep="")
        }else{
          var<-paramlist[[i]]
        } 
      }
    }
    sselect<-gsub(names(paramlist)[i],var,sselect)  
  }
  return(sselect)
}