annualMeans <- function(x,lambda.threshold=0.075,gaussian=T) {
        #x is from the annualMix function
        
        if(gaussian==F) bf <- x[[3]] 
        if(gaussian==T) bf <- rep(1,length(x[[3]]))
        
        out <- list()
        #setup init matrix
                lo <- c()
                        for(i in 1:length(bf)) {
                        if(bf[i]>0) {
                        lo[i] <-  length(x[[bf[i]]][[i]]$lambda)  
                                  }
                          }
                  mo <- max(lo)
        ll <- length(x[[1]])
        out <- as.data.frame(matrix(NA,nrow=ll,ncol=2+5*mo,dimnames=list(c(1:ll),c('Index','model',paste('par1',1:mo,sep="."),paste('par2',1:mo,sep="."),paste('lambda',1:mo,sep="."),paste('xbar',1:mo,sep="."),paste('index',1:mo,sep=".")))))
                
        for(i in 1:length(bf)) {
                if(bf[i]>0) {
                        y <- x[[bf[i]]][[i]]
                out[i,'Index'] <- i
                out[i,'model'] <- y$ft
                        
                if(y$ft=='gammamixEM') {
                                inde                                                                                 <- order(y$median)
                                llp                                                                                 <- length(inde)
                                out[i,grep('par1',colnames(out))][1:llp]         <- y$gamma.pars[1,][inde]
                                out[i,grep('par2',colnames(out))][1:llp]         <- y$gamma.pars[2,][inde]
                                out[i,grep('lambda',colnames(out))][1:llp]         <- y$lambda[inde]
                                out[i,grep('xbar',colnames(out))][1:llp]         <- y$median[inde]        
                                out[i,grep('index',colnames(out))][1:llp]         <- inde        
                                
                }else{
                                inde                                                                                 <- order(y$mu)
                                llp                                                                                 <- length(inde)
                                out[i,grep('par1',colnames(out))][1:llp]         <- y$mu[inde]
                                out[i,grep('par2',colnames(out))][1:llp]         <- y$sigma[inde]
                                out[i,grep('lambda',colnames(out))][1:llp]         <- y$lambda[inde]
                                out[i,grep('xbar',colnames(out))][1:llp]         <- y$mu[inde]
                                out[i,grep('index',colnames(out))][1:llp]         <- inde        
                        }
                }
        }
        return(as.data.frame(out))
}

