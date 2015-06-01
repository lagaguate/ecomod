Association.test <- function (data, hydro, strata.group, species, nreps = 0, method = c("KS-Test", "CVM-Test")[1], subset,stat.plot=F) {
  #amc additions for characterizing the distribution of catch weighted effort and effort
    method.int <- charmatch(method, c("KS-Test", "CVM-Test"))
    
    if (is.na(method.int)) 
        stop("Invalid test method specified")
    if (method.int == 0) 
        stop("Ambiguous test method")
    if (!inherits(data, "strata.data")) 
        stop("Not a legitimate strata data object")
    attach(data)
    if (!missing(subset)) {
        data <- data[subset, ]
    }
    detach("data")
    strata.use <- is.element(data$Strata, strata.group$Strata)
    hydro <- (data[hydro])[strata.use, ]
    species <- (data[species])[strata.use, ]
    Strata <- data$Strata[strata.use]
    if (is.null(species)) 
        species <- rep.int(1, length(hydro))
    nreps <- nreps - 1
    tempy <- cbind(hydro, species, Strata)
    if (any(is.na(hydro))) 
        tempy <- (na.omit(as.data.frame(tempy)))
    hydro <- as.numeric(as.vector(tempy[, 1]))
    species <- as.numeric(as.vector(tempy[, 2]))
    Strata <- as.vector(tempy[, 3])
    WH <- strata.group$NH
    na.strata <- match(strata.group$Strata, unique(Strata))
    WH[is.na(na.strata)] <- NA
    IWH <- cbind(strata.group$Strata, WH)
    IWH <- (na.omit(as.data.frame(IWH)))
    WH <- as.numeric(IWH$WH)
    IWH <- IWH[[1]]
    
    WH <- (WH/sum(WH))
    yhi <- split(species, Strata)
    nh <- as.vector(sapply(yhi, length))

    yst <- sum(WH * as.vector(sapply(yhi, mean)))
    wh <- WH/nh
    Whi <- rep(0, length(species))
    for (i in seq(along = wh)) Whi[c(Strata == IWH[i])] <- wh[i]
    gt <- cumsum(sapply(split((Whi * (species - yst))/yst, hydro), 
        sum))
#AMC addition
            max.values<-max(abs(gt))
            #to get 1st and 3rd quartile of catch
                    wh1 <- WH/(nh * yst)
                    Whi1 <- rep(0, length(species))
                    for (i in seq(along = wh)) Whi1[c(Strata == IWH[i])] <- wh1[i]
                    Whi1 <- Whi1 * species       
                    gt1 <- cumsum(sapply(split(Whi1, hydro), sum))

                    abc <-  data.frame(x=rbind(as.numeric(names(gt1[length(gt1[gt1<=0.25])])), as.numeric(names(gt1[gt1>0.25][1]))),y=rbind(gt1[length(gt1[gt1<=0.25])], gt1[gt1>0.25][1]))
                    if(nrow(abc)<2) min.50 <- as.numeric(names(gt1)[1])
                    if(nrow(abc)==2) {
                    abcd <-  coefficients(lm(abc[,2]~abc[,1]))
                    min.50 <- (0.25-abcd[1])/abcd[2]
                    }
                    abc<-data.frame(x=rbind(as.numeric(names(gt1[length(gt1[gt1<=0.5])])), as.numeric(names(gt1[gt1>0.5][1]))),y=rbind(gt1[length(gt1[gt1<=0.5])], gt1[gt1>0.5][1]))
                    if(nrow(abc)<2) med.50 <- as.numeric(names(gt1)[1])
                    if(nrow(abc)==2) {
                    abcd <-  coefficients(lm(abc[,2]~abc[,1]))
                    med.50 <- (0.5-abcd[1])/abcd[2]
                    }

                    abc<-data.frame(x=rbind(as.numeric(names(gt1[length(gt1[gt1<=0.75])])), as.numeric(names(gt1[gt1>0.75][1]))),y=rbind(gt1[length(gt1[gt1<=0.75])], gt1[gt1>0.75][1]))
                    abcd <-  coefficients(lm(abc[,2]~abc[,1]))
                    max.50 <- (0.75-abcd[1])/abcd[2]
            #to get 95% intervals of hydro variable
                    hv<-cumsum(sapply(split(Whi, hydro), sum))
                    abc <-  data.frame(x=rbind(as.numeric(names(hv[length(hv[hv<=0.5])])), as.numeric(names(hv[hv>0.5][1]))),y=rbind(hv[length(hv[hv<=0.5])], hv[hv>0.5][1]))
                    if(nrow(abc)<2) med.hv.50 <- as.numeric(names(hv)[1])
                    if(nrow(abc)==2) {
                    abcd <-  coefficients(lm(abc[,2]~abc[,1]))
                    med.hv.50 <- (0.5-abcd[1])/abcd[2]
                    }

                    abc <-  data.frame(x=rbind(as.numeric(names(hv[length(hv[hv<=0.025])])), as.numeric(names(hv[hv>0.025][1]))),y=rbind(hv[length(hv[hv<=0.025])], hv[hv>0.025][1]))
                    if(nrow(abc)<2) min.95.hv <- as.numeric(names(hv)[1])
                    if(nrow(abc)==2) {
                    abcd <-  coefficients(lm(abc[,2]~abc[,1]))
                    min.95.hv <- (0.025-abcd[1])/abcd[2]
                    }

                    abc<-data.frame(x=rbind(as.numeric(names(hv[length(hv[hv<=0.975])])), as.numeric(names(hv[hv>0.975][1]))),y=rbind(hv[length(hv[hv<=0.975])], hv[hv>0.975][1]))
                    abcd <-  coefficients(lm(abc[,2]~abc[,1]))
                    max.95.hv <- (0.975-abcd[1])/abcd[2]
if(min.95.hv>max.95.hv) browser()

    test.stat.rep <- vector(mode = "numeric", length = nreps)
    if (method.int == 1) {
        test.stat <- max(abs(gt))
        prefer.lab <- c("Kolmorogorov-Smirnov Type test")
        if (nreps != 0) {
            for (i in 1:nreps) {
                Ixh <- sample(hydro, replace = TRUE, prob = Whi)
                temp.stat <- cumsum(sapply(split((Whi * (species - 
                  yst))/yst, Ixh), sum))
                test.stat.rep[i] <- max(abs(temp.stat))
            }
        }
    }
    if (method.int == 2) {
        test.stat <- sum(gt^2)
        prefer.lab <- c("Cramer-Von Mises Type test")
        if (nreps != 0) {
            for (i in 1:nreps) {
                Ixh <- sample(hydro, replace = TRUE, prob = Whi)
                temp.stat <- cumsum(sapply(split((Whi * (species - 
                  yst))/yst, Ixh), sum))
                test.stat.rep[i] <- sum(temp.stat^2)
            }
        }
    }
     if(stat.plot==T) {
     x <- sort(c(test.stat.rep,test.stat))
    c.freq<-cumsum(tabulate(match(x,unique(x))))/length(x) #cumulative frequency of unique test stats
    plot(unique(x),c.freq,xlab='Test Statistic', ylab='Cumulative Frequency',type='l',xlim=c(min(x)-min(x)*0.02,max(x)+max(x)*0.02)) #cdf plot of test stats
        #adding in the specifics of the test statistic and p-value
        ddd<-data.frame(x=unique(x),y=c.freq)
        probs<-ddd[match(max.values,ddd[,1]),2]
        arrows(x0=max.values,x1=max.values,y0=probs,y1=0.01,length=0.1)
        arrows(x0=max.values,x1=min(x)-min(x)*0.01,y0=probs,y1=probs,length=0.1)
        text(max.values+max.values*0.05,y=0.01,paste("s",round(max.values,4),sep="="),cex=0.7)
        text(min(x)+min(x)*0.05,probs+0.02,paste("p=1-",round(1-sum(test.stat <= test.stat.rep)/length(test.stat.rep),4),sep=""),cex=0.7)
    }

    locs<-names(which(abs(gt)==max.values))#finding the row and column of the gt matrix where the test statistic was located (max value of gt-ft)

    res <- list(test.stat = test.stat, test.stat.rep = c(test.stat, 
        test.stat.rep), prefer.lab = prefer.lab, prefer2.lab = c("Randomization Test"), 
        prefer3.lab = c("P-level"), location.of.max.gt=locs,p=round(sum(test.stat <= test.stat.rep)/length(test.stat.rep),4),yst1=yst,ranges<-c(min.50,med.50,max.50),hydro.ranges<-c(min.95.hv,med.hv.50,max.95.hv), descrip = "Association Analysis")
    class(res) <- "assoc"
    res
}
