Prepare.strata.data <- function (data) {
    #data in frame like Scallopsurveydata
    if(any(names(data) %in% 'STRATA.ID')) data$Strata <- data$STRATA.ID
    if(any(names(data) %in% 'strat')) data$Strata <- data$strat
    
    w <- regexpr("_", names(data))
    wincr <- attr(w, "match.length") - 1
    wincr[wincr < 0] <- 0
    if(any(w>0)){ ii = which(w>0); substring(names(data)[ii], w[ii], w[ii] + wincr[ii]) <- "."}
    names(data)[charmatch("time", names(data))] <- "Time"
    oldClass(data) <- c("data.frame", "strata.data")
    data
}
