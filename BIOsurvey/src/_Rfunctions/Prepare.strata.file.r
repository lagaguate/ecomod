Prepare.strata.file <- function (data) {
    #data in frame like Scallopstrata
    if(any(names(data) %in% 'strat')) data$Strata = data$strat
  #  data = data[order(data$Strata),]
    list(Strata = data$Strata, NH = data$NH)
}
