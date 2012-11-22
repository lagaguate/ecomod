
crosstabulate.biologicals = function(X, p) {
  # and crosstabulate numbers and abundance and compute quantiles, and z-scores, etc


   if (length(i)>0) {
      y = sum.data(Y[i,], factors, variable)
      names(y) = c(factors, varname)
      X = merge(x=X, y=y, by=factors, all.x=T )
      X[,varname] = X[,varname] / X$sa   # express as x / km2
      X[!is.finite(X[,varname]),varname] = 0
    } else {
      dummy = rep(0, dim(X)[1])
      oldnames = names(X)
      X = cbind(X, dummy)
      names(X) = c(oldnames, varname)
    }
    return(X)

  sum.data = function(x, factors=c("trip", "set"), variable) {

    if (variable=="number") x[,variable] = 1  # create a dummy variable to allow counting of numbers
    m = as.data.frame.table(
          tapply( X=x[,variable],
                  INDEX=x[,factors],
                  FUN=sum,
                  simplify=T,
                  na.rm=T ) )
    names(m) = c(factors, paste(variable,"tot",sep="."))
    m = factor2character(m, factors)

    return(m)
  }





      # compute snow crab abundance from det tables
      numbers = as.data.frame( xtabs( rep(1,nrow(det)) ~ as.factor(trip) + as.factor(set), data=det ) )
      names(numbers) = c("trip", "set", "totno")
			numbers$trip = as.character( numbers$trip )
			numbers$set = as.numeric( as.character(numbers$set ))

      good = which(is.finite(det$mass))
      biomass = as.data.frame(xtabs( mass ~ as.factor(trip) + as.factor(set), data=det[good,], exclude="" ) )
      names(biomass) = c("trip", "set", "totmass")
      biomass$trip = as.character( biomass$trip )
			biomass$set = as.numeric( as.character(biomass$set ))
			biomass$totmass = biomass$totmass / 1000  # convert from grams to kg
      # !!!!!!!! must verify units of other species from observer system

      snowcrab = merge(x=numbers, y=biomass, by=c("trip", "set"), all=T)
     
}
