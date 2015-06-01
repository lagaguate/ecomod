Stratify <- function (data.obj, strata.group, species, subset) {
    if (!inherits(data.obj, "strata.data")) 
        stop("Not a legitimate strata data object")
    m <- match.call()
    attach(data.obj)
    species <- species
    if (!missing(subset)) {
        data.obj <- data.obj[subset, ]
        species <- species[subset]
    }
    detach("data.obj")
    s.group <- is.element(strata.group$Strata, data.obj$Strata)
    s.group.Strata <- strata.group$Strata[s.group]
    s.group.NH <- strata.group$NH[s.group]
    s.obj <- is.element(data.obj$Strata, s.group.Strata)
    species <- species[s.obj]
    data.obj <- data.obj[s.obj, ]
    yhi <- split(species, data.obj$Strata)
    nh <- as.vector(sapply(yhi, length))
    nhws <- sapply(yhi,function(x) length(x[x>0]))

    res <- list(yhi = yhi, Strata = s.group.Strata, Nh = s.group.NH, 
        Wh = s.group.NH/sum(s.group.NH), nh = nh, call = m,nhws = nhws)
    class(res) <- "strata"
    res
}
