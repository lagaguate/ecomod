  capwords = function(s, strict = FALSE) {
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }

