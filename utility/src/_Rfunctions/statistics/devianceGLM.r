devianceGLM <- function (x, data, test = "F", digits = 2) {
    #deviance explained by glm model 
    #x is glm model object
    nexpl <- x$deviance
    tot <- x$null.deviance
    expl <- tot - nexpl
    ratio <- round((expl/tot * 100), digits = digits)
    expl <- round(expl, digits = digits)
    tot <- round(tot, digits = digits)
    cat("Deviance explained: ", expl, "/", tot, "(", ratio, "percent)\n\n")
    formula0 <- paste(names(x$model)[1], "~ 1")
    model0 <- glm(as.formula(formula0), family = x$family, data = na.omit(data))
    result <- anova(model0, x, test = test)
    return(result)
    }
