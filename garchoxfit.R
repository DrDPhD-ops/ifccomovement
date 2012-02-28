"garchOxFit" <-
function (formula.mean = ~arma(0, 0), formula.var = ~garch(1, 1), 
    series = x, cond.dist = c("gaussian", "t", "ged", "skewed-t"), 
    include.mean = TRUE, include.var=TRUE, truncation = 100, 
    trace = TRUE, title = NULL, arch.in.mean=0) 
{
    fit = list()
    fit$x = series
##    include.var = TRUE
##    include.var=FALSE   (FTS 2008, This command can be used to estimate
##     volatility model without the constant term,e.g. RiskMetric models.
##      RST(4/27/2008)
## As an alternative, I moved "include.var" command into the argument.
## 
    fit$csts = c(include.mean, include.var)
    distris = 0:3
    names(distris) = c("gaussian", "t", "ged", "skewed-t")
    distri = distris[cond.dist[1]]
    fit$cond.dist = cond.dist[1]
    if (missing(formula.mean)) {
        fit$formula.mean = ~arma(0, 0)
        fit$arma.orders = c(0, 0)
    }
    else {
        fit$arma.orders = as.numeric(strsplit(strsplit(strsplit(as.character(formula.mean), 
            "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    }
    arfima = FALSE
    fit$arfima = as.integer(arfima)
    if (missing(formula.var)) {
        fit$formula.var = ~garch(1, 1)
        fit$garch.orders = c(1, 1)
    }
    else {
        fit$garch.orders = as.numeric(strsplit(strsplit(strsplit(as.character(formula.var), 
            "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    }
#    arch.in.mean = 0
    fit$arch.in.mean = arch.in.mean
    models = 1:11
    names(models) = c("garch", "egarch", "gjr", "aparch", "igarch", 
        "figarch.bbm", "figarch.chung", "fiegarch", "fiaparch.bbm", 
        "fiaparch.chung", "hygarch")
    selected = strsplit(as.character(formula.var), "\\(")[[2]][1]
    fit$model = models[selected]
    nt = length(series)
    ident = paste(selected, as.character(floor(runif(1) * 10000)), 
        sep = "")
    parameters = c(csts = fit$csts, distri = distri, arma = fit$arma.orders, 
        arfima = fit$arfima, garch = fit$garch.orders, model = fit$model, 
        inmean = fit$arch.in.mean, trunc = truncation, nt = nt)
    write(x = parameters, file = "OxParameter.txt")
    write(x = "X", file = "OxSeries.csv", ncolumns = 1)
    write(x=series, file = "OxSeries.csv", ncolumns = 1, append = TRUE)
    command = "/Applications/OxMetrics6/ox/bin/oxl /Applications/OxMetrics6/ox/lib/GarchOxModelling.ox"
 #   fit$ox = system(command, show.output.on.console = trace, 
 #       invisible = TRUE)
    fit$ox = system(command)
    fit$model = selected
    fit$call = match.call()
    fit$residuals = scan("OxResiduals.csv", skip = 1, quiet = TRUE)
    fit$condvars = scan("OxCondVars.csv", skip = 1, quiet = TRUE)
    fit$coef = matrix(scan("OxParameters.csv", skip = 1, quiet = TRUE), 
        byrow = TRUE, ncol = 3)
    fit$title = title
    if (is.null(title)) 
        fit$title = "GARCH Ox Modelling"
    fit$description = as.character(date())
    class(fit) = "garchOx"
    fit
}

