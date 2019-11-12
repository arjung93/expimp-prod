library(plm)
library(prodest)
load(file="forproduction.rda")
library(stargazer)

### Exogenous Productivity
gOPLP_ex <- function(vtheta, mX, mlX, vphi, vlag.phi, vres, stol, Pr.hat, lagexp,lagimp,
    att) 
{
    Omega <- vphi - mX %*% vtheta
    Omega_lag <- vlag.phi - mlX %*% vtheta
    if (att == FALSE) {
        Omega_lag_pol <- cbind(1, Omega_lag, Omega_lag^2, Omega_lag^3,lagexp,lagimp,lagexp*lagimp)
    }
    else {
        Omega_lag_pol <- cbind(1, Omega_lag, Omega_lag^2, Omega_lag^3,
            Pr.hat, Pr.hat^2, Pr.hat^3, Pr.hat * Omega_lag, Pr.hat^2 * 
                Omega_lag, Pr.hat * Omega_lag^2)
    }
    g_b <- solve(crossprod(Omega_lag_pol), tol = stol) %*% t(Omega_lag_pol) %*% 
        Omega
    XI <- vres - (mX %*% vtheta) - (Omega_lag_pol %*% g_b)
    crit <- crossprod(XI)
    return(crit)
}

finalOPLP_ex<- function (ind, data, fnum, snum, cnum, opt, theta0, boot, tol, 
    att, lagexp,lagimp) 
{
    if (sum(as.numeric(ind)) == length(ind)) {
        newid <- data[ind, "idvar", drop = FALSE]
    }
    else {
        newid <- as.matrix(as.numeric(rownames(ind)))
        ind <- as.matrix(ind)
    }
    data <- data[ind, ]
    first.stage <- lm(data[, "Y"] ~ data[, grepl("regvars", colnames(data))], 
        na.action = na.exclude)
    fX <- data[, grepl("free", colnames(data)), drop = FALSE]
    phi <- fitted(first.stage)
    beta.free <- coef(first.stage)[2:(1 + fnum)]
    if (cnum != 0) {
        beta.control <- coef(first.stage)[(2 + fnum):(1 + fnum + 
            cnum)]
    }
    else {
        beta.control <- NULL
    }
    if (is.null(theta0)) {
        theta0 <- coef(first.stage)[(2 + fnum):(1 + fnum + snum)] + 
            rnorm((snum), 0, 0.01)
    }
    phi <- phi - (fX %*% beta.free)
    newtime <- data[, "timevar", drop = FALSE]
    rownames(phi) <- NULL
    rownames(newtime) <- NULL
    lag.phi <- lagPanel(value = phi, idvar = newid, timevar = newtime)
    res <- data[, "Y", drop = FALSE] - (fX %*% beta.free)
    state = data[, grepl("state", colnames(data)), drop = FALSE]
    lag.sX = data[, grepl("lag.sX", colnames(data)), drop = FALSE]
    Pr.hat <- data[, grepl("Pr.hat", colnames(data)), drop = FALSE]
    lagexp <- data[,"lagexp"]
    lagimp <- data[,"lagimp"]
    tmp.data <- model.frame(state ~ lag.sX + phi + lag.phi + 
                                res + Pr.hat+ lagexp+lagimp)
    if (opt == "optim") {
        try.state <- try(optim(theta0, gOPLP_ex, method = "BFGS", 
            mX = tmp.data$state, mlX = tmp.data$lag.sX, vphi = tmp.data$phi, 
            vlag.phi = tmp.data$lag.phi, vres = tmp.data$res, 
            stol = tol, Pr.hat = tmp.data$Pr.hat, att = att ,lagexp=tmp.data$lagexp, lagimp=tmp.data$lagimp), 
            silent = TRUE)
        if (!inherits(try.state, "try-error")) {
            beta.state <- try.state$par
            opt.outcome <- try.state
        }
        else {
            beta.state <- matrix(NA, (snum), 1)
            opt.outcome <- list(convergence = 99)
        }
    }
    else if (opt == "DEoptim") {
        try.state <- try(DEoptim(fn = gOPLP, lower = c(theta0), 
            upper = rep.int(1, length(theta0)), mX = tmp.data$state, 
            mlX = tmp.data$lag.sX, vphi = tmp.data$phi, vlag.phi = tmp.data$lag.phi, 
            vres = tmp.data$res, stol = tol, Pr.hat = tmp.data$Pr.hat, 
            att = att, control = DEoptim.control(trace = FALSE)), 
            silent = TRUE)
        if (!inherits(try.state, "try-error")) {
            beta.state <- try.state$optim$bestmem
            opt.outcome <- try.state
        }
        else {
            beta.state <- matrix(NA, (snum), 1)
            opt.outcome <- list(convergence = 99)
        }
    }
    else if (opt == "solnp") {
        try.state <- try(suppressWarnings(solnp(theta0, gOPLP, 
            mX = tmp.data$state, mlX = tmp.data$lag.sX, vphi = tmp.data$phi, 
            vlag.phi = tmp.data$lag.phi, vres = tmp.data$res, 
            stol = tol, Pr.hat = tmp.data$Pr.hat, att = att, 
            control = list(trace = FALSE))), silent = TRUE)
        if (!inherits(try.state, "try-error")) {
            beta.state <- try.state$pars
            opt.outcome <- try.state
        }
        else {
            beta.state <- matrix(NA, (snum), 1)
            opt.outcome <- list(convergence = 99)
        }
    }
    if (boot == FALSE) {
        return(list(betas = c(beta.free, beta.state, beta.control), 
            opt.outcome = opt.outcome, FSbetas = coef(first.stage), 
            FSresiduals = resid(first.stage)))
    }
    else {
        return(c(beta.free, beta.state, beta.control))
    }
}



prodestLP_ex <- function (Y, fX, sX, pX, idvar, timevar, lagexp, lagimp, R = 20, cX = NULL, opt = "optim", 
    theta0 = NULL, cluster = NULL, tol = 1e-100, exit = FALSE ) 
{
    Start = Sys.time()
    Y <- checkM(Y)
    fX <- checkM(fX)
    sX <- checkM(sX)
    pX <- checkM(pX)
    idvar <- checkM(idvar)
    timevar <- checkM(timevar)
    snum <- ncol(sX)
    fnum <- ncol(fX)
    lagexp <- checkM(lagexp)
    lagimp <- checkM(lagimp)
    if (!is.null(cX)) {
        cX <- checkM(cX)
        cnum <- ncol(cX)
    }
    else {
        cnum <- 0
    }
    polyframe <- data.frame(cbind(sX, pX))
    mod <- model.matrix(~.^2 - 1, data = polyframe)
    mod <- mod[match(rownames(polyframe), rownames(mod)), ]
    if (exit[1] == TRUE) {
        att = TRUE
        if (is.logical(exit)) {
            exitdata = data.frame(idvar = idvar, timevar = timevar)
            maxdate <- max(exitdata$timevar)
            exit <- as.matrix(do.call(rbind, lapply(split(exitdata, 
                list(exitdata$idvar)), function(x) {
                maxid = max(x$timevar)
                x$ans <- x$timevar == maxid & maxid != maxdate
                cbind(x$ans)
            })))
        }
        else {
            exit <- checkMD(exit)
        }
        if ((mean(exit) == 1) | (mean(exit) == 0)) {
            stop("No ID appears to exit the sample. Check the exit variable or run the model without exit = TRUE")
        }
        else {
            lagProbitvars <- cbind(mod, sX^2, pX^2)
            for (i in 1:dim(lagProbitvars)[2]) {
                lagProbitvars[, i] <- lagPanel(lagProbitvars[, 
                  i], idvar = idvar, timevar = timevar)
            }
            probreg <- glm(exit ~ lagProbitvars, family = binomial(link = "probit"))
            Pr.hat <- predict(probreg, newdata = as.data.frame(lagProbitvars), 
                type = "response")
        }
    }
    else {
        att = FALSE
        Pr.hat <- matrix(0, nrow = nrow(mod), ncol = 1)
    }
    regvars <- cbind(fX, cX, mod, sX^2, pX^2,(sX^2)*(pX^2),sX^3, pX^3,(sX^3)*(pX^3))
    lag.sX = sX
    for (i in 1:snum) {
        lag.sX[, i] = lagPanel(sX[, i], idvar = idvar, timevar = timevar)
    }
    data <- suppressWarnings(as.matrix(data.frame(state = sX, 
        lag.sX = lag.sX, free = fX, Y = Y, idvar = idvar, timevar = timevar, 
        regvars = regvars, Pr.hat = Pr.hat, lagexp=lagexp, lagimp=lagimp)))
    if (!is.null(cX)) {
        data <- suppressWarnings(as.matrix(data.frame(data, cX = cX)))
    }
    betas <- finalOPLP_ex(ind = TRUE, data = data, fnum = fnum, 
        snum = snum, cnum = cnum, opt = opt, theta0 = theta0, 
        boot = FALSE, tol = tol, att = att, lagexp=lagexp, lagimp=lagimp)
    boot.indices <- block.boot.resample(idvar, R)
    if (is.null(cluster)) {
        nCores = NULL
        boot.betas <- matrix(NA, R, (fnum + snum + cnum))
        for (i in 1:R) {
            boot.betas[i, ] <- finalOPLP_ex(ind = boot.indices[[i]], 
                data = data, fnum = fnum, snum = snum, cnum = cnum, 
                opt = opt, theta0 = theta0, boot = TRUE, tol = tol, 
                att = att, lagexp= lagexp, lagimp=lagimp)
        }
    }
    else {
        nCores = length(cluster)
        clusterEvalQ(cl = cluster, library(prodest))
        boot.betas <- matrix(unlist(parLapply(cl = cluster, boot.indices, 
            finalOPLP, data = data, fnum = fnum, snum = snum, 
            cnum = cnum, opt = opt, theta0 = theta0, boot = TRUE, 
            tol = tol, att = att)), ncol = fnum + snum + cnum, 
            byrow = TRUE)
    }
    boot.errors <- apply(boot.betas, 2, sd, na.rm = TRUE)
    res.names <- c(colnames(fX, do.NULL = FALSE, prefix = "fX"), 
        colnames(sX, do.NULL = FALSE, prefix = "sX"))
    if (!is.null(cX)) {
        res.names <- c(res.names, colnames(cX, do.NULL = FALSE, 
            prefix = "cX"))
    }
    names(betas$betas) <- res.names
    names(betas$FSbetas)[2:length(betas$FSbetas)] <- c(res.names, 
        rep(" ", (length(betas$FSbetas) - length(res.names) - 
            1)))
    names(boot.errors) <- res.names
    elapsedTime = Sys.time() - Start
    out <- new("prod", Model = list(method = "LP", FSbetas = betas$FSbetas, 
        boot.repetitions = R, elapsed.time = elapsedTime, theta0 = theta0, 
        opt = opt, opt.outcome = betas$opt.outcome, nCores = nCores), 
        Data = list(Y = Y, free = fX, state = sX, proxy = pX, 
            control = cX, idvar = idvar, timevar = timevar, FSresiduals = betas$FSresiduals), 
        Estimates = list(pars = betas$betas, std.errors = boot.errors))
    return(out)
}


lp_ex <- lp
lagexp <- as.numeric(lp_ex$lagexp)
lagimp <- as.numeric(lp_ex$lagimp)

Y <- as.numeric(lp_ex$lsales)
fX<- as.numeric(lp_ex$lsalary)
sX<- as.numeric(lp_ex$lgfa)
pX<- as.numeric(lp_ex$lrawmat)
idvar<- as.numeric(lp_ex$sa_finance1_cocode)
timevar<- as.numeric(lp_ex$year)

lpex <- prodestLP_ex(Y,fX,sX,pX,idvar, timevar, lagexp, lagimp)

lpex_data <- data.frame(do.call("cbind",lpex@Data))
lpres <- lpex@Data$Y - (lpex@Data$free*lpex@Estimates$pars[1]) - (lpex@Data$state*lpex@Estimates$pars[2])
lpex_data$lpres <- lpres
lpex_data <- pdata.frame(lpex_data, index=c("V5","V6"))
lpex_data$lagres<- lag(lpex_data$lpres,1)
lpex_data$lagres2 <- lpex_data$lagres^2
lpex_data$lagres3 <- lpex_data$lagres^3
lpex_data <- cbind(lpex_data, lp_ex[,c("lagexp","lagimp")])
lpex_prodevol <- lm(lpres~lagres+lagres2+lagres3+lagexp+lagimp+ lagexp*lagimp, data=lpex_data)

sink(file="../DOC/TABLES/prod.gen")
stargazer(lpex_prodevol, covariate.labels=c("$alpha_{1}$","$alpha_{2}$","$alpha_{3}$","$alpha_{4}$","$alpha_{5}$","$alpha_{6}$","$alpha_{0}$"), title="Productivity Evolution", keep.stat="n", dep.var.labels= c("$\\omega_{it}$"))
sink()
regLPest <- do.call("rbind", lpex@Estimates)
regLPest <-t(regLPest)
rownames(regLPest) <- c("$ \\beta_{l}$", "$ \\beta_{k}$")
colnames(regLPest) <- c("Value", "Bootstrap Standard Errors")

sink(file="../DOC/TABLES/regLP.gen")
stargazer(regLPest, title="Cobb-Douglus coefficients")
sink()

## Debugging exogenous process of LP
## lp_ex <- lp
## lagexp <- as.numeric(lp_ex$lagexp)
## lagimp <- as.numeric(lp_ex$lagimp)

## Y <- as.numeric(lp_ex$lsales)
## fX<- as.numeric(lp_ex$lsalary)
## sX<- as.numeric(lp_ex$lgfa)
## pX<- as.numeric(lp_ex$lrawmat)
## idvar<- as.numeric(lp_ex$sa_finance1_cocode)
## timevar<- as.numeric(lp_ex$year)
## R <- 5
## tol = 1e-100
## cX <- NULL
## theta0 <- NULL

## Start = Sys.time()
## Y <- checkM(Y)
## fX <- checkM(fX)
## sX <- checkM(sX)
## pX <- checkM(pX)
## idvar <- checkM(idvar)
## timevar <- checkM(timevar)
## snum <- ncol(sX)
## fnum <- ncol(fX)
## if (!is.null(cX)) {
##     cX <- checkM(cX)
##     cnum <- ncol(cX)
## }else {
##     cnum <- 0
## }
## polyframe <- data.frame(cbind(sX, pX))
## mod <- model.matrix(~.^2 - 1, data = polyframe)
## mod <- mod[match(rownames(polyframe), rownames(mod)), ]
## if (exit[1] == TRUE) {
##     att = TRUE
##     if (is.logical(exit)) {
##         exitdata = data.frame(idvar = idvar, timevar = timevar)
##         maxdate <- max(exitdata$timevar)
##         exit <- as.matrix(do.call(rbind, lapply(split(exitdata, 
##                                                       list(exitdata$idvar)), function(x) {
##                                                           maxid = max(x$timevar)
##                                                           x$ans <- x$timevar == maxid & maxid != maxdate
##                                                           cbind(x$ans)
##                                                       })))
##     }
##     else {
##         exit <- checkMD(exit)
##     }
##     if ((mean(exit) == 1) | (mean(exit) == 0)) {
##         stop("No ID appears to exit the sample. Check the exit variable or run the model without exit = TRUE")
##     }
##     else {
##         lagProbitvars <- cbind(mod, sX^2, pX^2)
##         for (i in 1:dim(lagProbitvars)[2]) {
##             lagProbitvars[, i] <- lagPanel(lagProbitvars[, 
##                                                          i], idvar = idvar, timevar = timevar)
##         }
##         probreg <- glm(exit ~ lagProbitvars, family = binomial(link = "probit"))
##         Pr.hat <- predict(probreg, newdata = as.data.frame(lagProbitvars), 
##                           type = "response")
##     }
## }
## else {
##     att = FALSE
##     Pr.hat <- matrix(0, nrow = nrow(mod), ncol = 1)
## }
## regvars <- cbind(fX, cX, mod, sX^2, pX^2)
## lag.sX = sX
## for (i in 1:snum) {
##     lag.sX[, i] = lagPanel(sX[, i], idvar = idvar, timevar = timevar)
## }
## data <- suppressWarnings(as.matrix(data.frame(state = sX, 
##                                               lag.sX = lag.sX, free = fX, Y = Y, idvar = idvar, timevar = timevar, 
##                                               regvars = regvars, Pr.hat = Pr.hat)))
## if (!is.null(cX)) {
##     data <- suppressWarnings(as.matrix(data.frame(data, cX = cX)))
## }
## ind = TRUE
## data = data
## fnum = fnum
## snum = snum
## cnum = cnum
## opt = opt
## theta0 = theta0 
## boot = FALSE
## tol = tol
## att = att
## lagexp=lagexp
## lagimp=lagimp


## if (sum(as.numeric(ind)) == length(ind)) {
##         newid <- data[ind, "idvar", drop = FALSE]
##     }    else {
##         newid <- as.matrix(as.numeric(rownames(ind)))
##         ind <- as.matrix(ind)
##     }
##     data <- data[ind, ]
##     first.stage <- lm(data[, "Y"] ~ data[, grepl("regvars", colnames(data))], 
##         na.action = na.exclude)
##     fX <- data[, grepl("free", colnames(data)), drop = FALSE]
##     phi <- fitted(first.stage)
##     beta.free <- coef(first.stage)[2:(1 + fnum)]
##     if (cnum != 0) {
##         beta.control <- coef(first.stage)[(2 + fnum):(1 + fnum + 
##             cnum)]
##     }else {
##         beta.control <- NULL
##     }
##     if (is.null(theta0)) {
##         theta0 <- coef(first.stage)[(2 + fnum):(1 + fnum + snum)] + 
##             rnorm((snum), 0, 0.01)
##     }
##     phi <- phi - (fX %*% beta.free)
##     newtime <- data[, "timevar", drop = FALSE]
##     rownames(phi) <- NULL
##     rownames(newtime) <- NULL
##     lag.phi <- lagPanel(value = phi, idvar = newid, timevar = newtime)
##     res <- data[, "Y", drop = FALSE] - (fX %*% beta.free)
##     state = data[, grepl("state", colnames(data)), drop = FALSE]
##     lag.sX = data[, grepl("lag.sX", colnames(data)), drop = FALSE]
## Pr.hat <- data[, grepl("Pr.hat", colnames(data)), drop = FALSE]
## lagexp <- lagexp[!is.na(lagexp)]
## lagimp <- lagimp[!is.na(lagimp)]
## tmp.data <- model.frame(state ~ lag.sX + phi + lag.phi + 
##         res + Pr.hat + lagexp)


## mX = tmp.data$state
## mlX = tmp.data$lag.sX
## vphi = tmp.data$phi
## vlag.phi = tmp.data$lag.phi
## vres = tmp.data$res
## stol = tol
## Pr.hat = tmp.data$Pr.hat
## att = att
## lagexp=lagexp
## lagimp=lagimp
## vtheta <- theta0
## Omega <- vphi - mX %*% vtheta
##     Omega_lag <- vlag.phi - mlX %*% vtheta
##     if (att == FALSE) {
##         Omega_lag_pol <- cbind(1, Omega_lag, Omega_lag^2, Omega_lag^3,lagexp,lagimp,lagexp*lagimp)
##     }
##     else {
##         Omega_lag_pol <- cbind(1, Omega_lag, Omega_lag^2, Omega_lag^3,
##             Pr.hat, Pr.hat^2, Pr.hat^3, Pr.hat * Omega_lag, Pr.hat^2 * 
##                 Omega_lag, Pr.hat * Omega_lag^2)
##     }
##     g_b <- solve(crossprod(Omega_lag_pol), tol = stol) %*% t(Omega_lag_pol) %*% 
##         Omega
##     XI <- vres - (mX %*% vtheta) - (Omega_lag_pol %*% g_b)
##     crit <- crossprod(XI)
