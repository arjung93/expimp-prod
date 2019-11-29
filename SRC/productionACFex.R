library(plm)
library(prodest)
load(file="forproduction.rda")
library(stargazer)


gACF <- function (theta, mZ, mW, mX, mlX, vphi, vlag.phi,lagexp,lagimp) 
{
    Omega <- vphi - mX %*% theta
    Omega_lag <- vlag.phi - mlX %*% theta
    Omega_lag_pol <- cbind(1, Omega_lag, Omega_lag^2, Omega_lag^3,lagexp,lagimp,lagexp*lagimp)
    g_b <- solve(crossprod(Omega_lag_pol)) %*% t(Omega_lag_pol) %*% 
        Omega
    XI <- Omega - Omega_lag_pol %*% g_b
    crit <- t(crossprod(mZ, XI)) %*% mW %*% (crossprod(mZ, XI))
    return(crit)
}


finalACF <- function (ind, data, fnum, snum, cnum, opt, theta0,lagexp,lagimp,boot = FALSE) 
{
    if (sum(as.numeric(ind)) == length(ind)) {
        newid <- data[ind, "idvar", drop = FALSE]
    }
    else {
        newid <- as.matrix(as.numeric(rownames(ind)))
        ind <- as.matrix(ind)
    }
    data <- data[ind, ]
    first.stage <- lm(data[, "Y", drop = FALSE] ~ data[, grepl("regvars", 
        colnames(data)), drop = FALSE], na.action = na.exclude)
    phi <- fitted(first.stage)
    if (is.null(theta0)) {
        theta0 <- coef(first.stage)[2:(1 + snum + fnum + cnum)] + 
            rnorm((snum + fnum), 0, 0.01)
    }
    newtime <- data[, "timevar", drop = FALSE]
    rownames(phi) <- NULL
    rownames(newtime) <- NULL
    lag.phi <- lagPanel(idvar = newid, timevar = newtime, value = phi)
    Z <- data[, grepl("Z", colnames(data)), drop = FALSE]
    X <- data[, grepl("Xt", colnames(data)), drop = FALSE]
    lX <- data[, grepl("lX", colnames(data)), drop = FALSE]
    lagexp <- data[,"lagexp"]
    lagimp <- data[,"lagimp"]
    tmp.data <- model.frame(Z ~ X + lX + phi + lag.phi+lagexp+lagimp)
    W <- solve(crossprod(tmp.data$Z))/nrow(tmp.data$Z)
    if (opt == "optim") {
        try.out <- try(optim(theta0, gACF, method = "BFGS", mZ = tmp.data$Z, 
            mW = W, mX = tmp.data$X, mlX = tmp.data$lX, vphi = tmp.data$phi, 
            vlag.phi = tmp.data$lag.phi,
            lagexp=tmp.data$lagexp, lagimp=tmp.data$lagimp), silent = TRUE)
        if (!inherits(try.out, "try-error")) {
            betas <- try.out$par
            opt.outcome <- try.out
        }
        else {
            betas <- matrix(NA, (snum + fnum), 1)
            opt.outcome <- list(convergence = 999)
        }
    }
    else if (opt == "DEoptim") {
        try.out <- try(DEoptim(gACF, lower = theta0, upper = rep.int(1, 
            length(theta0)), mZ = tmp.data$Z, mW = W, mX = tmp.data$X, 
            mlX = tmp.data$lX, vphi = tmp.data$phi, vlag.phi = tmp.data$lag.phi, 
            control = DEoptim.control(trace = FALSE)), silent = TRUE)
        if (!inherits(try.out, "try-error")) {
            betas <- try.out$optim$bestmem
            opt.outcome <- try.out
        }
        else {
            betas <- matrix(NA, (snum + fnum), 1)
            opt.outcome <- list(convergence = 99)
        }
    }
    else if (opt == "solnp") {
        try.out <- try(suppressWarnings(solnp(theta0, gACF, mZ = tmp.data$Z, 
            mW = W, mX = tmp.data$X, mlX = tmp.data$lX, vphi = tmp.data$phi, 
            vlag.phi = tmp.data$lag.phi, control = list(trace = FALSE))), 
            silent = TRUE)
        if (!inherits(try.out, "try-error")) {
            betas <- try.out$pars
            opt.outcome <- try.out
        }
        else {
            betas <- matrix(NA, (snum + fnum), 1)
            opt.outcome <- list(convergence = 999)
        }
    }
    if (boot == FALSE) {
        return(list(betas = betas, opt.outcome = opt.outcome, 
            FSresiduals = resid(first.stage)))
    }
    else {
        return(betas)
    }
}



prodestACF <- function (Y, fX, sX, pX, idvar, timevar, lagexp, lagimp, R = 20, cX = NULL, opt = "optim", 
    theta0 = NULL, cluster = NULL) 
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
    if (length(theta0) != cnum + fnum + snum & !is.null(theta0)) {
        stop(paste0("theta0 length (", length(theta0), ") is inconsistent with the number of parameters (", 
            cnum + fnum + snum, ")"), sep = "")
    }
    polyframe <- data.frame(fX, sX, pX)
    mod <- model.matrix(~.^2 - 1, data = polyframe)
    mod <- mod[match(rownames(polyframe), rownames(mod)), ]
    regvars <- cbind(mod, fX^2, sX^2, pX^2)
    lag.sX = sX
    for (i in 1:snum) {
        lag.sX[, i] = lagPanel(sX[, i], idvar = idvar, timevar = timevar)
    }
    lag.fX = fX
    for (i in 1:fnum) {
        lag.fX[, i] = lagPanel(fX[, i], idvar = idvar, timevar = timevar)
    }
    if (!is.null(cX)) {
        data <- as.matrix(data.frame(Y = Y, idvar = idvar, timevar = timevar, 
            Z = data.frame(lag.fX, sX), Xt = data.frame(fX, sX), 
            lX = data.frame(lag.fX, lag.sX), cX = data.frame(cX), 
            regvars = regvars))
    }
    else {
        data <- as.matrix(data.frame(Y = Y, idvar = idvar, timevar = timevar, 
            Z = data.frame(lag.fX, sX), Xt = data.frame(fX, sX), 
            lX = data.frame(lag.fX, lag.sX), regvars = regvars, lagexp=lagexp, lagimp=lagimp))
    }
    betas <- finalACF(ind = TRUE, data = data, fnum = fnum, snum = snum, 
        cnum = cnum, opt = opt, theta0 = theta0, lagexp=lagexp, lagimp=lagimp)
    boot.indices <- block.boot.resample(idvar, R)
    if (is.null(cluster)) {
        nCores = NULL
        boot.betas <- matrix(unlist(lapply(boot.indices, finalACF, 
            data = data, fnum = fnum, snum = snum, cnum = cnum, 
            opt = opt, theta0 = theta0, boot = TRUE, lagexp=lagexp, lagimp=lagimp)), ncol = fnum + 
            snum + cnum, byrow = TRUE)
    }
    else {
        nCores = length(cluster)
        clusterEvalQ(cl = cluster, library(prodest))
        boot.betas <- matrix(unlist(parLapply(cl = cluster, boot.indices, 
            finalACF, data = data, fnum = fnum, snum = snum, 
            cnum = cnum, opt = opt, theta0 = theta0, boot = TRUE)), 
            ncol = fnum + snum + cnum, byrow = TRUE)
    }
    boot.errors <- apply(boot.betas, 2, sd, na.rm = TRUE)
    res.names <- c(colnames(fX, do.NULL = FALSE, prefix = "fX"), 
        colnames(sX, do.NULL = FALSE, prefix = "sX"))
    if (!is.null(cX)) {
        res.names <- c(res.names, colnames(cX, do.NULL = FALSE, 
            prefix = "cX"))
    }
    names(betas$betas) <- res.names
    names(boot.errors) <- res.names
    elapsedTime = Sys.time() - Start
    out <- new("prod", Model = list(method = "ACF", FSbetas = NA, 
        boot.repetitions = R, elapsed.time = elapsedTime, theta0 = theta0, 
        opt = opt, opt.outcome = betas$opt.outcome, nCores = nCores), 
        Data = list(Y = Y, free = fX, state = sX, proxy = pX, 
            control = cX, idvar = idvar, timevar = timevar, FSresiduals = betas$FSresiduals), 
        Estimates = list(pars = betas$betas, std.errors = boot.errors))
    return(out)
}


lp_ex <- lp
lagexp <- as.numeric(as.character(lp_ex$lagexp))
lagimp <- as.numeric(as.character(lp_ex$lagimp))

Y <- as.numeric(as.character(lp_ex$lsales))
fX<- as.numeric(as.character(lp_ex$lsalary))
sX<- as.numeric(as.character(lp_ex$lgfa))
pX<- as.numeric(as.character(lp_ex$lrawmat))
idvar<- as.numeric(as.character(lp_ex$sa_finance1_cocode))
timevar<- as.numeric(as.character(lp_ex$year))
nic <- as.factor(lp_ex$nic.2digit)

lpex <- prodestACF(Y,fX,sX,pX,idvar, timevar, lagexp, lagimp)


lpex_data <- data.frame(do.call("cbind",lpex@Data[-which(names(lpex@Data) %in% "control")]))
colnames(lpex_data) <- names(lpex@Data[-which(names(lpex@Data) %in% "control")])
lpres <- lpex@Data$Y - (lpex@Data$free*lpex@Estimates$pars[1]) - (lpex@Data$state*lpex@Estimates$pars[2])
lpex_data$lpres <- lpres
lpex_data <- pdata.frame(lpex_data, index=c("idvar","timevar"))
lpex_data$lagres<- lag(lpex_data$lpres,1)
lpex_data$lagres2 <- lpex_data$lagres^2
lpex_data$lagres3 <- lpex_data$lagres^3
lpex_data <- cbind(lpex_data, lp_ex[,c("lagexp","lagimp")])
lpex_prodevol <- lm(lpres~lagres+lagres2+lagres3+lagexp+lagimp+ lagexp*lagimp, data=lpex_data)


sink(file="../DOC/TABLES/prodACF.gen")
stargazer(lpex_prodevol, covariate.labels=c("$alpha_{1}$","$alpha_{2}$","$alpha_{3}$","$alpha_{4}$","$alpha_{5}$","$alpha_{6}$","$alpha_{0}$"), title="Productivity Evolution", keep.stat="n", dep.var.labels= c("$\\omega_{it}$"), label="prodACF")
sink()
regLPest <- do.call("rbind", lpex@Estimates)
regLPest <-t(regLPest)
rownames(regLPest) <- c("$ \\beta_{l}$", "$ \\beta_{k}$")
colnames(regLPest) <- c("Value", "Bootstrap Standard Errors")

sink(file="../DOC/TABLES/regACF.gen")
stargazer(regLPest, title="Cobb-Douglus coefficients", label="regACF")
sink()
