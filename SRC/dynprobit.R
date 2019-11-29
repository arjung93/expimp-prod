library(pglm)
library(cquad)
source("tools.R")
library(stargazer)
library(readstata13)
library(foreign)
load(file="forprobit.rda")
load(file="forproduction.rda")
load(file="fulldata.rda")
load(file="dynprobit.rda")

colnames(lpex_data)[5:6] <- c("sa_finance1_cocode", "year")

dynprobit <- merge(lp, lpex_data[,c("sa_finance1_cocode", "year", "lpres")], by=c("sa_finance1_cocode", "year"))

dynprobit <- pdata.frame(dynprobit, index=c("sa_finance1_cocode", "year"))

pglm(imp~lagimp+lsales+lsalary+exp+lpres, data=dynprobit, model="random",family=binomial('probit') )
pglm(exp~lagexp+lsales+lsalary+imp+lpres, data=dynprobit, model="random",family=binomial('probit') )


x <- plm(lexport~lpres+lgfa+lsalary+as.factor(year)+limport, data=dynprobit, model="within")
y <- plm(lexport~lpres+lgfa+lsalary+as.factor(year)+limport, data=dynprobit, model="random")
phtest(x, y)


# Starters versus never starters
starters <- split(dynprobit, dynprobit$sa_finance1_cocode)

starters <- lapply(starters, function(x){
    x <- x[order(as.numeric(as.character(x$year))),]
    x$expstarter <- 0
    x$expstarter[which(x$exp==1)[1]] <- 1
    x$impstarter <- 0
    x$impstarter[which(x$imp==1)[1]] <- 1
    return(x)
    })
starters <- do.call("rbind", starters)

starters$laglpres <- lag(starters$lpres, 1)
starters <- starters[!is.na(starters$laglpres),]


t1 <- plm(lag(lpres,1)~lag(lgfa,1)+lag(lsalary,1)+exp +imp+exp*imp, data=starters,model="within")
t2 <- plm(lag(lpres,2)~lag(lgfa,2)+lag(lsalary,2)+exp +imp+exp*imp, data=starters,model="within")
t3 <- plm(lag(lpres,3)~lag(lgfa,3)+lag(lsalary,3)+exp +imp+exp*imp, data=starters,model="within")

sink(file="../DOC/TABLES/discprod.gen")
stargazer(t1,t2,t3, dep.var.labels=c("t-1","t-2","t-3"),keep=c("exp","imp"), covariate.labels=c("$d_{it}^{X}$","$d_{it}^{M}$", "$d_{it}^{X}*d_{it}^{M}$"), title="Effect of Discrete Decision on Lagged Productivity", keep.stat="n")
sink()


t11 <- plm(lag(lpres,1)~lag(lgfa,1)+lag(lsalary,1)+lexport +limport, data=starters,model="within")
t22 <- plm(lag(lpres,2)~lag(lgfa,2)+lag(lsalary,2)+lexport +limport, data=starters,model="within")
t33 <- plm(lag(lpres,3)~lag(lgfa,3)+lag(lsalary,3)+lexport +limport, data=starters,model="within")


sink(file="../DOC/TABLES/contprod.gen")
stargazer(t11,t22,t33, dep.var.labels=c("t-1","t-2","t-3"),keep=c("exp","imp"), covariate.labels=c("$log(Export)_{it}$","$log(import)_{it}$"), title="Effect of Traded Value on Lagged Productivity", keep.stat="n")
sink()


forbiprobit <- merge(dynprobit[, c( "sa_finance1_cocode", "year", "lpres")]
                   , longprobit, c("sa_finance1_cocode", "year"))


write.dta(data=forbiprobit, file="../DATA/biprobit.dta", convert.factors="numeric")

