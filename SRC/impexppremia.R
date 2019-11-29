library(plm)
load(file="fulldata.rda")
library(stargazer)



sales <- plm(lsales~exp+imp+exp*imp+age+as.factor(nic.2digit)+as.factor(year), data=longpd, model="within")
lgfa <- plm(lgfa~exp+imp+exp*imp+age+as.factor(nic.2digit)+as.factor(year), data=longpd, model="within")
lrawmat <- plm(lrawmat~exp+imp+exp*imp+age+as.factor(nic.2digit)+as.factor(year), data=longpd, model="within")
lsalary <- plm(lsalary~exp+imp+exp*imp+age+as.factor(nic.2digit)+as.factor(year), data=longpd, model="within")



lexport <- plm(lexport~imp+age+as.factor(year), data=longpd, model="within")
limport <- plm(limport~exp+age+as.factor(year), data=longpd, model="within")


capprod <- plm(capprod~exp+imp+exp*imp+age+as.factor(nic.2digit)+as.factor(year), data=longpd, model="within")
patsales <- plm(patsales~exp+imp+exp*imp+age+as.factor(nic.2digit)+as.factor(year), data=longpd, model="within")

sink(file="../DOC/TABLES/expimppremia.gen")
stargazer(sales, lgfa, lrawmat, lsalary,
          dep.var.labels=c("Sales", "Gross Fixed Assets", "Raw Materials"),
          covariate.labels=c("$d_{it}^{X}$","$d_{it}^{M}$","$Age_{it}$", "$d_{it}^{X}*d_{it}^{M}$"),
          title="Export and Import Premia",
          keep=c("exp","imp","age"),
          add.lines= list(c("Industry Dummies","Yes","Yes","Yes","Yes"),
              c("Time Fixed Effects","Yes","Yes","Yes","Yes")),
          label="expimppremia")
sink()







sink(file="../DOC/TABLES/prodpremia.gen")
stargazer(lexport,limport, capprod, patsales,
          dep.var.labels=c("Export","Import","Capital Productivity","Profit to Sales"),
          covariate.labels=c("$d_{it}^{M}$","$d_{it}^{X}$","$Age_{it}$", "$d_{it}^{X}*d_{it}^{M}$"),
          title="Export and Import Premia",
          keep=c("exp","imp","age"),
          add.lines= list(c("Industry Dummies","Yes","Yes","Yes","Yes"),
              c("Time Fixed Effects","Yes","Yes","Yes","Yes")),
          label="prodpremia",
          keep.stat="n")
sink()
