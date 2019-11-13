source("tools.R")
load("../DATA/long.rda")
library(tsdb)
library(RMySQL)
library(tidyverse)
library(plm)
options(scipen=22)

## Removing column names with indigenous
indig <- colnames(long)[grep("indigen", colnames(long))]
long <-  long[, !names(long) %in% indig ]
imported <- colnames(long)[grep("imported", colnames(long))]
long <-  long[, !names(long) %in% imported ]
long <- long[,!names(long) %in%
              c("sa_import_rawmat_pc_rawmat_purchased", "sa_no_of_employees",
                "owner_gp_name", "own.group", "owner_code", "nic_prod_code","nic_name",
                "incorporation_year"      ,"nic.3digit")]
borr <- colnames(long)[grep("borr", colnames(long))]
borr <- borr[!borr %in% "sa_borrowings"]
long <-  long[, !names(long) %in% borr ]


##-----------------------------------------------------
##-- Filters --##

### Choosing manufacturing firms
long$nic.2digit <- as.numeric(long$nic.2digit)
long <- long[which(as.numeric(long$nic.2digit)<44),]
table(long$year)


## - Remove firm years where sales, ta, gfa, wage bill, rawmat is NA
long <- long[complete.cases(long[,c("sa_sales","sa_gross_fixed_assets",
                                    "sa_salaries","sa_rawmat_exp")]),]
table(long$year)

## - Remove firms with negative or zero values
posvec <- c("sa_salaries","sa_rawmat_exp","sa_gross_fixed_assets","age")
for(i in posvec){
  if(length(which(long[,i]<=0))>0){
    long <- long[-which(long[,i]<=0),]
  }
}
table(long$year)

## Removing duplicated entries which appear due to slotting algorithm in Prowess
long <- long[-which(duplicated(long[,c("sa_sales","sa_company_name")])),]
table(long$year)


## Creating table for the composition of the firms
compnfirms <- t(t(table(long$year)))
compnfirms <- data.frame(rownames(compnfirms), compnfirms[,1])
colnames(compnfirms) <- c("Year","Number of firms")
genxtable(xtable(compnfirms, caption="Compostion of firms by year"), "../DOC/TABLES/compnfirms")

## Filtering data from 2000 to 2016
long$year <- as.numeric(long$year)
long <- long[which(long$year >= 1996 & long$year <= 2016),]



rawdir <- "../DATA/"
meta <- read.csv(paste(rawdir, "metadata_financials.csv",sep=""), sep = ",", stringsAsFactors = FALSE)
colnames(meta) <- c("variable", "indicator", "unit", "table")

meta <- meta[meta$variable %in% colnames(long),][,1:2]
nic <- c("nic.2digit", "Broad industry classification code")
meta <- rbind(meta, nic)
sink("../DOC/TABLES/indicatordescription.gen")
print(xtable(unique(meta), caption="Data Variables"),
      include.rownames=FALSE,
      latex.environments=c("center"), floating=TRUE)
sink()



## Reading WPI data

wpi <- tsdb("in.wpi.all", freq="monthly")
wpi <- wpi[grep("Mar", index(wpi)),]
wpifactor <- as.numeric(wpi[which(index(wpi)=="Mar 2017"),1])/wpi[,1]



## Merging WPI and main data frame
index(wpifactor) <- paste("31", index(wpifactor))
index(wpifactor) <- as.Date(index(wpifactor), format="%d %b %Y")
wpifactor <- data.frame(wpifactor); wpifactor$sa_finance1_year<- as.Date(rownames(wpifactor))
long <- merge(long, wpifactor, by.x="sa_finance1_year")
summary(long)


## Multiplying all nominal series by wpifactor
nom.series <- colnames(long)[4:29]
nom.series <- nom.series[-which(nom.series %in% c("nic.2digit", "year", "age"))]
long[,nom.series] <- long$wpifactor*long[,nom.series]



## Data from 2000 to 2016
## dates <- seq.Date(as.Date("2000-03-31"),as.Date("2016-03-31"), by="1 year")
## long <- long[which(long$sa_finance1_year %in% dates),]


## Cleaning Export Variables
long[is.na(long$sa_export_goods), "sa_export_goods"] <- 0
long[which(long$sa_export_goods<0), "sa_export_goods"] <- 0
long[is.na(long$sa_export_serv), "sa_export_serv"] <- 0
long$export <- long$sa_export_goods + long$sa_export_serv
## Cleaning import variables
impcolumns <- colnames(long)[grep("sa_import_", colnames(long))][-5]
long[,impcolumns] <- apply(long[, impcolumns],2 , function(x) {
    x[is.na(x)] <- 0
    return(x)
})
long$import <- rowSums(long[,impcolumns])
# Dummy for both export only, import only, none, export and import 
long$expimp <- ""
long$expimp[which(long$export==0 & long$import==0)] <- "00"
long$expimp[which(long$export>0 & long$import==0)] <- "10"
long$expimp[which(long$export==0 & long$import>0)] <- "01"
long$expimp[which(long$export>0 & long$import>0)] <- "11"


# Dummy for export and import 
long$exp <- 0
long$imp <- 0
long$exp[which(long$export >0)] <- 1
long$imp[which(long$import>0)] <- 1

# Table of composition of firms
comp_table <- split(long, long$sa_finance1_year)
comp_table <- lapply(comp_table, function(x){
    year <- unique(format(as.Date(as.character(x$sa_finance1_year)), "%Y"))
    exp <- sum(x$expimp=="10")/nrow(x)
    imp <- sum(x$expimp=="01")/nrow(x)
    both <- sum(x$expimp=="11")/ nrow(x)
    none <- sum(x$expimp=="00")/ nrow(x)
    fin <- data.frame(year,none,  exp,imp,both, nrow(x))
    colnames(fin) <- c("Year","None","Export only", "Import only", "Both", "Total")
    return(fin)
})
comp_table <- do.call("rbind", comp_table)
genxtable(xtable(comp_table, caption="Composition of firms based on trade market participation"), "../DOC/TABLES/composition")

### Productivity 
## Labour Productivity
long$sa_sales_n_chg_in_stk[which(is.na(long$sa_sales_n_chg_in_stk))] <- 0
long$va <- long$sa_industrial_sales+long$sa_sales_n_chg_in_stk-long$sa_rawmat_exp-long$sa_power_and_fuel_exp
summary(long$va)
nrow(long[which(long$va<0 & long$pat>0),c("sa_company_name","sa_finance1_year","sa_pat","va")])

long$va[which(long$va<=0)] <- NA
long$labprod <- log(long$va)-log(long$sa_salaries) #(Tabrizy Trofimenko 2010)
long$capprod <- log(long$va)-log(long$sa_gross_fixed_assets) #(Tabrizy Trofimenko 2010)
long$labprod <- winsorise(long$labprod)$winsorised
long$capprod <- winsorise(long$capprod)$winsorised



### Creating additional variables 
long$patsales <- long$sa_pat/long$sa_sales
long$patsales <- winsorise(long$patsales)$winsorised
long$lsales <- log(long$sa_sales)
long$lgfa <- log(long$sa_gross_fixed_assets)
long$lsalary <- log(long$sa_salaries)
long$size <- (long$sa_total_assets+long$sa_sales)/2
long$lsize <- log(long$size)
long$lage <- log(long$age)
long$lrawmat <- log(long$sa_rawmat_exp +1)
long$lpower <- log(long$sa_power_and_fuel_exp+1)
long$lexport <- log(long$export)
long$limport <- log(long$import)

## Density plots for sales, Total assets, gross fixed assets, wage bill, Age, Power expenses
pdf("../DOC/PICS/denslsales.pdf", width=5.6, height=2.4, pointsize=10)
par(mai=c(.4, .7, .3, .2))
ggplot(long, aes(x=lsales, colour=expimp))+ geom_density()+ theme(legend.position=c(0.85, 0.85))+
    scale_color_manual(name = "Trade Status", labels= c("None","Export only", "Import only", "Both"),
                       values= c("00"= "blue", "01"= "green", "10"="red", "11"= "black"))
dev.off()

pdf("../DOC/PICS/denslgfa.pdf", width=5.6, height=2.4, pointsize=10)
par(mai=c(.4, .7, .3, .2))
ggplot(long, aes(x=lgfa, colour=expimp))+ geom_density()+ theme(legend.position=c(0.85, 0.85))+
    scale_color_manual(name = "Trade Status", labels= c("None","Export only", "Import only", "Both"),
                       values= c("00"= "blue", "01"= "green", "10"="red", "11"= "black"))
dev.off()

pdf("../DOC/PICS/denslsalary.pdf", width=5.6, height=2.4, pointsize=10)
par(mai=c(.4, .7, .3, .2))
ggplot(long, aes(x=lsalary, colour=expimp))+ geom_density() + theme(legend.position=c(0.85, 0.85))+
 scale_color_manual(name = "Trade Status", labels= c("None","Export only", "Import only", "Both"),
                     values= c("00"= "blue", "01"= "green", "10"="red", "11"= "black"))
dev.off()
pdf("../DOC/PICS/denslpower.pdf", width=5.6, height=2.4, pointsize=10)
par(mai=c(.4, .7, .3, .2))
ggplot(long, aes(x=lpower, colour=expimp))+ geom_density() + theme(legend.position=c(0.85, 0.85))+
 scale_color_manual(name = "Trade Status", labels= c("None","Export only", "Import only", "Both"),
                    values= c("00"= "blue", "01"= "green", "10"="red", "11"= "black"))
dev.off()
pdf("../DOC/PICS/denslrawmat.pdf", width=5.6, height=2.4, pointsize=10)
par(mai=c(.4, .7, .3, .2))
ggplot(long, aes(x=lrawmat, colour=expimp))+ geom_density()+ theme(legend.position=c(0.85, 0.85))+
 scale_color_manual(name = "Trade Status", labels= c("None","Export only", "Import only", "Both"),
                     values= c("00"= "blue", "01"= "green", "10"="red", "11"= "black"))
dev.off()
pdf("../DOC/PICS/denslexport.pdf", width=5.6, height=2.4, pointsize=10)
par(mai=c(.4, .7, .3, .2))
ggplot(long, aes(x=lexport, colour=expimp))+ geom_density()+ theme(legend.position=c(0.85, 0.85))+
 scale_color_manual(name = "Trade Status", labels= c("Export only", "Both"),
                     values= c( "10"="red", "11"= "black"))
dev.off()
pdf("../DOC/PICS/denslimport.pdf", width=5.6, height=2.4, pointsize=10)
par(mai=c(.4, .7, .3, .2))
ggplot(long, aes(x=limport, colour=expimp))+ geom_density()+ theme(legend.position=c(0.85, 0.85))+
 scale_color_manual(name = "Trade Status", labels= c( "Import only", "Both"),
                     values= c("01"= "green", "11"= "black"))
dev.off()

pdf("../DOC/PICS/denscapprod.pdf", width=5.6, height=2.4, pointsize=10)
ggplot(long, aes(x=capprod, colour=expimp))+ geom_density()+ theme(legend.position=c(0.85, 0.85))+
    scale_color_manual(name = "Trade Status", labels= c("None","Export only", "Import only", "Both"),
                       values= c("00"= "blue", "01"= "green", "10"="red", "11"= "black"))
dev.off()


pdf("../DOC/PICS/denspatsales.pdf", width=5.6, height=2.4, pointsize=10)
ggplot(long, aes(x=patsales, colour=expimp))+ geom_density()+ theme(legend.position=c(0.85, 0.85))+
    scale_color_manual(name = "Trade Status", labels= c("None","Export only", "Import only", "Both"),
                       values= c("00"= "blue", "01"= "green", "10"="red", "11"= "black"))+ xlim(-0.5,0.5)
dev.off()

## Summary statisitics 
genxtable <- function(x, basename, include.rownames=FALSE) {
  print(x,
        type="latex",
        file=paste(basename,".gen", sep=""),
        include.rownames=include.rownames,
        table.placement="tp",
        caption.placement="top",
        sanitize.text=function(x)x,
        latex.environments=c("center","footnotesize"), signif.stars = TRUE, floating=FALSE)
}


long$labels <- as.factor(long$expimp)
levels(long$labels) <- c("None","Import only", "Export Only", "Both")
col <- c( "lsales", "lgfa", "lsalary","lrawmat","lpower","lexport", "limport", "capprod", "patsales")
over <- lapply(col, function(y) {
sumstats <- split(long, long$expimp)
sumstats <- lapply(sumstats, function(x){
    status <- as.character(x$labels)
    status <- status[1]
    meanstat <- round(mean(x[,y], na.rm=TRUE),2)
    meanstat[is.infinite(meanstat)] <- 0
    sdstat<- round(sd(x[,y], na.rm=TRUE),2)
    sdstat[is.nan(sdstat)] <- 0
    x <- cbind(status,meanstat, sdstat)
    colnames(x) <- c("Status","mean", "sd")
    return(x)
})
sumstats <- do.call("rbind", sumstats)
genxtable(xtable(sumstats,caption=""),paste("../DOC/TABLES/sumstats",y,sep=""))
return(sumstats)
})

#### Table of transition probabilities
long <- long[order(long$sa_company_name, long$sa_finance1_year),]
longpd <- pdata.frame(long, index=c("sa_finance1_cocode", "year"))
longpd$lagexpimp  <- lag(longpd$expimp,1)
longpd$lagexp <- lag(longpd$exp,1)
longpd$lagimp <- lag(longpd$imp,1)

transition <- split(longpd, longpd$lagexpimp)
transition <- lapply( transition, function(x){
    x <- round(table(x$expimp)/nrow(x),3)
    return(x)
    })
transition <- do.call("rbind", transition)
colnames(transition) <- c("None", "Import Only", "Export Only", "Both")
transition <- cbind(c("None", "Import Only", "Export Only", "Both"), transition)
colnames(transition)[1] <- "T-1/ T"
genxtable(xtable(transition), "../DOC/TABLES/transition")

lpcol <- c("sa_finance1_cocode", "year", "lsales", "lsalary", "lgfa","lrawmat","lpower",
           "export", "import", "expimp", "exp","imp","limport", "lexport", "lagexpimp", "lagexp", "lagimp")
lp <- longpd[,lpcol]

save(lp,file="forproduction.rda")
save(longpd, file="fulldata.rda")
