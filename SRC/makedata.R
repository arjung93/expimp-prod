source("tools.R")
load("../DATA/long.rda")
library(tsdb)
library(RMySQL)
library(tidyverse)
library(plm)
options(scipen=22)










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
nom.series <- colnames(long)[4:46]
nom.series <- nom.series[-c(5:12)]
long[,nom.series] <- long$wpifactor*long[,nom.series]


##-----------------------------------------------------
##-- Filters --##

### Choosing manufacturing firms
long$ind.sales <- long[,"sa_industrial_sales"]/long[,"sa_sales"]
summary(long$ind.sales)
long <- long[which(long$ind.sales>0.50),]
nrow(long)
long$nic.2digit <- as.numeric(long$nic.2digit)
long <- long[which(as.numeric(long$nic.2digit)>9 & as.numeric(long$nic.2digit)<33),]

## - Removing firm years for which sales < 5
long <- long[-which(long$sa_sales<5),]
nrow(long)

## - Remove firm years where sales, ta, gfa, wage bill, rawmat is NA
long <- long[complete.cases(long[,c("sa_sales","sa_gross_fixed_assets","sa_salaries","sa_rawmat_exp")]),]
nrow(long)

## - Remove firms with negative or zero values
posvec <- c("sa_salaries","sa_rawmat_exp","sa_gross_fixed_assets","age")
for(i in posvec){
  if(length(which(long[,i]<=0))>0){
    long <- long[-which(long[,i]<=0),]
  }
}
nrow(long)

## Removing duplicated entries which appear due to slotting algorithm in Prowess
long <- long[-which(duplicated(long[,c("sa_sales","sa_company_name")])),]
nrow(long)
length(unique(long$sa_finance1_cocode))

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

#### Table of transition probabilities 
longpd <- pdata.frame(long, index=c("sa_finance1_year","sa_finance1_cocode"))
longpd$lagexpimp  <- lag(longpd$expimp,1)


transition <- split(longpd, longpd$lagexpimp)
transition <- lapply( transition, function(x){
    x <- table(x$expimp)/nrow(x)
    return(x)
    })
transition <- do.call("rbind", transition)


### Productivity 
## Labour Productivity
long$sa_sales_n_chg_in_stk[which(is.na(long$sa_sales_n_chg_in_stk))] <- 0
long$va <- long$sa_industrial_sales+long$sa_sales_n_chg_in_stk-long$sa_rawmat_exp-long$sa_power_and_fuel_exp
summary(long$va)
nrow(long[which(long$va<0 & long$pat>0),c("sa_company_name","sa_finance1_year","sa_pat","va")])

long$va[which(long$va<=0)] <- NA
long$labprod <- log(long$va)-log(long$sa_salaries) #(Tabrizy Trofimenko 2010)
long$capprod <- log(long$va)-log(long$sa_gross_fixed_assets) #(Tabrizy Trofimenko 2010)
plot(density(long$labprod,na.rm=TRUE))
plot(density(long$capprod,na.rm=TRUE))
long$labprod <- winsorise(long$labprod)$winsorised
long$capprod <- winsorise(long$capprod)$winsorised


### Creating additional variables 
long$dom_sales <- long$sa_sales-long$sa_export_goods-long$sa_export_serv
long$patsales <- long$sa_pat/long$sa_sales
long$lsales <- log(long$sa_sales)
long$lgfa <- log(long$sa_gross_fixed_assets)
long$lsalary <- log(long$sa_salaries)
long$size <- (long$sa_total_assets+long$sa_sales)/2
long$lsize <- log(long$size)
long$lage <- log(long$age)
long$lrawmat <- log(long$sa_rawmat_exp)
long$lpower <- log(long$sa_power_and_fuel_exp)
long$lexport <- log(long$export)
long$limport <- log(long$import)


## Density plots for sales, Total assets, gross fixed assets, wage bill, Age, Power expenses

long$exportimportfactor <- "None"
long$exportimportfactor[which(long$export>0 & long$import==0)] <- "Export"
long$exportimportfactor[which(long$export==0 & long$import>0)] <- "Import"
long$exportimportfactor[which(long$export>0 & long$import>0)] <- "Both"
long$exportimportfactor <- as.factor(long$exportimportfactor)


ggplot(long, aes(x=lsales, colour=exportimportfactor))+ geom_density()
ggplot(long, aes(x=lgfa, colour=exportimportfactor))+ geom_density()
ggplot(long, aes(x=lsalary, colour=exportimportfactor))+ geom_density()
ggplot(long, aes(x=lpower, colour=exportimportfactor))+ geom_density()
ggplot(long, aes(x=lrawmat, colour=exportimportfactor))+ geom_density()
ggplot(long, aes(x=lexport, colour=exportimportfactor))+ geom_density()
ggplot(long, aes(x=limport, colour=exportimportfactor))+ geom_density()


