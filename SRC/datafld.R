library(RMySQL)
library(data.table)
source("tools.R")


## Making connection to batabase


rawdir <- "../DATA/"

###  READING METADATA TO REGROUPING THE DATASETS WITHIN DIFFERENT FINANCIAL TABLES 
meta <- read.csv(paste(rawdir, "metadata_financials.csv",sep=""), sep = ",")
colnames(meta) <- c("variable", "indicator", "unit", "table")

genTable <- function(data, table)
  {
    field <- as.character(meta[meta$table == table, "variable"])
    extract.col <- c(1:4, na.omit(match(field, colnames(data))))
    fields <- colnames(data)[extract.col]
    data <- data[ , fields, with = FALSE]
    colnames(data) <- substr(colnames(data), 1, 64)
    data <- as.data.frame(data)
    data[, 4:ncol(data)] <- apply(data[, 4:ncol(data)], 2, function (x) as.numeric(as.character(x)))
    return(data)
  }

##################################### financial tables ####################################
###--------------- sa annual financial tables
sa.ann.fin <- fread(paste(rawdir, "sa_prowess_annual_financials.txt", sep = "/"), sep = "|")
### list of tables to be generated
tables <- unique(as.character(meta$table))[-1]
tables <- tables[grep("sa_annual", tables)]
table.name <- paste("prowess", tables, sep = "_")


# identity 

identity <- read.csv(paste(rawdir, "identity.csv", sep=""), sep="|")
colnames(identity)[1] <- "finance1_cocode"
iden_colm <- c("company_name", "finance1_cocode", "owner_code", "owner_gp_name", "nic_prod_code", "nic_name", "incorporation_year")
iden <- identity[, iden_colm]

## Broad ownership groups
values.4 <- c("Private-Foreign","Co-operative","Joint sector")
names(values.4) <- c("202","203","204")
values.3 <- c("Government","Private")
names(values.3) <- c("10","20")

cmie.5.ownership <- function(s) {
 if (is.na(s)) {return(NA)}
 leading4 <- substr(s, 1, 3)
 attempt <- values.4[leading4]
 if (!is.na(attempt)) {return(attempt)}
 leading3 <- substr(s, 1, 2)
 attempt <- values.3[leading3]
 if (!is.na(attempt)) {return(attempt)}
 return("MISTAKE") 
}


possibilities <- unique(iden$owner_code)
values <- rep("", length(possibilities))
for (i in 1:length(possibilities)) {
 values[i] <- cmie.5.ownership(possibilities[i])
}
names(values) <- possibilities
iden$own.group <- values[as.character(iden$owner_code)]

## 1) NIC code
iden[,"nic_prod_code"] <- as.character(iden[,"nic_prod_code"])
k <- nchar(iden[,"nic_prod_code"])
m <- k%in%4
j <- k%in%2
iden$nic.3digit <- substr(iden[,"nic_prod_code"],1,3)
iden$nic.3digit[which(m==TRUE)] <-
  paste("0",substr(iden[,"nic_prod_code"][which(m==TRUE)],1,2),sep="")
iden$nic.3digit[which(j==TRUE)] <-
  paste("0",iden[,"nic_prod_code"][which(j==TRUE)], sep="")
table(iden$nic.3digit)
iden$nic.2digit <- substr(iden$nic.3digit, 1,2)


### Firm-year data
## Income 
income <- c("sa_finance1_cocode", "sa_company_name", "sa_finance1_year", "sa_total_income","sa_sales", "sa_industrial_sales", "sa_sales_n_chg_in_stk")
income <- sa.ann.fin[,income, with=FALSE]
# Removing negative income numbers
income <- income[-which(income$sa_total_income<0),]

## Profit
profit <- c("sa_finance1_cocode", "sa_finance1_year","sa_pat", "sa_pbdita")
profit <- sa.ann.fin[,profit, with=FALSE]
## Assets
assets <- c("sa_finance1_cocode", "sa_finance1_year", "sa_total_assets","sa_gross_fixed_assets")
assets <- sa.ann.fin[,assets, with=FALSE]
assets <- assets[-which(assets$sa_total_assets<0),]
## Borrowing
borrowing <- c("sa_finance1_cocode", "sa_finance1_year", "sa_borrowings", "sa_bank_borrowings", "sa_current_liabilities", "sa_sec_st_bank_borr" , "sa_unsec_st_bank_borr", "sa_commercial_papers" , "sa_frgn_crncy_borr")
borrowing <- sa.ann.fin[,borrowing, with=FALSE]
borrowing <- borrowing[-which(borrowing$sa_sec_st_bank_borr<0),]

fininst <- c( "sa_finance1_cocode", "sa_finance1_year","sa_st_borr_from_fin_inst", "sa_capital_employed")
fininst <- sa.ann.fin[,fininst, with=FALSE]
# Capital
ca <- c("sa_finance1_cocode" , "sa_finance1_year", "sa_current_assets")
ca <- sa.ann.fin[, ca, with=FALSE]
# Expenses        
expenses <- c("sa_finance1_cocode", "sa_finance1_year", "sa_total_expense", "sa_power_and_fuel_exp", "sa_salaries", "sa_rawmat_exp","sa_nf_operating_expenses")
expenses <- sa.ann.fin[,expenses, with=FALSE]
# Export   
export <- c("sa_finance1_cocode", "sa_finance1_year", "sa_export_goods", "sa_export_serv")
export <- sa.ann.fin[,export, with=FALSE]
# Employees 
employees <- c( "sa_finance1_cocode", "sa_finance1_year", "sa_no_of_employees")
employees <- sa.ann.fin[,employees, with=FALSE]
# Import
import <- c( "sa_finance1_cocode", "sa_finance1_year",
            colnames(sa.ann.fin)[grep("import",colnames(sa.ann.fin))])
import <- sa.ann.fin[, import, with=FALSE]


## Merge these datasets
long <- merge(income,iden, by.y=c("company_name","finance1_cocode"), by.x=c("sa_company_name", "sa_finance1_cocode"))
long <- merge(long, profit, by=c("sa_finance1_cocode","sa_finance1_year"), all.x=TRUE)
nrow(long)
long <- merge(long, assets, by=c("sa_finance1_cocode","sa_finance1_year"), all.x=TRUE)
nrow(long)
long <- merge(long, borrowing, by=c("sa_finance1_cocode","sa_finance1_year"), all.x=TRUE)
nrow(long)
long <- merge(long, fininst, by=c("sa_finance1_cocode","sa_finance1_year"), all.x=TRUE)
nrow(long)
long <- merge(long, ca, by=c("sa_finance1_cocode","sa_finance1_year"), all.x=TRUE)
nrow(long)
long <- merge(long, expenses, by=c("sa_finance1_cocode","sa_finance1_year"), all.x=TRUE)
nrow(long)
long <- merge(long, export, by=c("sa_finance1_cocode","sa_finance1_year"), all.x=TRUE)
nrow(long)
long <- merge(long, employees, by=c("sa_finance1_cocode","sa_finance1_year"), all.x=TRUE)
nrow(long)
long <- merge(long, import, by=c("sa_finance1_cocode","sa_finance1_year"), all.x=TRUE)
nrow(long)
    
long <- long[-which(is.na(long[,"sa_total_income"])|
                    is.na(long[,"sa_total_assets"])),]
long <- data.frame(long)
long <- long[,c(1,3,2,4:ncol(long))]

long$sa_company_name <- as.character(long$sa_company_name)
rm(sa.ann.fin)

long <- slotYear(long)



### Calculate age
long$year <- substr(as.character(long$sa_finance1_year), 1,4)
long$age <- as.numeric(as.character(long$year))- as.numeric(long$incorporation_year)

## TODO: negative age 

save(long, file="../DATA/long.rda")




















    
