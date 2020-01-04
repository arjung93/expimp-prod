clear
capture log close	
global path "C:\Users\u823125\Downloads\expimp-prod-master\expimp-prod-master\STATA"
use "$path\biprobit.dta"
log using C:\Users\u823125\Downloads\expimp-prod-master\expimp-prod-master\STATA\resultsacf.log, text replace
xtset sa_finance1_cocode year

xtprobit exp lagexp lpresacf lgfa lsalary initexp i.year i.nic_2digit, re 
est store mexp1
margins, dydx(lagexp lpresacf lgfa lsalary) predict(pu0) post
est store mexp11

xtprobit exp lagexp lagimp lpresacf lgfa lsalary initexp i.year i.nic_2digit, re 
est store mexp2
margins, dydx(lagexp lagimp lpresacf lgfa lsalary) predict(pu0) post
est store mexp22

xtprobit imp lagimp lpresacf lgfa lsalary initimp i.year i.nic_2digit, re
est store mimp1
margins, dydx(lagimp lpresacf lgfa lsalary) predict(pu0) post
est store mimp11


xtprobit imp lagexp lagimp lpresacf lgfa lsalary initimp i.year i.nic_2digit, re
est store mimp2
margins, dydx(lagexp lagimp lpresacf lgfa lsalary) predict(pu0) post
est store mimp22


esttab mexp1 mexp2 mimp1 mimp2 using "$path\dynprobitacf.tex", replace drop(*.nic_2digit *.year) stats(rho sigma_u ll) 

esttab mexp11 mexp22 mimp11 mimp22 using "$path\dynprobitacfme.tex", replace

biprobit exp imp lagexp lagimp lpresacf lgfa lsalary i.year i.nic_2digit, r
est store a
esttab a using "$path/biprobitacf.tex" , replace drop(*.nic_2digit *.year) stats(rho chi2)
