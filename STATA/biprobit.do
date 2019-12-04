global path "C:\Users\u823125\Downloads\expimp-prod-master\expimp-prod-master\STATA"
use "$path\biprobit.dta"

xtset sa_finance1_cocode year

xtprobit exp lagexp l.lpres l.lgfa l.lsalary initexp i.year i.nic_2digit, re 
est store mexp1

xtprobit exp lagexp lagimp l.lpres l.lgfa l.lsalary initexp i.year i.nic_2digit, re 
est store mexp2

xtprobit imp lagimp l.lpres l.lgfa l.lsalary initimp i.year i.nic_2digit, re
est store mimp1


xtprobit imp lagexp lagimp l.lpres l.lgfa l.lsalary initimp i.year i.nic_2digit, re
est store mimp2


esttab mexp1 mexp2 mimp1 mimp2 using "$path\dynprobit.tex", replace drop(*.nic_2digit *.year) stats(rho sigma_u ll) 


biprobit exp imp lagexp lagimp   l.lpres l.lgfa l.lsalary i.year i.nic_2digit , r
est store a
esttab a using "$path/biprobit.tex" , replace drop(*.nic_2digit *.year) stats(rho chi2)


