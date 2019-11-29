xtset sa_finance1_cocode year

xtprobit exp lagexp l.lpres l.lgfa l.lsalary initexp i.year i.nic_2digit, re 
estimates store m1

xtprobit imp lagimp l.lpres l.lgfa l.lsalary initimp i.year i.nic_2digit, re
est sto m2

esttab 


g both= lagexp* lagimp
biprobit exp imp lagexp lagimp  both l.lpres l.lgfa l.lsalary i.year i.nic_2digit , r
eststo a
esttab a, keep(i.nic_2digit)


