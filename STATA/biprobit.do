xtset sa_finance1_cocode year
biprobit exp imp lagexp lagimp l.lpres l.lgfa l.lsalary l.lsales i.year i.nic_2digit , r
eststo a
esttab a, keep(i.nic_2digit)


