#1
dt <- ToothGrowth
dt$len[dt$supp == 'OJ' & dt$dose == 0.5]
t_stat <- t.test(dt$len[dt$supp == 'OJ' & dt$dose == 0.5]
       , dt$len[dt$supp == 'VC' & dt$dose == 2])$statistic
#2
t.test(lekarstva$Pressure_before, lekarstva$Pressure_after, paired = T)
#3
ds <- dataset_11504_16.2
bartlett.test(V1 ~ V2, ds)
t.test(ds$V1, ds$V2)
wilcox.test(V1 ~ V2, ds)

