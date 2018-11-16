#1-2
ds <- npk
fit <- aov(yield ~ N + P + K, data = ds )
summary(fit)
#3
ds <- iris
fit <- aov(Sepal.Width ~ Species, data = ds)

TukeyHSD(fit)
#4
ds <- Pillulkin
str(ds)
ds$patient <- as.factor(ds$patient)
fit <- aov(temperature ~ pill , data = ds ) 
fit <- aov(temperature ~ (pill + doctor + patient)^2, data = ds ) 
summary(fit)
#5
library(ggplot2)
pd = position_dodge(0.1)
ds <- ToothGrowth
obj <- ggplot(ds, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2)) 
obj
