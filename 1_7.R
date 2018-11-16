#1
red_men <- prop.table(HairEyeColor[,'Blue','Male'], )['Red']
#2
sum(HairEyeColor[,'Green','Female'])
#3

library("ggplot2")
mydata <- as.data.frame(HairEyeColor[,,'Female'])
obj <- ggplot(data = mydata, aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity",position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
#4

chisq.test(HairEyeColor['Brown',,'Female'])
#5
dt <- table(data.frame(diamonds['cut'], diamonds['color']))
main_stat <-chisq.test(dt)$statistic[['X-squared']]
#6
d6 <- data.frame(diamonds['price'], diamonds['carat'])
fk<-d6$price >= mean(d6$price)
d6$factor_price<-replace(x = fk,list = fk, values = 1 )
fk<-d6$carat >= mean(d6$carat)
d6$factor_carat <-replace(x = fk,list = fk, values = 1 )
t6 <- table(d6$factor_price,d6$factor_carat)
main_stat <- chisq.test(t6)$statistic[['X-squared']]
#7
fisher_test = fisher.test(mtcars$am, mtcars$vs)$p.value
