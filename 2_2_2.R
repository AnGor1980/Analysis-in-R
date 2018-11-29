library(ggplot2)
#1
my_plot = ggplot(mtcars, aes(x=factor(am), y = mpg), fill = "white") +
  geom_violin() +
  geom_boxplot(width = 0.2)
#2
sales = read.csv("https://stepic.org/media/attachments/course/724/sales.csv")

my_plot = ggplot(sales, aes(x = income, y = sale)) + 
  geom_point(aes(color = shop)) +
  geom_smooth()

#3
my_plot = ggplot(sales, aes(x = shop, y = income, color = season)) + 
  stat_summary(fun.data = mean_cl_boot, position = position_dodge(0.2))
#4
my_plot = ggplot(sales, aes(x = date, y = sale, color = shop)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(0.2)) +
  stat_summary(fun.data = mean_cl_boot, geom = "point", position = position_dodge(0.2)) +
  stat_summary(fun.data = mean_cl_boot, geom = "line", position = position_dodge(0.2))
