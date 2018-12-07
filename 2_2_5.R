library(ggplot2)

iris_plot <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, 
                              color = factor(Species)))+
  geom_point() +
  geom_smooth(method = "lm")+
  scale_color_discrete(name = "Вид цветка", labels = c("Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский"))+
  scale_x_continuous(name = "Длина чашелистика", breaks = seq(from= 4, to = 8 ,by = 1), limits = c(4,8))+ 
  scale_y_continuous(name = "Длина лепестка", breaks = seq(from= 1, to = 7 ,by = 1))
