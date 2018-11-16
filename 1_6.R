#1
library(ggplot2)
ggplot(airquality, aes(group = Month, x = Month, y = Ozone))+
  geom_boxplot()
#2
plot1 <- ggplot(mtcars, aes(mpg, disp,col=hp))
plot1 <- plot1 + geom_point() +
  geom_point()
#+ scale_colour_gradient(low = "blue")
plot1
#3
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()
#4
ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species, size = Petal.Length)) +
   geom_point()