library(ggplot2)
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot() +
  facet_grid(am ~ vs)
#2
ggplot(iris, aes(x = Sepal.Length, y = )) +
  geom_density() +
facet_wrap(~ Species)
#3
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Species)
#4
myMovieData = read.csv("https://stepik.org/media/attachments/course/724/myMovieData.csv")
ggplot(myMovieData, aes(x = Type, y = Budget)) +
  geom_boxplot() +
  facet_grid(.~Year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
