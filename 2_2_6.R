teapot <- read.csv("https://stepic.org/media/attachments/course/724/teapot.csv", sep = ";")
library(plotly)
library(data.table)
teapot
mesh <- data.table(
  x = rnorm(40),
  y = rnorm(40),
  z = rnorm(40)
)
i = teapot[1:3,1]
j = teapot[1:3,2]
k = teapot[1:3,3]
plot_ly(teapot[1:3,], type="mesh3d", x = ~x, y = ~y, z = ~z, i = ~i, j = ~j, k = ~k)


i.s <- seq(from = 0, to=nrow(teapot)-1, by = 3)
j.s <- seq(from = 1, to=nrow(teapot), by = 3)
k.s <- seq(from = 2, to=nrow(teapot), by = 3)
plot_ly(teapot, x = ~x, y = ~y, z = ~z
        , i = ~i.s, j = ~j.s, k = ~k.s
        , type = "mesh3d")
make.fancy.teapot <- function(teapot.coords) {
  i.s <- seq(from = 0, to=nrow(teapot.coords)-1, by = 3)
  j.s <- seq(from = 1, to=nrow(teapot.coords), by = 3)
  k.s <- seq(from = 2, to=nrow(teapot.coords), by = 3)
  plot_ly(teapot.coords, x = ~x, y = ~y, z = ~z
          , i = ~i.s, j = ~j.s, k = ~k.s
          , type = "mesh3d")
}
teapot <- as.data.table(teapot)
make.fancy.teapot(teapot)
