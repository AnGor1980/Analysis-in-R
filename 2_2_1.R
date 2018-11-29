library(ggplot2)
#1
depth_hist <- qplot(diamonds$depth)
#2
price_carat_clarity_points <- qplot(x = carat
      ,y = price
      ,color = clarity
      ,data =  diamonds
      ,geom = "point")
#3-4
x_density <- qplot(x = x
      ,color = cut
      ,data =  diamonds
      ,geom = "density")
x_density
#5
price_violin <- qplot(x = color
                      , y = price
                      ,data =  diamonds
                      ,geom = "violin")
