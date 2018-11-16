#1
df  <- mtcars
result <- mean(df$qsec[df$cyl != 3 & df$mpg > 20])
#2
descriptions_stat = aggregate(cbind(hp, disp) ~ am, mtcars, sd)
#3
air1 <-  subset(airquality, Month %in% c(7,8,9))
result = aggregate(Ozone ~ Ozone, data = subset(air1, !is.na(Ozone)),  FUN = length)
result1 = aggregate(Ozone ~ Month, data = air1,  FUN = length)
#4
library(psych)
describeBy(x = airquality, group = airquality$Month)
#5
dd = describeBy(x = iris)
#6
my_vector <- rnorm(30)

my_vector[sample(1:30, 10)] <- NA # ???? ???????????? ?????????????????? ?????????????? ???????????????? NA
replace(x=my_vector, list = is.na(my_vector), values = mean(my_vector[!is.na(my_vector)]))
