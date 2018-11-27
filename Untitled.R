#1
corr.calc <- function(x)
{
  xc <- cor.test(x[[1]], x[[2]])
  return(c(xc$estimate, xc$p.value))
}
corr.calc(mtcars[, c(1,5)]) 
#2

step6 <-  read.table("step6.csv",  header=TRUE, sep=',' )
str(step6)
names(step6)
typeof(step6$V1)
filtered.cor <- function(x)
{
  library('psych')
  dcol = c()
  for(i in names(x))
  {
    if(typeof(x[[i]]) == "double")
    {
      dcol <- c(dcol, i)
    }
  }
  dx = x[,dcol]
  fit <- corr.test(dx)
  r <- fit$r - diag(nrow(fit$r))
  return(r[which.max(abs(r))])
}
fit$r
which.max(abs(fit$r[fit$r!=1]))
str(fit)
fit$r[fit$r!=1]
filtered.cor(step6)
test_data <- as.data.frame(list(V6 = c("c", "c", "c", "c", "c", "c", "c", "c"), V4 = c("r", "r", "r", "r", "r", "r", "r", "r"), V2 = c(1, 0.2, -0.8, -0.6, -0.9, 0.4, -0.8, -1.1), V5 = c("d", "d", "d", "d", "d", "d", "d", "d"), V1 = c(0.6, -1.3, 1.4, 0.1, 0.9, 0.9, -1.4, 0.2), V7 = c("e", "e", "e", "e", "e", "e", "e", "e"), V3 = c(0.6, -1.3, 1.4, 0.1, 0.9, 0.9, -1.4, 0.2)))
filtered.cor(test_data)
#3
s <- shapiro.test(test_data[[3]])
s$p.value
smart_cor <- function(x)
{
  pirs <- TRUE
  for(i in names(x))
  {
    pirs <- pirs & (shapiro.test(x[[i]])$p.value >= 0.05)
  }
  if(pirs)
  {
    return(cor(x)[2])
  }
  else
  {
    return(cor(x,method = 'spearman',)[2])
  }
}
test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
smart_cor(test_data)
#4
df  <- read.table('dataset_11508_12.txt')
fit  <- lm(V1 ~ V2, df)
summary(fit)
#5
library('ggplot2')
df <- diamonds[diamonds$carat == 0.46 & diamonds$cut == 'Ideal',]
fit <- lm(price ~ depth, df)
fit_coef <- fit$coefficients
summary(fit)
#6
regr.calc <- function(x)
{
  pirs <- TRUE
  for(i in names(x))
  {
    pirs <- pirs & (shapiro.test(x[[i]])$p.value >= 0.05)
  }
  if(pirs)
  {
    p <- cor.test(x[[1]] + x[[2]])$p.value
  }
  else
  {
    p <- cor.test(x[[1]], x[[2]])$p.value
    #p <- cor.test(x[[1]] + x[[2]],method = 'spearman',)$p.value
  }
  if(p < 0.05)
  {
    f <- lm(x[[1]] ~ x[[2]])
    x$fit <- predict(f,x)
    return(x)
  }
  else
  {
    return("There is no sense in prediction")
  }
}
test_data <- as.data.frame(list(V1 = c(3.84, 7.44, 4.25, 4.83, 3.44, 4.58, 4.88, 4.75, 5.29, 4.76), V2 = c(4.46, 7.51, 3.54, 5.85, 5.1, 5.38, 5.1, 4.06, 5.62, 3.74)))
df = iris[,1:2]
lm(df[[1]] ~ df[[2]])
regr.calc(test_data)
#7
my_plot = ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species))+
  geom_point(size = 2) + 
  geom_smooth(method = "lm")
  
