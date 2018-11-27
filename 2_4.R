#1
NA.position <- function(x)
{
  is_na <- is.na(x)
  ret_v <- c()
  for(i in 1:(length(x)))
  {
    if(is_na[i])
    {
      ret_v <- c(ret_v,i)  
    }
    
  }
  return(ret_v)
}
my_vector <- c(1, 2, 3, NA, NA)
NA.position(my_vector)
#2
NA.counter <- function(x)
{
  is_na <- is.na(x)
  return(sum(replace(is_na, list = is_na, values = 1)))
}
NA.counter(my_vector)
#3
filtered.sum <- function(x)
{
  sum(ifelse(x < 0 | is.na(x), 0, x))
}
x <- c(1, -2, 3, NA, NA)
filtered.sum(x)
#4
outliers.rm <- function(x)
{
  iqr_loc <- IQR(x) * 1.5
  q <- quantile(x, probs = c(0.25, 0.75))
  x <- x[(x < iqr_loc + q[2]) & (x > q[1] - iqr_loc)]
  return(x)
}
x <- rnorm(1000)
boxplot(x)
x <- outliers.rm(x)
boxplot(x)
