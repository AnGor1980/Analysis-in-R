library(dplyr)
all_to_factor <- function(x){
  mutate_each(x, funs(as.factor(.)))
}
x <- mtcars
x$cyl <- as.factor(x$cyl)

test_data <- as.data.frame(list(V1 = c(-0.4, 1.2, 0.9, 0.1, -1.5), V2 = c(0.4, -0.3, -1.5, -0.5, 0.6), V3 = c(0.4, 0.1, -1, -1.8, 1.4), V4 = c(-0.5, -0.3, -1.9, -0.3, 0.8), V5 = c("A", "B", "A", "B", "A")))
test_data <- as_data_frame(test_data)
m_f <-function(x)  if(is.numeric(x)) log(1 + (x-min(x))/(max(x)-min(x))) else x
mutate_each(select(test_data,1,2), funs(-.))
mutate_each(test_data, funs(m_f))
str(test_data)
#3
test_data <- read.csv("https://stepic.org/media/attachments/course/724/salary.csv")
test_data <- as_data_frame(test_data)
test_data <- group_by_if(test_data, is.factor)

summarise(test_data, 
          n = n()
          , mean = mean(salary, na.rm = T)
          , sd = sd(salary, na.rm = T)
          , median = median(salary, na.rm = T)
          , first_quartile = quantile(salary, na.rm = T)[['25%']]
          , third_quartile = quantile(salary, na.rm = T)[['75%']]
          , na_values = sum(ifelse(is.na(salary),1,0)) )
#4
test_data <- as_data_frame(mtcars[1:4])
factors <-  c(1, 3)
factors <- names(test_data[factors])

test_data <- mutate_at(.tbl = test_data, .vars = factors, .funs = function(x) ifelse(x > mean(x), 1, 0)) 
str(test_data) <- mutate_at(.tbl = test_data, .vars = factors, .funs = as.factor)
#mutate_(test_data, new_var = interp(~ifelse(var > mean(var), 1, 0), var = as.name(num_var)))

to_factors <- function(test_data, factors){
  factors <- names(test_data[factors])    
  test_data <- mutate_at(.tbl = test_data, .vars = factors, .funs = function(x) ifelse(x > mean(x), 1, 0))
  test_data <- mutate_at(.tbl = test_data, .vars = factors, .funs = as.factor)
  return(test_data) 
}
to_factors(as_data_frame(mtcars[1:4]), c(1, 3))
#5
library(ggplot2)
d <- as_data_frame(diamonds)
d <- group_by(.data = d, color)
d <- arrange(d, desc(price))
slice(select(d, color, price) , 1:10)
d %>% 
  group_by(color) %>% 
  arrange(desc(price)) %>% 
  select(color, price) %>% 
  slice(1:10)
library(ggplot2)
d <- as_data_frame(diamonds)
high_price <- d %>% 
  group_by(color) %>% 
  arrange(desc(price)) %>% 
  select(color, price) %>% 
  slice(1:10)
