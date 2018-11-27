library(dplyr)
library(ggplot2)
#1
seq(from=1, to = 10,by = 2)
slice(diamonds, seq(from=1, to = nrow(diamonds),by = 2))
diamonds
#2
my_df <- mtcars %>% 
  filter( mpg > 14, hp > 100) %>% 
  arrange(desc(mpg)) %>% 
  slice(1:10) %>% 
  select(mpg, hp, am, vs) %>% 
  rename('Miles per gallon' = mpg, 'Gross horsepower' = hp)
my_df
