#1
library(data.table)
sample.products <- data.table(price = c(10000, 600000, 700000, 1000000),
                              brand = c("a", "b", "c", "d"),
                              available = c(T, T, F, T))
filter_d <- c("a", "c", "d")
sample.products[brand %in% filter_d & price >= 500000 & available == T]
#2
sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
sample.purchases[order(-price)][  
  quantity > 0
  ,list(ordernumber, product_id)
]
#3
sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
sample.purchases[quantity >= 0, ][
                ,.(price = sum(price*quantity))  
                , by = ordernumber
                ][,.(price = median(price))]
