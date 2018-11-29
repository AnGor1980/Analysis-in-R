#1
product.category <- data.table(product_id = c(1,1,2,2,3),
                               category_id = c(1,2,1,3,3))
purchases <- data.table(product_id = c(1, 2, 3),
                        totalcents = c(100, 200, 300),
                        quantity = c(1, 1, 3))
setkey(product.category, product_id)
setkey(purchases, product_id)

ttl <- merge(product.category, purchases, by = 'product_id' )
ttl[,.(totalcents = sum(totalcents), quantity = sum(quantity)), by = category_id]
#2
library(data.table)

sample.purchases <- data.table(price = c(100, 300, 50, 700, 30),
                               ordernumber = c(1,1,1,2,3),
                               quantity = c(1,1,2,1,-1),
                               product_id = 1:5)

sample.purchases <- sample.purchases[quantity >= 0][,price.portion := sprintf("%.2f", price*quantity/sum(price*quantity)*100), by = ordernumber]
format(c(2.315, 0.043, 96.168, 0.297, 1.177, 0.632), digits = 1, nsmall  = 2)
x <- c(2.315, 0.043, 96.168, 0.297, 1.177, 0.632)
sprintf(fmt = "%.2f",2.315)
n <- 1:18
sprintf(paste0("e with %2d digits = %.", n, "g"), n, exp(1))
