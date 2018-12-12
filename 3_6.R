#1
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
smart_hclust<-  function(test_data, cluster_number){
  fit <- hclust(dist(test_data))
  test_data$cluster <- as.factor(cutree(fit,cluster_number))
  test_data
}
fit <- hclust(dist(test_data))
plot(fit)
#2
test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")  

get_difference<-  function(test_data, n_cluster){
  smart_hclust<-  function(test_data, cluster_number){
    fit <- hclust(dist(test_data))
    test_data$cluster <- as.factor(cutree(fit,cluster_number))
    test_data
  }    
  ncol <- names(sapply(test_data, is.numeric))
  pv <- sapply(ncol, function(x,y){
    y <- smart_hclust(y, n_cluster)
    simple_fit <- lm(y[[x]]~y[['cluster']])
    pv <- anova(simple_fit)$`Pr(>F)`
    pv[!is.na(pv)]
  }, test_data)
  names(pv[pv<0.05])
}

get_difference(test_data1,2)
get_difference(test_data2,2)

#3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
get_pc <- function(d){
  
  pc <- prcomp(d)
  d$PC1 <-  pc$x[,'PC1']
  d$PC2 <-  pc$x[,'PC2']
  return(d)
}
get_pc(test_data)

#4
test <- swiss
fit <- prcomp(test)
sf <- summary(fit)
p <- 0
i <- 1
oldnames <- names(test)
pcnames <- colnames(fit$x)
pcn <- c()
while(p<0.90){
  p <- sf$importance[3,i]  
  pcn <- c(pcn, pcnames[i])
  test<- cbind(test, fit$x[,i])
  i <- i + 1
}
names(test) <- c(oldnames,pcn)
test
get_pca2 <- function(test_data){    
  fit <- prcomp(test_data)    
  cum_prop <- summary(fit)$importance['Cumulative Proportion',]    
  test_data <- cbind(test_data, fit$x[,1:min(which(cum_prop>0.9))])    
  return(test_data)    
}
get_pca2(swiss)

#5
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
test_data <- as.data.frame(list(V1 = c(16, 7, 12, 2, -2), V2 = c(4, -5, 3, 10, 37), V3 = c(-5, -2, 12, -2, -3), V4 = c(8, -1, 7, 14, 41), V5 = c(12, 9, -5, 9, 10)))
ct <- cor(test_data)
ct <- ct - diag(1, ncol(ct)) 
ct <- round(ct,2)
ct <- ifelse(abs(ct) == 1.00, row.names(ct), '')
if(length(ct[ct!=''])==0){
  "There is no collinearity in the data"
}
else{
  ct[ct!='']
}

ds <- smart_hclust(swiss, 2)
library(ggplot2)
my_plot <- ggplot(ds, aes(Education, Catholic, col = cluster))+
  geom_point() +
  stat_smooth(method="lm")
  