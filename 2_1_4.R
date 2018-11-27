#1
d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))

lapply(d, function(x){ 
  x[is.na(x)] <- 0
  sum(x[x>0])})

#2
test_data <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")
my_names <- function(dataset, names){
  m <- sapply(names, grepl, dataset$name)
  x <- apply(m*dataset$expression, 2, sum)
  x1 <- as.data.frame(x)
  x1$name <- names(x)
  x1$expression <- x1$x
  x1$x <- NULL
  return(x1)
}
x <- my_names(test_data, names)
#3
t <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")
find_outliers <- function(t){
  test_data <- t
  num_col <- names(test_data[sapply(test_data, is.numeric)])
  orig_names <- names(test_data)
  meand <- aggregate(  x = test_data[sapply(test_data, is.numeric)]
                       , by = test_data[sapply(test_data, is.factor)]
                       , data = test_data
                       , FUN=mean)
  sdd <- aggregate(  x = test_data[sapply(test_data, is.numeric)]
                     , by = test_data[sapply(test_data, is.factor)]
                     #, data = test_data
                     , FUN=sd)
  sdd$sd <- sdd[num_col]*2
  sdd$msd <- -sdd[num_col]*2
  sd_mean <- merge(x = meand, y = sdd, by = names(meand)[sapply(meand, is.factor)])
  sd_mean$outl <- sd_mean[paste(num_col,'x', sep = '.')] + sdd$sd
  sd_mean$moutl <- sd_mean[paste(num_col,'x', sep = '.')] + sdd$msd
  test_data <- merge(x = test_data, y = sd_mean, by = names(test_data)[sapply(test_data, is.factor)])
  test_data$is_outlier <- ifelse(test_data[num_col] > test_data$outl | test_data[num_col] < test_data$moutl, 1, 0)
  test_data <- test_data[c(orig_names, 'is_outlier')]
  return(test_data)
}

test_data <- read.csv("https://stepic.org/media/attachments/course/724/test_data_2.csv")
t <- find_outliers(test_data)
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/test_data_2_ans.csv")
sum(t$is_outlier)
sum(correct_answer$is_outlier)
test_data <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")
t <- find_outliers(test_data)
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/hard_task_ans.csv")
t1 <- subset(t, t$is_outlier==1)
correct_answer1 <- subset(correct_answer, correct_answer$is_outlier == 1)
str(correct_answer)

#4

test_data <- read.csv("https://stepik.org/media/attachments/course/724/test.csv")
test_data <- swiss
test_data <- data.frame(x = 1:100, y = 1:100, z = 1:100)

smart_lm <- function(x){
  x <- sapply(test_data, shapiro.test)
  test_data <- test_data[sapply(as.data.frame(x), function(xf) xf$p.value > 0.05 )]
  if(ncol(test_data) > 0){
    lm(data = test_data)[['coefficients']]
  }else{
    print("There are no normal variables in the data")
  }
    
}
dddd <- smart_lm(test_data)
#5
lst <-  lapply(iris[sapply(iris, is.numeric)], t.test, mu = 4)
lapply(lst, function(x) c(x$statistic, x$parameter, x$p.value))

one_sample_t <- function(test_data, general_mean){
  lst <-  lapply(test_data[sapply(test_data, is.numeric)], t.test, mu = general_mean)
  lapply(lst, function(x) c(x$statistic, x$parameter, x$p.value))
}
test_data <- as.data.frame(list(V1 = c(42, 39, 47, 23, 31, 44, 39, 48, 35, 50), V2 = c(24, 48, 42, 19, 44, 43, 28, 46, 31, 38), V3 = c(34, 25, 32, 34, 26, 38, 35, 49, 39, 57), V4 = c("B", "A", "B", "B", "B", "B", "B", "B", "A", "A"), V5 = c("B", "A", "B", "B", "A", "B", "A", "A", "A", "A")))
one_sample_t(test_data, 7)
#6
normality_tests <- lapply(iris[, 1:4], shapiro.test)
lapply(normality_tests, function(x) c(x$p.value))
