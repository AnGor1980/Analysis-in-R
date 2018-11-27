#1
test_data <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), V2 = c(-0.3, NA, -2, -1.2), V3 = c(1, 2, 3, NA)))
test_data <- as.data.frame(list(V1 = c(0.4, -0.2, -0.9, 0, -0.2, NA, NA, NA), V2 = c(0.4, -0.8, 0, -0.8, -0.7, NA, NA, NA), V3 = c(0, 0.1, NA, -0.8, 0.4, NA, NA, NA), V4 = c(0.5, 0.6, 1.4, 0.1, 0.8, NA, NA, NA), V5 = c(-2, NA, 0.2, -0.6, 0.3, NA, NA, NA), V6 = c(-0.7, 0.1, 0.4, -0.2, -0.6, NA, NA, NA), V7 = c(1.6, 1.8, 0.1, 0.2, 1.3, NA, NA, NA), V8 = c(-1.6, 0.2, 0.6, -0.5, -0.8, NA, NA, NA)))
get_negative_values <- function(x){
  my_list <- apply(x, 2, function(x) x[ifelse(is.na(x),0,x) < 0])
  my_list <- Filter(length, my_list)
  sz <- unlist(lapply(my_list, length))
  if(min(sz) == max(sz)){
    matrix(unlist(my_list), ncol = length(my_list))
  }
  else{
    my_list
  }
}
my_list <- get_negative_values(test_data)
test_data <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), V2 = c(-0.3, NA, -2, -1.2), V3 = c(1, 2, 3, NA)))
#2
test_data <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))
na_rm <- function(x){
  
}
#<- apply(test_data, 2, mean, na.rm = T)
as.data.frame(apply(test_data, 2, function(x) ifelse(is.na(x), x[is.na(x)] <- mean(x,na.rm = T), x)))

