library(dplyr)
test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")

smart_anova <- function(test_data){
shapirot <- sapply(levels(test_data$y), function(x, ds){
  shapiro.test(x = ds$x[ds$y == x])$p.value  
}, test_data)
bartlettt <- bartlett.test(x ~ y, test_data)$p.value   

if(all(shapirot > 0.05) & (bartlettt > 0.05)){
  fit <- aov(formula = x~y, data = test_data)
  p <- anova(fit)$`Pr(>F)`[1]
  names(p) <- "ANOVA"
} 
else{
  fit <- kruskal.test(formula = x~y, data = test_data)
  p <- fit$p.value
  names(p) <- "KW"
}
return(p)
}

p <- smart_anova(test_data)
p
