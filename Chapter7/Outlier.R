outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier

outlier$sex <- ifelse(outlier$sex > 2, NA, outlier$sex)
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

library(dplyr)

outlier %>% filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(score_mean = mean(score))


library(ggplot2)

boxplot(mpg$hwy)$stats

mpg$hwy <- ifelse(mpg$hwy < 12 & mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))
