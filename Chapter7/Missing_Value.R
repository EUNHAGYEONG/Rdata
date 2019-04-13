#결측ㅈ

df <- data.frame(sex = c("M", "F", NA, "M", "F"), 
                 score = c(5, 4, 3, 4, NA))

df
is.na(df)

table(is.na(df$sex))
table(is.na(df$score))
table(is.na(df))

table(is.na(df$sex)) 

table(is.na(df$score))


library(dplyr)

df %>% filter(is.na(score))
df %>% filter(!is.na(score))

df_nomiss <- df %>% filter(!is.na(score))
mean(df_nomiss$score)
d

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss

df_nomiss <- na.omit(df)
df_nomiss


mean(df$score, na.rm = T)
sum(df$score, na.rm = T)

mid_exam <- read.csv("df_midterm.csv")
mid_exam

mid_exam[c(1, 3), "math"] <- NA
mid_exam

mid_exam %>% summarise(mean_math = mean(math, na.rm = T))

mid_exam %>% summarise(mean_math = mean(math, na.rm = T),
                       sum_math = sum(math, na.rm = T),
                       median_math = median(math, na.rm = T))

mid_exam$math <- ifelse(is.na(mid_exam$math), 40, mid_exam$math)
table(is.na(mid_exam$math))
mid_exam

mean(mid_exam$math)
