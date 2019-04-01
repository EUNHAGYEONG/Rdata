install.packages("readxl")
library(readxl)




df_std_exam <- read_excel("std_exam.xlsx")
df_std_exam


math_m <- mean(df_std_exam$math)
math_m
english_m <- mean(df_std_exam$english)
english_m
