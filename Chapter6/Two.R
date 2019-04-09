library(dplyr)

midterm <- read.csv("df_midterm.csv")
midterm

midterm %>% mutate(total = english + math,
                   mean = (english + math) / 2)

midterm %>% mutate(total = english + math,
                   mean = (english + math) / 2,
                   test = ifelse(mean >= 60, "pass", "fail")) %>% 
  arrange(mean)

midterm %>% summarise(mean_math = mean(math))

midterm %>% group_by(class) %>%       #class(반)별로 분리
  summarise(mean_math = mean(math))   #각 class(반)별 수학 점수 평균 산출

test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))

test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 90, 85))

test1
test2

total_test <- left_join(test1, test2, by = 'id') #id 기준으로 합쳐 total_test에 할당
total_test

group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))

group_all <- bind_rows(group_a, group_b)
group_all
