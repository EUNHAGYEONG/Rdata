df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1)) # 새로운 데이터 프레임 만들기
df

df$var_sum <- df$var1 + df$var2 # var1과 var2를 더한 var_sum 파생변수 생성
df

df$var_mean <- df$var_sum / 2 #전체 평균인 var_mean 파생변수 생성
df

#mpg 통합 연비 변수 만들기
#mpg 데이터의 cty, hwy 변수를 이용하여 도로 유형을 통틀어 가장 높은 연비를 가진 자동차 모델 구하기


mpg$total <- (mpg$cty + mpg$hwy) / 2 #자동차 통합 연비를 구하기 위해 total 파생변수 생성
mpg



mean(mpg$total)

#조건문을 활용해 파생변수 만들기
#mpg 데이터를 이용해 연비가 기준치를 넘기면 합격, 넘기지 못하면 불합격을 부여

#1. 기준값 정하기
summary(mpg$total) #summary() 함수를 이용하여 total 변수의 평균과 중앙값을 확인. 평균->20.15, 중앙값->20.50
hist(mpg$total) x축->total값, y축->빈도수
#이를 통해 기준값을 20으로 정하는 것이 가장 적합할 것으로 판단됨. -> 통합연비가 20을 넘기면 합격, 아니면 불합.

mpg$test <- ifelse(mpg$total >= mean(mpg$total), "pass", "fail") #ifelse(조건, 조건에 맞을시 실행, 조건에 맞지 않을 시 실행)

head(mpg, 20) #20행까지 데이터 확인

table(mpg$test) #합격 연비 빈도표 생성

library(ggplot2) 


qplot(mpg$test) #연비 합격 빈도수 막대그래프 생성

#중첩 조건문 활용하기 -> 30이상 : A, 20~29 : B, 20미만 : C
mpg$grade <- ifelse(mpg$total >= 30, "A", 
                    ifelse(mpg$total >= 20, "B", "C"))

mpg
head(mpg)
table(mpg$grade)



qplot(mpg$grade)
                    