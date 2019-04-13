#우선 mpg 데이터를 불러와 일부러 이상치를 만들겠습니다. 
#drv(구동 방식) 변수의 값은 4(사륜구동), f(전륜구동), r(후륜구동) 세 종류로 되어 있습니다.
#몇 개의 행에 존재할 수 없는 값 k를 할당하겠습니다.
#cty(도시 연비) 변수도 몇 개의 행에 극단적으로 크거나 작은 값을 할당하겠습니다.

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10, 14, 58, 93), "drv"] <- "k"
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)

#이상치가 들어 있는 mpg 데이터를 활용해 문제를 해결해 보세요.
#구동 방식별로 도시 연비가 다른지 알아보려고 합니다.
#분석을 하기 전에 우선 두 변수에 이상치가 있는지 확인하려고 합니다.

#Q1 -> drv에 이상치가 있는지 확인하세요.
#이상치를 결측 처리한 후 이상치가 사라졌는지 확인하세요.
#결측 처리를 할 때는 %in% 기호를 활용하세요.

mpg$drv <- ifelse(mpg$drv %in% c("4", "f", "r"), mpg$drv, NA)
table(is.na(mpg$drv))

#Q2 -> 상자 그림을 이용해 cty에 이상치가 있는지 확인하세요.
#상자 그림의 통계치를 이용해 정상 범위를 벗어난 값을 결측 처리한 후 다시 상자 그림을 만들어 이상치가 사라졌는확 ㅎ인하세요.
boxplot(mpg$cty)
boxplot(mpg$cty)$stats

mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
boxplot(mpg$cty)


#Q3 -> 두 변수의 이상치를 결측 처리 했으니 이제 분석할 차례입니다.
#이상치를 제외한 다음 drv별로 cty 평균이 어떻게 다른지 알아보세요.
#하나의 dplyr 구문으로 만들어야 합니다.

library(dplyr)
library(ggplot2)
mpg %>% filter(drv %in% c("4", "f", "r")) %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty, na.rm = T))

