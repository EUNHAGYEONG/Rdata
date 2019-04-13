#결측지 실습
#mpg 데이터 원본에는 결측치가 없습니다. 
#우선 mpg 데이터를 불러와 일부러 몇 개의 값을 결측치로 만들겠습니다.
#hwy 변수에 NA를 할당하겠습니다.

library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
mpg_cp <- mpg
mpg_cp[c(65, 124, 131, 153, 212), "hwy"] <- NA      #65, 124, 131, 153, 212행의 hwy 변수에 NA 할당

#drv(구동 방식) 별로 hwy(고속도로 연비) 평균이 어떻게 다른지 알아보려고 합니다. 
#분석을 하기 전에 우선 두 변수에 결측치가 있는지 확인해야 하니다.
#drv 변수와 hwy 변수에 결측치가 볓 개 있는지 알아보세요.
table(is.na(mpg_cp$drv))
table(is.na(mpg_cp$hwy))


#filter()를 이용해 hwy 변수의 결측치를 제외하고, 어떤 구동 방식의 hwy 평균이 가장 높은지 알아보세요.
#하나의 dplyr 구문으로 만들어야 합니다.

library(dplyr)
mpg_cp %>% filter(!is.na(mpg_cp$hwy)) %>% 
  group_by(drv) %>% 
  summarise(hwy_mean = mean(hwy)) %>% 
  arrange(desc(hwy_mean))

#f 구동방식의 hwy 평균이 가장 높다.