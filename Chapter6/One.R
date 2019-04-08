install.packages("dplyr")
library(dplyr)

install.packages("readxl")
library(readxl)

Busan <- read.csv("부산광역시_사상구_모범음식점현황_20190213.csv")
Busan
View(Busan)

Busan %>% filter(업태 == '한식')

Busan %>% filter(업태 != '한식')

Busan %>% filter(업태 == '한식' & 메뉴 == '생선회')

Busan %>% filter(업태 == '한식' | 메뉴 == '초밥')

Busan %>% filter(업태 == '중국식' | 업태 == '일본식' | 업태 == '뷔페식')

Busan %>% filter(업태 %in% c('중국식', '일본식', '뷔페식'))

Busan %>% select(업소명, 전화번호)

Busan %>% select(-업소명, -전화번호)

Busan %>% filter(업태 == '한식') %>% 
  select('업소명', '메뉴') %>% 
  head()

Busan %>% arrange(업소명)

Busan %>% arrange(최초지정일자)
