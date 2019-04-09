library("ggplot2")
library("dplyr")

cp_midwest <- midwest
cp_midwest

#Q1. popadults는 해당 지역의 성인 인구, poptotal은 전체 인구를 나타냅니다. midwest 데이터에 '전체 인구 대비 미성년 인구 백분율' 변수를 추가하세요.
cp_midwest <- cp_midwest %>% mutate(popnonadults_perc = 100 - (popadults / poptotal * 100))
cp_midwest
View(cp_midwest)

#Q2. 미성년 인구 백분율이 가장 높은 상위 5개 county(지역)의 미성년 인구 백분율을 출력하세요.
cp_midwest %>% group_by(county) %>% 
  arrange(desc(mean_popnonadults_perc)) %>% 
  head(5)

#Q3. 분류표의 기준에 따라 미성년 비율 등급 변수를 추가하고, 각 등급에 몇 개의 지역이 있는지 알아보세요.
cp_midwest <- cp_midwest %>% mutate(nonadults_rank = ifelse(popnonadults_perc >= 40, "large", 
                                                            ifelse(popnonadults_perc >=30 & popnonadults_perc < 40, "middle", "small")))
cp_midwest %>% group_by(nonadults_rank) %>% 
  summarise(county_nonadults_rank = n())
                                
#Q4.popasian은 해당 지역의 아시아인 인구를 나타냅니다. '전체 인구 대비 아시아인 인구 백분율' 변수를 추가하고 하위 10개 지역의 state(주), 
#county(지역), 아시아인 인구 백분율을 출력하세요.

cp_midwest <- cp_midwest %>% mutate(popasian_perc = popasian / poptotal * 100)
cp_midwest %>% select(state, county, popasian_perc) %>% 
  arrange(popasian_perc) %>% 
  head(10)
