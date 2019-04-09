#1.자동차 배기량에 따라 고속도로 연비가 다른지 알아보려고 합니다. displ(배기량)이 4 이하인 자동차와 5 이상인 자동차 중 어떤 자동차의 hwy(고속도로 연비)
#가 평균적으로 더 높은지 알아보세요.
library("dplyr")
install.packages("ggplot2")
library("ggplot2")

mpg <- as.data.frame(ggplot2::mpg)

View(mpg)

displ_under4 <- mpg %>% filter(displ <= 4) #displ_under4 : 배기량이 4 이하인 자동차만 추출
displ_under4


displ_over5 <- mpg %>% filter(displ >= 5) #displ_over5 : 배기량이 5 이상인 자동차만 추추
displ_over5

mean(displ_under4$hwy)
mean(displ_over5$hwy)

# 배기량이 4 이하인 자동차의 hwy가 더 높다.

#2.자동차 제조 회사에 따라 도시 연비가 다른지 알아보려고 합니다. "audi"와 "toyota" 중 어느 manufacturer(자동차 제조 회사)dml cty(도시 연비)가 평균적으로
#더 높은지 알아보세요.

audi <- mpg %>% filter(manufacturer == "audi")
toyota <- mpg %>% filter(manufacturer == "toyota")

mean(audi$cty)
mean(toyota$cty)

# cty는 toyota 회사가 더 높다.

#3."chevrolet", "ford", "honda" 자동차의 고속도로 연비 평균을 알아보려고 합니다. 이 회사들의 데이터를 추출한 후 hwy 전체 평균을 구해보세요.
chevrolet <- mpg %>% filter(manufacturer == "chevrolet")
ford <- mpg %>% filter(manufacturer == "ford")
honda <- mpg %>% filter(manufacturer == "honda")

three_datas <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(three_datas$hwy)

cfh_datas <- mpg %>% filter(manufacturer == "chevrolet" | manufacturer == "ford" | manufacturer == "honda")
mean(cfh_datas$hwy)

#22.50943

#1.mpg 데이터는 11개 변수로 구성되어 있습니다. 이 중 일부만 추출해 분석에 활용하려고 합니다. mpg 데이터에서 class(자동차 종류), cty(도시 연비) 변수를
#추출해 새로운 데이터를 만드세요. 새로 만든 데이터의 일부를 출력해 두 변수로만 구성되어 있는지 확인하세요.

new_mpg <- mpg %>% select(class, cty)
new_mpg

#2.자동차 종류에 따라 도시 연비가 다른지 알아보려고 합니다. 앞에서 추출한 데이터를 이용해 class(자동차 종류)가 "audi"인 자동차와 "compact"인 자동차 중 
#어떤 자동차의 cty(도시 연비)가 더 높은지 알아보세요.

suv_cty <- new_mpg %>% filter(class == "suv")
compact_cty <- new_mpg %>% filter(class == "compact")

mean(suv_cty$cty)
mean(compact_cty$cty)

#compact가 더 높다.

#1."audi"에서 생산한 자동차 중에 어떤 자동차 모델의 hwy(고속도로 연비)가 높은지 알아보려고 합니다. "audi"에서 생산한 자동차 중 hwy가 1~5위에 해당하는
#자동차의 데이터를 출력하세요.
audi <- mpg %>% filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)
audi

#mpg 데이터는 연비를 나타내는 변수가 hwy(고속도로 연비), cty(도시 연비) 두 종류로 분리되어 있습니다. 두 변수를 각각 활용하는 대신 하나의 통합 연비
#변수를 만들어 분석하려고 합니다.
#1.mpg() 데이터 복사본을 만들고, cty와 hwy를 더한 '합산 연비 변수'를 추가하세요.
cp_mpg <- mpg 

cp_mpg <- cp_mpg %>% mutate(sum_fuel = hwy + cty)
cp_mpg

#2.앞에서 만든 '합산 연비 변수'를 2로 나눠 '평균 연비 변수'를 추가하세요.
cp_mpg <- cp_mpg %>% mutate(mean_fuel = sum_fuel / 2)
cp_mpg

#3.'평균 연비 변수'가 가장 높은 자동차 3종의 데이터를 출력하세요.
cp_mpg %>% arrange(desc(mean_fuel)) %>% 
  head(3)

#4.1~3번 문제를 해결할 수 있는 하나로 연결된 dplyr 구문을 만들어 실행해보세요. 데이터는 복사본 대신 mpg 원본을 이용하세요.
mpg %>% mutate(sum_fuel = hwy + cty) %>% 
  mutate(mean_fuel = sum_fuel / 2) %>% 
  arrange(desc(mean_fuel)) %>% 
  head(3)

#1.mpg 데이터의 class는 "suv", "compact" 등 자동차를 특징에 따라 일곱 종류로 분류한 변수입니다. 어떤 차종의 도시 연비가 높은지 비교해 보려고 합니다.
#class별 cty 평균을 구해보세요.
mpg %>% group_by(class) %>% 
  summarise(mean_cty = mean(cty))

#2.앞 문제의 출력 결과는 class 값 알파벳 순으로 정렬되어 있습니다. 어떤 차종의 도시 연비가 높은지 쉽게 알아볼 수 있도록 cty 평균이 높은 순으로
#정렬해 출력하세요.
mpg %>% group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))

#3.어떤 회사 자동차의 hwy(고속도로 연비)가 가장 높은지 알아보려고 합니다. hwy 평균이 가장 높은 회사 세 곳을 출력하세요.
mpg %>% group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)

#4.어떤 회사에서 "compact"(경차) 차종을 가장 많이 생산하는지 알아보려고 합니다. 각 회사별 "compact" 차종 수를 내림차순으로 정렬해 출력하세요.
mpg %>% group_by(manufacturer) %>% 
  filter(class == "compact") %>% 
  summarise(compact_num = n()) %>% #compact_num : compact 차종 수
  arrange(compact_num)수

#mpg 데이터의 f1 변수는 자동차에 사용하는 연료(fuel)를 의미합니다. 오른쪽은 자동차 연료별 가격을 나타낸 표입니다. 우선 이 정보를 이용해 연로와
#가격으로 구성된 데이터 프레임을 만들어 보세요.
fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F) #stringsAsFactors = F : 문자를 factor 타입으로 변환하지 않도록 설정하는 파라미터.

fuel

#1.mpg 데이터는 연료 종류를 나타낸 fl 변수는 있지만 연료 가격을 나타낸 변수는 없습니다. 위에서 만든 fuel 데이터를 이용해 mpg 데이터에 price.fl
#(연료 가격) 변수를 추가하세요.
cp_mpg <- left_join(cp_mpg, fuel, by = "fl")
cp_mpg

#2.연료 가격 변수가 잘 추가됐는지 확인하기 위해 model, fl, price_fl 변수를 추출해 앞부분 5행을 출력해 보세요.
cp_mpg %>% select(model, fl, price_fl) %>% 
  head(5)
