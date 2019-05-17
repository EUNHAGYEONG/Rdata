#mpg 데이터와 midwest 데이터를 이용해 분석 문제를 해결해 보세요.

#Q1)mpg 데이터와 cty(도시 연비)와 hwy(고속도로 연비) 간에 어떤 관계가 있는지 알아보려고 합니다.
#Q1)x축은 cty, y축은 hwy로 된 산점도를 만들어 보세요.

ggplot(data = mpg, aes(x = cty, y = hwy)) + 
  geom_point()
