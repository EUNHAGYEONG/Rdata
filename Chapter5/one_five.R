cc <- read.csv("충청남도_예산군_농어촌체험마을_20180917.csv", stringsAsFactor = F) #csv 파일 불러오기
head(cc) # head - 데이터 앞부분 확인하기, 앞에서부터 여섯 번째 행까지 출력
head(cc, 3) #숫자를 입력하면 앞에서부터 입력한 행까지의 데이터를 출력
tail(cc) #tail - 뒤에서부터 여섯 행을 출력
tail(cc, 3) #뒤에서부터 입력한 행까지의 데이터를 출력
View(cc) #View - 뷰어 창에서 데이터 확인하기 하나의 파일로 엑셀 형식으로 데이터를 보여줌.
dim(cc) #dim - 데이터가 몇 행, 몇 열로 구성되어 있는지 알려줌.
str(cc) #str - 변수의 자료형 을 알려줌.

summary(cc) #summary - 변수의 값을 요약하여 알려줌. 변수의 특성을 파악하는데 도움이 됨.


#mpg 데이터를 가지고 연습하기
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg) #ggplot2 패키지 내장 데이터인 mpg를 불러와 데이터 프레임을 만듦.
head(mpg)
head(mpg, 3)
tail(mpg)
tail(mpg, 3)
View(mpg)
dim(mpg)
str(mpg)
summary(mpg)
