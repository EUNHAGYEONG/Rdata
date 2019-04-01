#변수명 바꾸기
#변수명을 바꾸기 위해서는 dplyr이란 패키지를 다운받아 rename() 함수를 사용

df_raw = data.frame(var1 = c(1, 2, 1),
                    var2 = c(2, 3, 2))

install.packages("dplyr")
library(dplyr)

df_new <- df_raw #df_raw의 복사본 df_new 생성
df_new <- rename(df_new, v2 = var2) #var2를 v2로 수정
df_new


df_raw


df_new

df_new <- rename(df_new, v2 = var2)


df_new



# mpg 데이터를 이용하여 아래 문제를 해결해보시오.
# 1) ggplot2() 패키지의 mpg 데이터를 사용할 수 있도록 불러온 후 복사본을 만드세요.
mpg_copy <- as.data.frame(ggplot2::mpg)
mpg_copy
# 2) 복사본 데이터를 이용해 cty는 city로, hwy는 highway로 수정하세요.
mpg_copy <- rename(mpg_copy, city = cty, highway = hwy)
# 3) 데이터 일부를 출력해 변수명이 바뀌었는지 혹인해 보세요. 아래와 같은 결과물이 출려되어야 합니다.
mpg_copy
