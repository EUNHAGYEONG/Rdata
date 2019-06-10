library(dplyr)
library(ggplot2)

mosquito <- read.csv("2018년_모기밀도결과_인천.csv") #모기 밀집 데이터 로드

mosquito <- rename(mosquito, week = "연중주수") #연중주수 -> week로 변수명 변경 
mosquito$week <- gsub("주$", "", mosquito$week) #week에 '주'를 다 빼고 숫자만 남김 
mosquito$week <- as.numeric(mosquito$week) #week 자료형 숫자형으로 형변환

#채집장소의 데이터를 코드명으로 변경
mosquito$"채집장소" <- ifelse(mosquito$"채집장소" == "인천광역시 계양구 선주지동", 1, 
                               ifelse(mosquito$"채집장소" == "인천광역시 부평구 부평동", 2,
                                      ifelse(mosquito$"채집장소" == "인천광역시 서구 연희동", 3,
                                             ifelse(mosquito$"채집장소" == "인천광역시 서구 백석동", 4,
                                                    ifelse(mosquito$"채집장소" == "인천광역시 중구 운남동 (영종도)", 5,
                                                           ifelse(mosquito$"채집장소" == "인천광역시 강화 송해면 (숭뢰리)", 6,
                                                                  ifelse(mosquito$"채집장소" == "인천광역시 강화 송해면 (솔정리)", 6,
                                                                         ifelse(mosquito$"채집장소" == "인천광역시 강화 선원면 (금월리)", 7,
                                                                                ifelse(mosquito$"채집장소" == "인천광역시 강화 삼산면 (석모리)", 8,
                                                                                       ifelse(mosquito$"채집장소" == "인천광역시 강화 교동면 (대룡리)", 9,
                                                                                              ifelse(mosquito$"채집장소" == "인천광역시 강화 강화읍 (대산리)", 10,
                                                                                                     ifelse(mosquito$"채집장소" == "인천광역시 강화 강화읍 (월곳리)", 10, NA))))))))))))

#모기개체수의 총합을 나타내는 total 변수 생성
mosquito <- mosquito %>% 
  mutate (total = 얼룩날개모기류 + 이나토미집모기 + 반점날개집모기 + 동양집모기 + 빨간집모기 + 작은빨간집모기 + 줄다리집모기 + 노랑늪모기 + 반점날개늪모기 + 흰줄숲모기 + 금빛숲모기 + 한국숲모기 + 토고숲모기 + 등줄숲모기 + 큰검정들모기 + 금빛어깨숲모기)

gyeyang1_TEMP <- read.csv("계양1동 기온.csv") # 계양1동 기온 데이터 로드 

gyeyang1_TEMP <- rename(gyeyang1_TEMP, day = format..day) #계양1동 format..day -> day 변수명 변경 
gyeyang1_TEMP <- rename(gyeyang1_TEMP, TEMP = value.location.56_127.Start...20180401) #계양1동 value.location.56_127.Start...20180401 -> TEMP 변수명 변경 

#계양1동의 hour나 TEMP에 NA가 있으면 모두 제거
gyeyang1_TEMP <- gyeyang1_TEMP %>% 
  filter(!is.na(hour) | !is.na(TEMP))

#week 변수 생성 : 하루에 24개의 온도 데이터 생성 일주일이면 24*7 = 168개 온도 데이터 생성
k <- 14
for (i in 1:5856) {
  gyeyang1_TEMP$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

#_final 데이터 : week는 44주까지만, week별 평균기온 요약, 채집장소 변수 생성
gyeyang1_TEMP_final <- gyeyang1_TEMP %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_TEMP = mean(TEMP)) %>% 
  mutate(채집장소 = 1)

bupyeong1_TEMP <- read.csv("부평1동 기온.csv") #부평1동 기온 데이터 로드

bupyeong1_TEMP <- rename(bupyeong1_TEMP, day = format..day) 
bupyeong1_TEMP <- rename(bupyeong1_TEMP, TEMP = value.location.55_125.Start...20180401)
bupyeong1_TEMP <- bupyeong1_TEMP %>% 
  filter(!is.na(hour) | !is.na(TEMP))

k <- 14
for (i in 1:5856) {
  bupyeong1_TEMP$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

bupyeong1_TEMP_final <- bupyeong1_TEMP %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_TEMP1 = mean(TEMP)) %>% 
  mutate(채집장소 = 2)

bupyeong4_TEMP <- read.csv("부평4동 기온.csv") #부평4동 기온 데이터 로딩 

bupyeong4_TEMP <- rename(bupyeong4_TEMP, day = format..day)
bupyeong4_TEMP <- rename(bupyeong4_TEMP, TEMP = value.location.56_125.Start...20180401)

bupyeong4_TEMP <- bupyeong4_TEMP %>% 
  filter(!is.na(hour) | !is.na(TEMP))


k <- 14
for (i in 1:5856) {
  bupyeong4_TEMP$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

bupyeong4_TEMP_final <- bupyeong4_TEMP %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_TEMP2 = mean(TEMP)) %>% 
  mutate(채집장소 = 2)

yeonhee_TEMP <- read.csv("연희동 기온.csv") #연희동 기온데이터 로딩 

yeonhee_TEMP <- rename(yeonhee_TEMP, day = format..day)
yeonhee_TEMP <- rename(yeonhee_TEMP, TEMP = value.location.55_126.Start...20180401)

yeonhee_TEMP <- yeonhee_TEMP %>% 
  filter(!is.na(hour) | !is.na(TEMP))

k <- 14
for (i in 1:5856) {
  yeonhee_TEMP$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

yeonhee_TEMP_final <- yeonhee_TEMP %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_TEMP = mean(TEMP)) %>% 
  mutate(채집장소 = 3)

geomam_TEMP <- read.csv("검암경서동 기온.csv") #검암경서동 기온 데이터 로딩 

geomam_TEMP <- rename(geomam_TEMP, day = format..day)
geomam_TEMP <- rename(geomam_TEMP, TEMP = value.location.55_127.Start...20180401)

geomam_TEMP <- geomam_TEMP %>% 
  filter(!is.na(hour) | !is.na(TEMP))

k <- 14
for (i in 1:5856) {
  geomam_TEMP$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

geomam_TEMP_final <- geomam_TEMP %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_TEMP = mean(TEMP)) %>% 
  mutate(채집장소 = 4)

youngjong_TEMP <- read.csv("영종동 기온.csv") #영종동 기온데이터 로딩 

youngjong_TEMP <- rename(youngjong_TEMP, day = format..day)
youngjong_TEMP <- rename(youngjong_TEMP, TEMP = value.location.52_125.Start...20180401)

youngjong_TEMP <- youngjong_TEMP %>% 
  filter(!is.na(hour) | !is.na(TEMP))

k <- 14
for (i in 1:5856) {
  youngjong_TEMP$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

youngjong_TEMP_final <- youngjong_TEMP %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_TEMP = mean(TEMP)) %>% 
  mutate(채집장소 = 5)

songhae_TEMP <- read.csv("송해면 기온.csv") #송해면 기온데이터 로딩 

songhae_TEMP <- rename(songhae_TEMP, day = format..day)
songhae_TEMP <- rename(songhae_TEMP, TEMP = value.location.51_131.Start...20180401)

songhae_TEMP <- songhae_TEMP %>% 
  filter(!is.na(hour) | !is.na(TEMP))

k <- 14
for (i in 1:5856) {
  songhae_TEMP$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

songhae_TEMP_final <- songhae_TEMP %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_TEMP = mean(TEMP)) %>% 
  mutate(채집장소 = 6)

seonwon_TEMP <- read.csv("선원면 기온.csv") #선원면 기온데이터 로딩 

seonwon_TEMP <- rename(seonwon_TEMP, day = format..day)
seonwon_TEMP <- rename(seonwon_TEMP, TEMP = value.location.51_130.Start...20180401)

seonwon_TEMP <- seonwon_TEMP %>% 
  filter(!is.na(hour) | !is.na(TEMP))

k <- 14
for (i in 1:5856) {
  seonwon_TEMP$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

seonwon_TEMP_final <- seonwon_TEMP %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_TEMP = mean(TEMP)) %>% 
  mutate(채집장소 = 7)

samsan_TEMP <- read.csv("삼산면 기온.csv") #삼산면 기온데이터 로딩 

samsan_TEMP <- rename(samsan_TEMP, day = format..day)
samsan_TEMP <- rename(samsan_TEMP, TEMP = value.location.49_130.Start...20180401)

samsan_TEMP <- samsan_TEMP %>% 
  filter(!is.na(hour) | !is.na(TEMP))

k <- 14
for (i in 1:5856) {
  samsan_TEMP$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

samsan_TEMP_final <- samsan_TEMP %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_TEMP = mean(TEMP)) %>% 
  mutate(채집장소 = 8)

gyodong_TEMP <- read.csv("교동면 기온.csv") #교동면 기온데이터 로딩 

gyodong_TEMP <- rename(gyodong_TEMP, day = format..day)
gyodong_TEMP <- rename(gyodong_TEMP, TEMP = value.location.48_131.Start...20180401)

gyodong_TEMP <- gyodong_TEMP %>% 
  filter(!is.na(hour) | !is.na(TEMP))

k <- 14
for (i in 1:5856) {
  gyodong_TEMP$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

gyodong_TEMP_final <- gyodong_TEMP %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_TEMP = mean(TEMP)) %>% 
  mutate(채집장소 = 9)

ganghwa_TEMP <- read.csv("강화읍 기온.csv") #강화읍 기온데이터 로딩 

ganghwa_TEMP <- rename(ganghwa_TEMP, day = format..day)
ganghwa_TEMP <- rename(ganghwa_TEMP, TEMP = value.location.51_131.Start...20180401)

ganghwa_TEMP <- ganghwa_TEMP %>% 
  filter(!is.na(hour) | !is.na(TEMP))

k <- 14
for (i in 1:5856) {
  ganghwa_TEMP$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

ganghwa_TEMP_final <- ganghwa_TEMP %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_TEMP = mean(TEMP)) %>% 
  mutate(채집장소 = 10)

#부평1동 데이터와 부평4동의 데이터를 부평동 데이터로 병합시키는 과정
bupyeong_TEMP <- merge(bupyeong1_TEMP_final, bupyeong4_TEMP_final)

#다른 final 데이터와 마찬가지로 mean_TEMP1 변수를 만듦(부평1동과 부평4동의 평균기온), 그 후 부평1동, 부평4동 데이터 삭제 
bupyeong_TEMP_final <- bupyeong_TEMP %>% 
  select(week, 채집장소, mean_TEMP1, mean_TEMP2) %>% 
  mutate(mean_TEMP = (mean_TEMP1 + mean_TEMP2) / 2) %>% 
  select(-mean_TEMP1, -mean_TEMP2)

#각 행정구역 final 기온 데이터를 TEMP라는 데이터프레임에 모음
TEMP <- bind_rows(gyeyang1_TEMP_final, bupyeong_TEMP_final, yeonhee_TEMP_final, geomam_TEMP_final, youngjong_TEMP_final, songhae_TEMP_final, seonwon_TEMP_final, samsan_TEMP_final, gyodong_TEMP_final, ganghwa_TEMP_final)

#week와 채집장소를 기준으로 TEMP 데이터프레임과 mosquito 데이터프레임을 합침 
Final_TEMP <- merge(mosquito, TEMP, by = c("week", "채집장소"))

#온도별모기개체수 관계
TEMP_Mosquito <- Final_TEMP %>%
  group_by(week) %>% 
  mutate(mosquito = sum(total)) %>% 
  mutate(TEMP = mean(mean_TEMP)) %>% 
  select(월, week, TEMP, mosquito)

View(TEMP_Mosquito)
TEMP_Mosquito <- unique(TEMP_Mosquito)

View(TEMP_mosquito)
