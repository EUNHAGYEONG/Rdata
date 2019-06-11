library(dplyr)
library(ggplot2)

#2018년도 인천광역시 모기밀도결과 csv 파일 로드
mosquito <- read.csv("2018년_모기밀도결과_인천.csv")

#연중주수 변수명 week로 변경
mosquito <- rename(mosquito, week = "연중주수")

#week 데이터에서 '주'라는 글자 모두 삭제
mosquito$week <- gsub("주$", "", mosquito$week)

#week 데이터의 자료형을 숫자형으로 변경
mosquito$week <- as.numeric(mosquito$week)

#채집장소 데이터를 모두 코드명으로 변경
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

#mosquito 데이터에 total이라는 모기종류 총합 변수 생성
mosquito <- mosquito %>% 
  mutate (total = 얼룩날개모기류 + 이나토미집모기 + 반점날개집모기 + 동양집모기 + 빨간집모기 + 작은빨간집모기 + 줄다리집모기 + 노랑늪모기 + 반점날개늪모기 + 흰줄숲모기 + 금빛숲모기 + 한국숲모기 + 토고숲모기 + 등줄숲모기 + 큰검정들모기 + 금빛어깨숲모기)

#계양1동 습도 데이터 로디
gyeyang1_Humidity <- read.csv("계양1동 습도.csv")
str(gyeyang1_Humidity)

gyeyang1_Humidity <- rename(gyeyang1_Humidity, day = format..day)
gyeyang1_Humidity <- rename(gyeyang1_Humidity, Humidity = value.location.56_127.Start...20180401)

gyeyang1_Humidity <- gyeyang1_Humidity %>% 
  filter(!is.na(hour))

gyeyang1_Humidity <- gyeyang1_Humidity %>% 
  filter(!is.na(Humidity))
gyeyang1_Humidity$Humidity <- ifelse(gyeyang1_Humidity$Humidity == -1 | gyeyang1_Humidity$Humidity == 0, NA, gyeyang1_Humidity$Humidity)

Incheon_Humidity <- read.csv("인천 습도.csv")
str(Incheon_Humidity)

Incheon_Humidity <- rename(Incheon_Humidity, Humidity = 평균.상대습도...)

k <- 14
for (i in 1:217) {
  Incheon_Humidity$week[i] <- k
  ifelse(i %% 7 == 0, k <- k + 1, k)
}

Incheon_Humidity <- Incheon_Humidity %>% 
  group_by(week) %>% 
  summarise(mean_Humidity = mean(Humidity))

View(Incheon_Humidity)

k <- 14
for (i in 1:5856) {
  gyeyang1_Humidity$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

gyeyang1_Humidity_final <- gyeyang1_Humidity %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_Humidity = mean(Humidity)) %>% 
  mutate(채집장소 = 1)

gyeyang1_Humidity_final$mean_Humidity[15:31] <- c(83.21429, 78.64286, 75.50000, 64.62857, 77.10000, 58.27143, 73.12857, 82.84286, 67.81429, 62.08571, 74.95714, 53.98571, 67.65714, 52.87143, 60.90000, 62.85714, 61.81429)

bupyeong1_Humidity <- read.csv("부평1동 습도.csv")
str(bupyeong1_Humidity)

bupyeong1_Humidity <- rename(bupyeong1_Humidity, day = format..day)
bupyeong1_Humidity <- rename(bupyeong1_Humidity, Humidity = value.location.55_125.Start...20180401)

bupyeong1_Humidity <- bupyeong1_Humidity %>% 
  filter(!is.na(hour) & !is.na(Humidity))

bupyeong1_Humidity$Humidity <- ifelse(bupyeong1_Humidity$Humidity == -1, NA, bupyeong1_Humidity$Humidity)

k <- 14
for (i in 1:5856) {
  bupyeong1_Humidity$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

bupyeong1_Humidity_final <- bupyeong1_Humidity %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_Humidity1 = mean(Humidity, na.rm = T)) %>% 
  mutate(채집장소 = 2)

bupyeong4_Humidity <- read.csv("부평4동 습도.csv")
str(bupyeong4_Humidity)

bupyeong4_Humidity <- rename(bupyeong4_Humidity, day = format..day)
bupyeong4_Humidity <- rename(bupyeong4_Humidity, Humidity = value.location.56_125.Start...20180401)

bupyeong4_Humidity <- bupyeong4_Humidity %>% 
  filter(!is.na(hour) & !is.na(Humidity))

table(bupyeong4_Humidity$Humidity)
bupyeong4_Humidity$Humidity <- ifelse(bupyeong4_Humidity$Humidity == -1, NA, bupyeong4_Humidity$Humidity)

k <- 14
for (i in 1:5856) {
  bupyeong4_Humidity$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

bupyeong4_Humidity_final <- bupyeong4_Humidity %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_Humidity2 = mean(Humidity, na.rm = T)) %>% 
  mutate(채집장소 = 2)

yeonhee_Humidity <- read.csv("연희동 습도.csv")
str(yeonhee_Humidity)

yeonhee_Humidity <- rename(yeonhee_Humidity, day = format..day)
yeonhee_Humidity <- rename(yeonhee_Humidity, Humidity = value.location.55_126.Start...20180401)

yeonhee_Humidity <- yeonhee_Humidity %>% 
  filter(!is.na(hour) & !is.na(Humidity))

table(yeonhee_Humidity$Humidity)
yeonhee_Humidity$Humidity <- ifelse(yeonhee_Humidity$Humidity == -1, NA, yeonhee_Humidity$Humidity)

k <- 14
for (i in 1:5856) {
  yeonhee_Humidity$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

yeonhee_Humidity_final <- yeonhee_Humidity %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_Humidity = mean(Humidity, na.rm = T)) %>% 
  mutate(채집장소 = 3)

geomam_Humidity <- read.csv("검암경서동 습도.csv")
str(geomam_Humidity)

geomam_Humidity <- rename(geomam_Humidity, day = format..day)
geomam_Humidity <- rename(geomam_Humidity, Humidity = value.location.55_127.Start...20180401)

geomam_Humidity <- geomam_Humidity %>% 
  filter(!is.na(hour) & !is.na(Humidity))

geomam_Humidity$Humidity <- ifelse(geomam_Humidity$Humidity == -1 | geomam_Humidity$Humidity == 0, NA, gyeyang1_Humidity$Humidity)

k <- 14
for (i in 1:5856) {
  geomam_Humidity$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

geomam_Humidity_final <- geomam_Humidity %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_Humidity = mean(Humidity)) %>% 
  mutate(채집장소 = 4)

geomam_Humidity_final$mean_Humidity[15:31] <- c(83.21429, 78.64286, 75.50000, 64.62857, 77.10000, 58.27143, 73.12857, 82.84286, 67.81429, 62.08571, 74.95714, 53.98571, 67.65714, 52.87143, 60.90000, 62.85714, 61.81429)

youngjong_Humidity <- read.csv("영종동 습도.csv")
str(youngjong_Humidity)

youngjong_Humidity <- rename(youngjong_Humidity, day = format..day)
youngjong_Humidity <- rename(youngjong_Humidity, Humidity = value.location.52_125.Start...20180401)

youngjong_Humidity <- youngjong_Humidity %>% 
  filter(!is.na(hour) & !is.na(Humidity))

table(youngjong_Humidity$Humidity)

k <- 14
for (i in 1:5856) {
  youngjong_Humidity$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

youngjong_Humidity_final <- youngjong_Humidity %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_Humidity = mean(Humidity)) %>% 
  mutate(채집장소 = 5)

songhae_Humidity <- read.csv("송해면 습도.csv")
str(songhae_Humidity)

songhae_Humidity <- rename(songhae_Humidity, day = format..day)
songhae_Humidity <- rename(songhae_Humidity, Humidity = value.location.51_131.Start...20180401)

songhae_Humidity <- songhae_Humidity %>% 
  filter(!is.na(hour) & !is.na(Humidity))

table(songhae_Humidity$Humidity)
songhae_Humidity$Humidity <- ifelse(songhae_Humidity$Humidity == -1, NA, songhae_Humidity$Humidity)

k <- 14
for (i in 1:5856) {
  songhae_Humidity$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

songhae_Humidity_final <- songhae_Humidity %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_Humidity = mean(Humidity, na.rm = T)) %>% 
  mutate(채집장소 = 6)

songhae_Humidity_final$mean_Humidity <- floor(songhae_Humidity_final$mean_Humidity)

seonwon_Humidity <- read.csv("선원면 습도.csv")
str(seonwon_Humidity)

seonwon_Humidity <- rename(seonwon_Humidity, day = format..day)
seonwon_Humidity <- rename(seonwon_Humidity, Humidity = value.location.51_130.Start...20180401)

seonwon_Humidity <- seonwon_Humidity %>% 
  filter(!is.na(hour) & !is.na(Humidity))

table(seonwon_Humidity$Humidity)

k <- 14
for (i in 1:5856) {
  seonwon_Humidity$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

seonwon_Humidity_final <- seonwon_Humidity %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_Humidity = mean(Humidity, na.rm = T)) %>% 
  mutate(채집장소 = 7)

seonwon_Humidity_final$mean_Humidity <- floor(seonwon_Humidity_final$mean_Humidity)
View(seonwon_Humidity_final)

samsan_Humidity<- read.csv("삼산면 습도.csv")

samsan_Humidity <- rename(samsan_Humidity, day = format..day)
samsan_Humidity <- rename(samsan_Humidity, Humidity = value.location.49_130.Start...20180401)

samsan_Humidity <- samsan_Humidity %>% 
  filter(!is.na(hour) & !is.na(Humidity))

table(samsan_Humidity$Humidity)
samsan_Humidity$Humidity <- ifelse(samsan_Humidity$Humidity == -1, NA, samsan_Humidity$Humidity)

k <- 14
for (i in 1:5856) {
  samsan_Humidity$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

samsan_Humidity_final <- samsan_Humidity %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_Humidity = mean(Humidity, na.rm = T)) %>% 
  mutate(채집장소 = 8)

gyodong_Humidity <- read.csv("교동면 습도.csv")
str(gyodong_Humidity)

gyodong_Humidity <- rename(gyodong_Humidity, day = format..day)
gyodong_Humidity <- rename(gyodong_Humidity, Humidity = value.location.48_131.Start...20180401)

gyodong_Humidity <- gyodong_Humidity %>% 
  filter(!is.na(hour) & !is.na(Humidity))

table(gyodong_Humidity$Humidity)
gyodong_Humidity$Humidity <- ifelse(gyodong_Humidity$Humidity == -1, NA, gyodong_Humidity$Humidity)

k <- 14
for (i in 1:5856) {
  gyodong_Humidity$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

gyodong_Humidity_final <- gyodong_Humidity %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_Humidity = mean(Humidity, na.rm = T)) %>% 
  mutate(채집장소 = 9)

ganghwa_Humidity <- read.csv("강화읍 습도.csv")
str(ganghwa_Humidity)

ganghwa_Humidity <- rename(ganghwa_Humidity, day = format..day)
ganghwa_Humidity <- rename(ganghwa_Humidity, Humidity = value.location.51_131.Start...20180401)

ganghwa_Humidity <- ganghwa_Humidity %>% 
  filter(!is.na(hour) & !is.na(Humidity))

table(ganghwa_Humidity$Humidity)
ganghwa_Humidity$Humidity <- ifelse(ganghwa_Humidity$Humidity == -1, NA, ganghwa_Humidity$Humidity)

k <- 14
for (i in 1:5856) {
  ganghwa_Humidity$week[i] <- k
  ifelse(i %% 168 == 0, k <- k + 1, k)
}

ganghwa_Humidity_final <- ganghwa_Humidity %>% 
  filter(!(week > 44)) %>% 
  group_by(week) %>% 
  summarise(mean_Humidity = mean(Humidity, na.rm = T)) %>% 
  mutate(채집장소 = 10)

View(ganghwa_Humidity_final)

bupyeong_Humidity <- merge(bupyeong1_Humidity_final, bupyeong4_Humidity_final)

bupyeong_Humidity_final <- bupyeong_Humidity %>% 
  select(week, 채집장소, mean_Humidity1, mean_Humidity2) %>% 
  mutate(mean_Humidity = (mean_Humidity1 + mean_Humidity2) / 2) %>% 
  select(-mean_Humidity1, -mean_Humidity2)

#각 행정구역 final 기온 데이터를 TEMP라는 데이터프레임에 한데 모음
Humidity <- bind_rows(gyeyang1_Humidity_final, bupyeong_Humidity_final, yeonhee_Humidity_final, geomam_Humidity_final, youngjong_Humidity_final, songhae_Humidity_final, seonwon_Humidity_final, samsan_Humidity_final, gyodong_Humidity_final, ganghwa_Humidity_final)

#week와 채집장소를 기준으로 TEMP 데이터프레임과 mosquito_2018 데이터프레임을 합침 
Final_Humidity <- merge(mosquito, Humidity, by = c("week", "채집장소"))

#습도별모기개체수 관계
Humidity_Mosquito <- Final_Humidity %>%
  group_by(week) %>% 
  mutate(mosquito = sum(total)) %>% 
  mutate(Humidity = mean(mean_Humidity)) %>% 
  select(월, week, Humidity, mosquito)

Humidity_Mosquito <- unique(Humidity_Mosquito)

View(Humidity_Mosquito)
