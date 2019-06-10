install.packages("plotly")
library(plotly)
library(ggplot2)
Final <- merge(TEMP_Mosquito, Humidity_Mosquito, by = c("월", "week", "mosquito"))

Final <- Final %>% 
  arrange(week)

View(Final)

t <- ggplot(data = Final, aes(x = TEMP, y = mosquito)) + 
  geom_point(size = 2, color = "red") +
  ggtitle("모기개체수와 온도의 관계") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
cor.test(Final$TEMP, Final$mosquito)

h <- ggplot(data = Final, aes(x = Humidity, y = mosquito)) + 
  geom_point(size = 2, color = "skyblue") +
  ggtitle("모기개체수와 습도의 관계") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
cor.test(Final$Humidity, Final$mosquito)

ggplotly(t)
ggplotly(h)

cor <- data.frame(변수 = c("온도", "습도"),
                  상관계수 = c(0.655372, 0.5, 0.3826354))

X <- ggplot(data = cor, aes(x = reorder(data, -상관계수), y = 상관계수, fill = 변수)) +
  geom_col() +
  scale_fill_manual(values = c("skyblue", "red")) +
  ggtitle("모기개체수와 습도, 모기개체수와 온도의 상관계수 비교") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  geom_text(mapping = aes(label = 상관계수), size = 4, vjust = -0.5)

ggplotly(X)
