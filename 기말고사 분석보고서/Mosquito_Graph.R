install.packages("plotly")
library(plotly)
library(ggplot2)

Final <- merge(TEMP_Mosquito, Humidity_Mosquito, by = c("월", "week", "mosquito"))
Final <- Final %>% 
  arrange(week)

head(Final)

t <- ggplot(data = Final, aes(x = TEMP, y = mosquito)) + 
  geom_point(size = 2, color = "red") +
  ggtitle("모기개체수와 온도의 관계") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
ggplotly(t)

h <- ggplot(data = Final, aes(x = Humidity, y = mosquito)) + 
  geom_point(size = 2, color = "skyblue") +
  ggtitle("모기개체수와 습도의 관계") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
ggplotly(h)

cor.test(Final$TEMP, Final$mosquito)
cor.test(Final$Humidity, Final$mosquito)

cor <- data.frame(sort = c("온도", "습도"),
                  cor = c(0.655372, 0.3826354))

X <- ggplot(data = cor, aes(x = sort, y = cor, fill = sort)) +
  geom_col() +
  scale_fill_manual(values = c("skyblue", "red")) +
  ggtitle("모기개체수와 습도, 모기개체수와 온도의 상관계수 비교") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  geom_text(mapping = aes(label = cor), size = 4, vjust = -0.5)

ggplotly(X)
