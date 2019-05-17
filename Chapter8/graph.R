library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)

ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  xlim(3, 6) +
  ylim(10, 30)

library(dplyr)

mpg_df <- mpg %>% 
  group_by(drv) %>% 
  summarise(hwy_mean = mean(hwy))

ggplot(data = mpg_df, aes(x = drv, y = hwy_mean)) + geom_col()
ggplot(data = mpg_df, aes(x = reorder(drv, -hwy_mean), y = hwy_mean)) + geom_col()


ggplot(data = mpg, aes(x = drv)) + geom_bar()

ggplot(data = mpg, aes(x = hwy)) + geom_bar()
