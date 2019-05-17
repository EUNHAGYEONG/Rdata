#연령대 및 성별 월급 차이
sex_ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex, ageg) %>% 
  summarise(mean_income = mean(income))

ggplot(data = sex_ageg_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))
