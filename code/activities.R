library(tidyverse)
activities <- read_csv("GOOGLE DATA ANALYTICS/activities.csv")
activities$Id <- as.character(activities$Id)
activities <- activities %>%
                select(-c('LoggedActivitiesDistance')) %>%
                  mutate(delta = TotalDistance - TrackerDistance,
                         tot_minutes = VeryActiveMinutes,
                                       FairlyActiveMinutes,
                                       LightlyActiveMinutes,
                                       SedentaryMinutes)

activities <- activities %>%
                select(-c('delta', 'TrackerDistance'))

cor(activities$Calories, activities$TotalSteps)
cor(activities$TotalDistance, activities$TotalSteps)
cor(activities$TotalDistance, activities$Calories)

cor(activities$VeryActiveMinutes, activities$VeryActiveDistance)
cor(activities$FairlyActiveMinutes, activities$ModeratelyActiveDistance)
cor(activities$LightlyActiveMinutes, activities$LightActiveDistance)

activities %>%
  group_by(Id) %>%
  summarize(VeryActiveDistance = mean(VeryActiveDistance),
            ModeratelyActiveDistance = mean(ModeratelyActiveDistance),
            LightActiveDistance = mean(LightActiveDistance),
            TotalDistance = mean(TotalDistance)) %>%
  pivot_longer(cols = c('VeryActiveDistance', 'ModeratelyActiveDistance', 'LightActiveDistance')) %>% 
  ggplot(aes(Id, TotalDistance)) + 
  geom_col(aes(fill = name))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank())+
  labs(title = 'Distribution of activities among distances tracked',
       y = 'Total distance')+
  scale_fill_discrete(name = "Activities")


activities %>%
  group_by(Id) %>%
  summarize(VeryActiveMinutes = mean(VeryActiveMinutes),
            FairlyActiveMinutes = mean(FairlyActiveMinutes),
            LightlyActiveMinutes = mean(LightlyActiveMinutes),
            SedentaryMinutes = mean(SedentaryMinutes),
            tot_minutes = mean(tot_minutes)) %>%
  pivot_longer(cols = c('VeryActiveMinutes', 'FairlyActiveMinutes', 'LightlyActiveMinutes', 'SedentaryMinutes')) %>% 
  ggplot(aes(Id, tot_minutes)) + 
  geom_col(aes(fill = name))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank())+
  labs(title = 'Distribution of activities among minutes tracked',
       y = 'Total minutes')+
  scale_fill_discrete(name = "Activities")
ggsave('distributionactivitiesamongminutes.png', dpi = 300)

activities %>%
  group_by(Id) %>%
  summarize(VeryActiveDistance = mean(VeryActiveDistance),
            ModeratelyActiveDistance = mean(ModeratelyActiveDistance),
            LightActiveDistance = mean(LightActiveDistance),
            TotalDistance = mean(TotalDistance)) %>%
  summarize(VeryActiveDistance = mean(VeryActiveDistance),
            ModeratelyActiveDistance = mean(ModeratelyActiveDistance),
            LightActiveDistance = mean(LightActiveDistance))



activities %>%
  group_by(Id) %>%
  summarize(VeryActiveMinutes = mean(VeryActiveMinutes),
            FairlyActiveMinutes = mean(FairlyActiveMinutes),
            LightlyActiveMinutes = mean(LightlyActiveMinutes),
            SedentaryMinutes = mean(SedentaryMinutes)) %>%
  summarize(VeryActiveMinutes = mean(VeryActiveMinutes),
            FairlyActiveMinutes = mean(FairlyActiveMinutes),
            LightlyActiveMinutes = mean(LightlyActiveMinutes),
            SedentaryMinutes = mean(SedentaryMinutes))

  