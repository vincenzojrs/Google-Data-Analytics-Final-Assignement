install.packages("ggExtra")

library(forcats)
library(tidyverse)
library(ggExtra)
library(ggplot2)
library(skimr)
library(dplyr)

sleep <- read.csv('~/GOOGLE DATA ANALYTICS/sleep.csv')
sleep$SleepDay <- as.Date(sleep$SleepDay, format = '%m/%d/%Y')
sleep$delta_minutes <- sleep$TotalTimeInBed - sleep$TotalMinutesAsleep
sleep$Id <- as.character(sleep$Id)

skim_without_charts(sleep)

sleep$weekday <- weekdays(as.Date(sleep$SleepDay))

ggplot() +
  ylab('Day')+
  geom_tile(data = sleep, aes(x = Id,
                              y = SleepDay,
                              fill = round(TotalMinutesAsleep/60))) +
  scale_fill_distiller(name = 'Hours Slept', palette = 'Reds') +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank())+
  labs(title = 'Distribution of sleep hours over time per user')


cor(sleep$TotalMinutesAsleep, sleep$TotalTimeInBed)

sleep %>% 
  group_by(Id) %>%
  summarize(avg_minutes_slept = mean(TotalMinutesAsleep)) %>% skim_without_charts()

sleep %>% 
  group_by(Id) %>%
  summarize(avg_minutes_slept = mean(TotalMinutesAsleep)) %>%
  ggplot(mapping = aes(x = fct_reorder(as.character(Id), avg_minutes_slept), 
                       y = avg_minutes_slept)) + 
    geom_col(fill = 'red')+
  geom_hline(yintercept = 480)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  labs(title = 'Average minutes slept per person',
       y = 'Minutes')

# comparing distribution of total minutes 
ggplot(data = sleep) +
  geom_boxplot(mapping = aes(x = weekday, y = TotalMinutesAsleep), alpha = 0.3)+
  geom_boxplot(mapping = aes(x = weekday, y = TotalTimeInBed), alpha = 0.3, color = 'red')+
  labs(title = 'Comparing minutes asleep vs. total minutes in bed',
       subtitle = 'red = Total time in bed',
       y = "Minutes")+
  theme_classic()

# total time in bed vs. delta minutes
ggMarginal(ggplot(data = sleep, aes(TotalTimeInBed, delta_minutes)) +
  geom_jitter(alpha = 0.7)+
  geom_smooth(method = lm, color = 'red', se = FALSE)+
  labs(title = 'Total time in bed vs. delta_minutes')+
  theme_classic())


# delta minutes across weekdays
ggMarginal(ggplot(sleep, aes(x=weekday, y=delta_minutes)) +
             geom_jitter(width = 0.1 , alpha = 0.3)+
             geom_point()+
  theme_classic()+
             labs(title = 'Distribution of delta_minutes among weekdays'))
  

round(9.1)
