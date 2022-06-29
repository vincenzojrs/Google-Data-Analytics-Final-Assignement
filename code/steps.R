library(tidyverse)
library(forcats)
library(tidyr)
library(skimr)
require(scales)

steps <- read.csv('~/GOOGLE DATA ANALYTICS/steps.csv')
steps$Id <- as.character(steps$Id)
steps$ActivityDay <- as.Date(steps$ActivityDay, format = '%m/%d/%Y')
steps$weekdays <- weekdays(steps$ActivityDay)

ggplot(data = steps, aes(x = Id,
                         y = ActivityDay,
                         fill = StepTotal)) +
  ylab('Day')+
  geom_tile() +
  scale_fill_distiller(name = 'Daily Steps', palette = "Spectral") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank())+
  labs(title = 'Distribution of steps over time per user')
  
skim_without_charts(steps)

steps %>%
  group_by(Id) %>%
  summarise(avg_d_steps = mean(StepTotal),
            max_d_steps = max(StepTotal)) %>%
  ggplot(mapping = aes(x = fct_reorder(as.character(Id), avg_d_steps), y = avg_d_steps)) +
  geom_col(fill = 'red')+
  ylab('Daily Steps')+
  geom_hline(yintercept = 10000)+
  geom_hline(yintercept = 7638, linetype = 'dashed')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank())+
  labs(title = 'Average daily steps per person',
       y = 'Steps')

#########

x <- c('Martedì', 'Mercoledì', 'Giovedì', 'Venerdì', 'Sabato', 'Domenica', 'Lunedì')

week1 <- filter(steps, (ActivityDay<='2016-04-18'))
week1 <- week1 %>% group_by(weekdays) %>% summarize(sum = sum(StepTotal))

week2 <- filter(steps, (ActivityDay>='2016-04-19' & ActivityDay<='2016-04-25'))
week2 <- week2 %>% group_by(weekdays) %>% summarize(sum = sum(StepTotal))

week3 <- filter(steps, (ActivityDay>='2016-04-26' & ActivityDay<='2016-05-02'))
week3 <- week3 %>% group_by(weekdays) %>% summarize(sum = sum(StepTotal))

week4 <- filter(steps, (ActivityDay>='2016-05-03'))
week4 <- week4 %>% group_by(weekdays) %>% summarize(sum = sum(StepTotal))

month <- steps %>% group_by(weekdays) %>% summarize(avg_step_per_weekday = mean(StepTotal)) 


ggplot() + 
  geom_line(week1, mapping = aes(weekdays, sum, group = 1), color = 'red', size = 1.2) + scale_x_discrete(limits = x)+
  geom_line(week2, mapping = aes(weekdays, sum, group = 1), color = 'blue', size = 1.2)+
  geom_line(week3, mapping = aes(weekdays, sum, group = 1), color = 'green', size = 1.2)+
  geom_line(week4, mapping = aes(weekdays, sum, group = 1), color = 'magenta', size = 1.2)+
  geom_col(month, mapping = aes(weekdays, avg_step_per_weekday*25), fill = 'black', alpha = 0.5)+
  scale_y_continuous(labels = comma)+
  ylab('Sum of steps / Average steps')+
  labs(title = 'Sum of steps per weekdays per 4 weeks / Average steps per weekday')+
  theme_classic()



