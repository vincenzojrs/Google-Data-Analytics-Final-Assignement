library(tidyverse)
library(forcats)
library(skimr)
library(tidyr)
require(scales)
df <- read.csv('~/GOOGLE DATA ANALYTICS/calories.csv')
df$Id <- as.character(df$Id)
df$ActivityDay <- as.Date(df$ActivityDay, format = '%m/%d/%Y')
df$weekdays <- weekdays(df$ActivityDay)
skim_without_charts(df)

df %>%
  group_by(Id) %>%
  summarize(avg_daily_calories = mean(Calories)) %>%
  ggplot() + 
  geom_col(mapping = aes(fct_reorder(Id, avg_daily_calories), y = avg_daily_calories), fill = 'red')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank())+
  geom_hline(yintercept = 2340, linetype = 'dashed', size = 1.2,)+
  geom_hline(yintercept = 2304)+
  labs(title = 'Average daily calories burnt per user',
       subtitle = 'The dashed line represent the TDEE, the other one the average expenditure')

ggplot(data = df, aes(x = Id,
                         y = ActivityDay,
                         fill = Calories)) +
  ylab('Day')+
  geom_tile() +
  scale_fill_distiller(name = 'Calories per day', palette = "Spectral") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank())+
  labs(title = 'Distribution of calories burnt over time per user')
  
x <- c('Martedì', 'Mercoledì', 'Giovedì', 'Venerdì', 'Sabato', 'Domenica', 'Lunedì')

week1 <- filter(df, (ActivityDay<='2016-04-18'))
week1 <- week1 %>% group_by(weekdays) %>% summarize(sum = sum(Calories))

week2 <- filter(df, (ActivityDay>='2016-04-19' & ActivityDay<='2016-04-25'))
week2 <- week2 %>% group_by(weekdays) %>% summarize(sum = sum(Calories))

week3 <- filter(df, (ActivityDay>='2016-04-26' & ActivityDay<='2016-05-02'))
week3 <- week3 %>% group_by(weekdays) %>% summarize(sum = sum(Calories))

week4 <- filter(df, (ActivityDay>='2016-05-03'))
week4 <- week4 %>% group_by(weekdays) %>% summarize(sum = sum(Calories))

month <- df %>% group_by(weekdays) %>% summarize(avg_calories_per_weekday = mean(Calories)) 


ggplot() + 
  geom_line(week1, mapping = aes(weekdays, sum, group = 1), color = 'red', size = 1.2) + scale_x_discrete(limits = x)+
  geom_line(week2, mapping = aes(weekdays, sum, group = 1), color = 'blue', size = 1.2)+
  geom_line(week3, mapping = aes(weekdays, sum, group = 1), color = 'green', size = 1.2)+
  geom_line(week4, mapping = aes(weekdays, sum, group = 1), color = 'magenta', size = 1.2)+
  geom_col(month, mapping = aes(weekdays, avg_calories_per_weekday*25), fill = 'black', alpha = 0.5)+
  scale_y_continuous(labels = comma)+
  ylab('Sum of calories / Average calories')+
  labs(title = 'Sum of calories burnt per weekdays per 4 weeks / Average calories per weekday')+
  theme_classic()
