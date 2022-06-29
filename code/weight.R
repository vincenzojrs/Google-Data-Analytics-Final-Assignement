library(tidyverse)
library(skimr)
weight <- read.csv('~/GOOGLE DATA ANALYTICS/weight.csv')
weight$Id <- as.character(weight$Id)
weight$Date <- as.Date(weight$Date, format = '%m/%d/%Y')
skim_without_charts(weight)

weight <- weight %>%
  select(-WeightPounds, -Fat, -BMI, -LogId)

weight$IsManualReport <- as.logical(weight$IsManualReport)
weight$IsManualReport <- as.integer(weight$IsManualReport)

summary <- weight %>%
  group_by(Id) %>%
    summarize(
      count = n(),
      meankg = mean(WeightKg),
      manualreport = mean(IsManualReport),
      differenceweight = (max(WeightKg) - min(WeightKg)),
      delta_date = (max(Date)-min(Date))) %>%
  mutate(weight_over_days = differenceweight/as.integer(delta_date))

summary

cor(summary$count, summary$meankg)
cor(summary$manualreport, summary$meankg)
cor(summary$count, summary$manualreport)

table(summary$manualreport)

ggplot() + 
  geom_line(data = filter(weight, Id == 8877689391), mapping = aes(x = Date, y = WeightKg))+
  geom_smooth(data = filter(weight, Id == 8877689391), mapping = aes(x = Date, y = WeightKg), se = FALSE, color = 'red')+
  labs(title = 'Weight trend of the best performing individual',
       subtitle = 'Id = 8877689391, losing 1.8kg in 30 days')+
theme_classic()
  