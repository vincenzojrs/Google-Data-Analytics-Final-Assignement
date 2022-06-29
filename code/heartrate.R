library(tidyverse)
library(lubridate)
library(skimr)
require(scales)

heartrate <- read_csv("GOOGLE DATA ANALYTICS/heartrate.csv", 
                      col_types = cols(Id = col_character(), 
                                       Time = col_datetime(format = "%m/%d/%Y %H:%M:%S %p")))
heartrate$hours <- str_sub(heartrate$Time, -8)
heartrate$hours_brief <- str_sub(heartrate$hours, 1,2) #delete, when creating the last plot
heartrate$hours <- hms(heartrate$hours) #delete, when creating the last plot
heartrate$weekdays <- weekdays(heartrate$Time) #delete, when creating the last plot
heartrate$day <- str_sub(heartrate$Time, 1,10) #delete, when creating the last plot

breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59")) #delete, when creating the last plot
labels <- c("Night", "Morning", "Afternoon", "Evening") #delete, when creating the last plot

heartrate$Time_of_day <- cut(x=hour(heartrate$hours), breaks = breaks, labels = labels, include.lowest=TRUE) #delete, when creating the last plot

skim_without_charts(heartrate)

heartrate %>%
  group_by(Time_of_day) %>%
  summarize(avg_bpm = mean(Value),
            sd_bpm = sd(Value),
            max_bpm = max(Value),
            n = n())

heartrate %>%
  group_by(weekdays) %>%
  summarize(avg_bpm = mean(Value),
            sd_bpm = sd(Value),
            max_bpm = max(Value))

ggplot() + 
  geom_point(data = 
               (heartrate %>%
                  filter(between(hour(hours), 7, 22)) %>% 
                  group_by(Id, day) %>%
                  summarize(n = n())),
             mapping = aes(x = Id, y = day, size = n), alpha = 1, color = 'red') +
  scale_size_continuous(range = c(3,7))+
  geom_point(data =
               (heartrate %>%
                  filter(between(hour(hours), 22, 23) | between(hour(hours), 0, 6)) %>%
                  group_by(Id, day) %>%
                  summarize(n = n())),
             mapping = aes(x = Id, y = day, size = n), alpha = 0.5)+
  scale_size_continuous(range = c(3,7))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank(),
        legend.position="none") +
  labs(title = 'Count for daytime measurements (in red) and nighttime ones (in black)')
ggsave('nighttimedaytime.png', dpi = 300)

ggplot(heartrate, aes(x=Id, y=Value)) + 
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=23, size=2)+
  ylab('BPM')+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = 'Distribution of heart rates over users')
ggsave('Distribution of heart rates over users.png', dpi = 250)

x <- c('Martedì', 'Mercoledì', 'Giovedì', 'Venerdì', 'Sabato', 'Domenica', 'Lunedì')

heartrate %>%
  group_by(weekdays, hours_brief) %>%
  summarize(avg_bpm = mean(Value))%>%
  ggplot() +
  geom_tile(mapping = aes(x = weekdays,
                          y = hours_brief,
                          fill = avg_bpm)) +
  scale_x_discrete(limits = x) +
  scale_fill_distiller(name = 'Average BPM',
                       palette = "Spectral") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = 'Average heartrate per hour per weekday')
ggsave('avg_bpm_hour_week.png', dpi = 250)

heartrate %>%
  filter(Value > 120) %>%
  group_by(hours_brief) %>%
  summarize(n = n()) %>%
  ggplot() + 
    geom_col(mapping = aes(x = hours_brief, y = n), fill = 'red')+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  ylab('Count')+
  labs(title = 'Count for "active" bpm throughout the day',
       subtitle = 'filtered for BPM > 120')
ggsave('count_120_bpm.png', dpi = 250)

### MIND THE COMMENTS ABOVE
heartrate <- filter(heartrate, Id == '8877689391')

ggplot()+
  geom_line(data = filter(heartrate, Time < as.POSIXct("2016-04-13 00:00:00")), 
            mapping = aes(x= hours, y = Value, group = 1), color = '#025259', alpha = 0.4,)+
  geom_line(data = filter(heartrate, Time >= as.POSIXct("2016-04-13 00:00:00") & Time < as.POSIXct("2016-04-14 00:00:00")), 
            mapping = aes(x= hours, y = Value, group = 1), color = '#007172', alpha = 0.4, size = 1.2)+
  geom_line(data = filter(heartrate, Time >= as.POSIXct("2016-04-14 00:00:00") & Time < as.POSIXct("2016-04-15 00:00:00")), 
            mapping = aes(x= hours, y = Value, group = 1), color = '#F29325', alpha = 0.4)+
  geom_line(data = filter(heartrate, Time >= as.POSIXct("2016-04-15 00:00:00") & Time < as.POSIXct("2016-04-16 00:00:00")), 
            mapping = aes(x= hours, y = Value, group = 1), color = '#D94F04', alpha = 0.4)+
  geom_line(data = filter(heartrate, Time >= as.POSIXct("2016-04-16 00:00:00") & Time < as.POSIXct("2016-04-17 00:00:00")), 
            mapping = aes(x= hours, y = Value, group = 1), color = '#000000', alpha = 0.4)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  labs(title = 'Comparing heartrates of the first five days available for Mara')

ggsave('heartrates.png',
       width =  4096,
       height = 3112,
       units = 'px',
       dpi=300)

write_csv(heartrate, 'heartrate_mod.csv')