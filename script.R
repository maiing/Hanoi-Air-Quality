## DataTalk No.2
## Author: Mai Nguyen 
## Date: Jun 24, 2018
## Topic: Hanoi Air Quality


#--- Call lib
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)



#--- Import data 
air_quality_org <- read.csv("https://raw.githubusercontent.com/chuvanan/data_projects/master/datatalk-meetup/meetup-02/data/hanoi-air-quality.csv", header=TRUE)

air_quality <- air_quality_org

write.csv(air_quality_org, "/Users/mainguyen/Analytics/Hanoi Air Quailty/air_quality.csv")



#--- Explore & clean data

# check timeframe
timeframe <- air_quality %>%
  group_by(year, month) %>%
  summarise(n=n())

# remove june 2018 
air_quality <- air_quality[!(air_quality$year == 2018 & air_quality$month == 6), ]

# keep valid cases only
air_quality <- filter(air_quality, qc_name == "Valid" & !is.na(aqi))
# air_quality$aqi <- as.double(air_quality$aqi) Nếu không chạy lệnh này thì có khác gì không

# remove 2015: because it has only December. Poluttion ranges from month to month
air_quality <- filter(air_quality, year != 2015)



#--- Data visualization
# Scatter - fail 
# scatter_plot <- ggplot(air_quality, aes(x=month, y=aqi, colour=year)) +
#   geom_point() 
# 
# scatter_plot


# Plot 1: Which month is the best time to run? 
by_month <- air_quality %>%
  mutate(month_fac=as.factor(month),
         year_fac=as.factor(year)) %>%
  group_by(month_fac, year_fac) %>%
  summarise(avg=mean(aqi))


best_month <- ggplot(by_month, aes(x=month_fac, y=avg, colour=year_fac, group=year_fac)) +
  geom_line() +
  geom_point()


best_month


# Plot 2: Best time of the day for running (by month)

by_hour <- air_quality %>%
  mutate(hour_fac=as.factor(hour),
         month_fac=as.factor(month)) %>%
  group_by(hour_fac, month_fac) %>%
  summarise(avg=mean(aqi))


best_hour <- ggplot(by_hour, aes(x=hour_fac, y=avg, colour=month_fac, group=month_fac)) +
  geom_line() +
  geom_point()

best_hour


# Plot 3: Best time of the day for running (by year)

by_hour_year <- air_quality %>%
  mutate(hour_fac=as.factor(hour),
         year_fac=as.factor(year)) %>%
  filter(month<=6) %>%
  group_by(hour_fac, year_fac) %>%
  summarise(avg=mean(aqi))


best_hour_year <- ggplot(by_hour_year, aes(x=hour_fac, y=avg, colour=year_fac, group=year_fac)) +
  geom_line() +
  geom_point()

best_hour_year

# Plot 4: By hour & By season 
air_quality$season <- ifelse(air_quality$month==12 | air_quality$month==1 | air_quality$month==2, "Winter", 
                             ifelse(air_quality$month==3 | air_quality$month==4 | air_quality$month==5, "Spring", 
                                    ifelse(air_quality$month==6 | air_quality$month==7 | air_quality$month==8, "Summer", 
                                           ifelse(air_quality$month==9 | air_quality$month==10 | air_quality$month==11, "Autumn", "Other")
                                    )
                             )
)

by_season_year <- air_quality %>%
  mutate(hour_fac=as.factor(hour),
         season_fac=as.factor(season), 
         year_fac=as.factor(year)) %>%
  group_by(hour_fac, season_fac, year_fac) %>%
  summarise(avg=mean(aqi))

by_season_year$year_fac <- factor(by_season_year$year_fac, 
                         labels=c("2016", "2017", "5M 2018"))



season_impact <- ggplot(by_season_year, aes(x=hour_fac, y=avg, colour=year_fac, group=year_fac)) + 
  geom_line() +
  geom_point(size=1.5, shape=20) +
  geom_hline(yintercept=100, color = "red", size=1.5) +
  facet_wrap(~season_fac) +
  ggtitle("Best time for running") +
  #theme(text = element_text(size = 16, family="Helvetica", face="italic")) + 
  theme_grey() +
  labs(y = "AQI Index", x = "Hour of the day", colour="Year") +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 6)) +
  theme(axis.text.y = element_text(size = 6)) +
  scale_colour_brewer(palette="Dark2")
  
pdf("season_impact.pdf") 
season_impact
dev.off()

# Plot 5: Is the air getting worse? 
status_year <- air_quality %>%
  mutate(year_fac=as.factor(year)) %>%
  group_by(aqi_categ, year) %>% 
  summarise(n=n())

year_count <- air_quality %>%
  group_by(year) %>%
  summarise(count=n())  

status_year <- left_join(status_year, year_count, by="year")
status_year$pct <- (status_year$n/status_year$count)*100
status_year$aqi_categ <- factor(status_year$aqi_categ, levels=c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"))

stacked <- ggplot(status_year, aes(x=year, y=pct, fill=aqi_categ)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE)) +
  ggtitle("Is the air getting worse?") +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal() +
  labs(y = "%", x = element_blank(), fill = "Pollution status") +
  theme(axis.title.y = element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  #guides(fill=guide_legend(title = NULL)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"))

pdf("pollution_status.pdf")
stacked
dev.off()
