# Tidy Tuesday Week 12
# Emily Xing

library(readr)
library(lubridate)
library(ggmap)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)

#Rename dataframes
week12_google_trends <- read_csv("data-science/Tidy Tuesday 12/week12_google_trends.csv")
google <- week12_google_trends
View(google)

week12_mediacloud_hurricanes <- read_csv("data-science/Tidy Tuesday 12/week12_mediacloud_hurricanes.csv")
hurricanes <- week12_mediacloud_hurricanes
View(hurricanes)

week12_mediacloud_states <- read_csv("data-science/Tidy Tuesday 12/week12_mediacloud_states.csv")
states <- week12_mediacloud_states
View(states)

week12_mediacloud_top_online_news <- read_csv("data-science/Tidy Tuesday 12/week12_mediacloud_top_online_news.csv")
online <- week12_mediacloud_top_online_news
View(online)

week12_tv_hurricanes <- read_csv("data-science/Tidy Tuesday 12/week12_tv_hurricanes.csv")
tv.hurricanes <- week12_tv_hurricanes
View(tv.hurricanes)

#Tidy the dates
tv.hurricanes$Date <- mdy(tv.hurricanes$Date)
states$Date <- mdy(states$Date)
hurricanes$Date <- mdy(hurricanes$Date)

#Clean state names
colnames(states) <- c("Date", "texas", "puerto_rico", "florida")
colnames(google) <- c("Date", "Harvey", "Irma", "Maria", "Jose")

#Tidy the data
google <- gather(google, key = Hurricanes, value = frequency, Harvey, Irma, Maria, Jose)

hurricanes <- gather(hurricanes, key = Hurricanes, value = frequency, Harvey, Irma, Maria, Jose)

states <- gather(states, key = States, value = frequency, texas, puerto_rico, florida)

tv.hurricanes <- gather(tv.hurricanes, key = Hurricanes, value = frequency, Harvey, Irma, Maria, Jose)

#tv.hurricanes - plotting coverage over time
tv.coverage <- ggplot(data = tv.hurricanes, aes(x=Date)) + 
  geom_path(aes(y=frequency, colour = Hurricanes))

#hurricanes
hurricanes.plot <- ggplot(data = hurricanes, aes(x=Date)) + 
  geom_line(aes(y=frequency, colour = Hurricanes))

#Fatalities
Hurricanes <- c("Harvey", "Irma", "Maria", "Jose")
Fatalities <- c(82, 134, 4465, 1)
Deaths <- c("82 deaths", "134 deaths", "4465 deaths", "1 death")
Date <- c("2017-09-03", "2017-09-13", "2017-10-02", "2017-09-25") %>%
  as.Date()
deaths <- data.frame(Date, Hurricanes, Fatalities, Deaths)

#Compare fatalities and media coverage over time
hurricanes.plot + 
  geom_col(data=deaths, aes(y=Fatalities, fill = Hurricanes), alpha = 0.5) +
  geom_text(data=deaths, aes(label = Deaths, y = Fatalities + 20), size = 4) +
  xlab("") + ylab("frequency(sentences per day and deaths)") +
  ggtitle("Fatalities vs. Media coverage per hurricane (2017)") +
  theme_linedraw() +
  theme(
    plot.title = element_text(colour = "black", face = "bold"))

hurricanes.plot + 
  geom_col(data=deaths, aes(y=Fatalities, fill = Hurricanes), alpha = 0.5) +
  geom_label_repel(data=deaths, aes(y= Fatalities, label = Deaths),colour = "black", size = 3.5) +
  xlab("") + ylab("frequency(sentences per day and deaths)") +
  ggtitle("Fatalities vs. Media coverage per hurricane (2017)") +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(colour = "black", face = "bold"))






