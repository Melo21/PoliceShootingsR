library(ggplot2)
library(dplyr)
library(lubridate)
getwd()
df <- read.csv(file="/Users/melodywong/Desktop/policekillingsUS.csv", header=TRUE, sep=",")
head(df)

mendf <- subset(df, gender=="M", select= c(age,armed,race,signs_of_mental_illness,threat_level,flee))
head(mendf)
femdf <- subset(df, gender=="F", select= c(age,armed,race,signs_of_mental_illness,threat_level,flee))
head(femdf)

boxplot(mendf$age,femdf$age, names=c("MEN","WOMEN"), col="gold")
##Question: Where are the most police shootings by state?
##Isolated the dataframe to group killings by state and sorted the data to descending
cc<-df %>% group_by(state) %>% arrange(state) %>% 
  summarise(freq = n())  %>% 
  arrange(desc(freq))  %>%
  head(20)

ggplot(cc, aes(x=reorder(state, -freq), y=freq)) + geom_bar(stat="identity") + 
  labs(x="Count of People Killed", y="States", title="Most States With Police Shootings")

##Looking deeper into California's shootings, is it consistent across the years?

df$date <- parse_date_time(df$date, orders = c("mdy", "dmy"))
df$Date <- as.Date(df$date, format = "%m/%d/%y")

#split month, day , year 
df$day <- factor(day(df$Date))
df$month <- factor(month(df$Date, label = TRUE))
df$year <- factor(year(df$Date))
df$dayofweek <- factor(wday(df$Date, label = TRUE))

yearc<- rbind(y15, y16, y17)

y15 <- subset(df, state=="CA",year=2015, select= c(threat_level,year))
y16 <- subset(df, state=="CA",year=2016, select= c(threat_level,year))
y17 <- subset(df, state=="CA",year=2017, select= c(threat_level,year))
head(y15)

yearc<-yearc %>% group_by(year) %>% arrange(year) %>% 
  summarise(freq = n())  %>% 
  arrange(desc(freq))  %>%
  head(20)


ggplot(yearc, aes(x=year, y=freq)) + geom_bar(stat="identity") + 
  labs(x="Count of People Killed", y="States", title="Most States With Police Shootings")

plot(y15)
