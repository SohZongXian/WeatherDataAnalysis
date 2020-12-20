install.packages("readxl")
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
library(readxl)
installed.packages()
install.packages("datasets")
library(datasets)
install.packages("tidyverse")
library(tidyverse)
install.packages("plotrix")
library(plotrix)
#filter,arrange select, mutate, summarise, group by, 

weather=read.csv(file = 'C:\\Users\\lenovo\\Desktop\\weather.csv', header=TRUE, sep=",")
ggplot(data = weather) + geom_bar(mapping = aes(x = MinTemp, fill=MinTemp))

ggplot(data = weather) + geom_count(mapping=aes(x = RainToday, y = RainTomorrow, color = ..n..))
b=ggplot(weather, aes(x = Pressure9am, y = Temp9am))
b+geom_point(colour="blue")+geom_smooth()

ggplot(data = weather) + geom_boxplot(mapping=aes(x = RainToday, y = Temp9am, fill=RainToday))

ggplot(weather, aes(x = Pressure3pm, y = Temp3pm))+geom_line()

ggplot(data = weather) + geom_boxplot(mapping=aes(x = RainTomorrow, y = Evaporation, fill=RainTomorrow))

#piechart
WindDirection <- c()
WindDirection[1] = nrow(x[x$WindGustDir == "NW",])
WindDirection[2] = nrow(x[x$WindGustDir == "ENE",])
WindDirection[3] = nrow(x[x$WindGustDir == "SSE",])
WindDirection[4] = nrow(x[x$WindGustDir == "SE",])
WindDirection[5] = nrow(x[x$WindGustDir == "S",])
WindDirection[6] = nrow(x[x$WindGustDir == "E",])
WindDirection[7] = nrow(x[x$WindGustDir == "N",])
windtitle=c("NW","ENE","SSE","SE","S", "E","N")
pct <- round(WindDirection/sum(WindDirection)*100)
windtitle <- paste(windtitle, pct)
windtitle <- paste(windtitle,"%",sep="")
pie(radius=1, WindDirection,labels=windtitle, col=rainbow(length(windtitle)), main="Pie chart of Wind Direction")

#filter 1
z=filter(weather, RainToday=="Yes" & RainTomorrow=="Yes")  
ggplot(data = z) + geom_bar(mapping = aes(x = WindGustDir, fill=WindGustDir)) #weathernation, northwest system bringing heavy rain and snow
ggplot(data = z) + geom_bar(mapping = aes(x = Rainfall, fill=Rainfall))# drizzle lasts longer, therefore sustain for 2 days
ggplot(data = z) + geom_bar(mapping = aes(x = WindDir3pm, fill=WindDir3pm))#weather nation further proven
summary(z)
#filter 2
riskmmplus=filter(weather, RISK_MM>0)
ggplot(data = riskmmplus) + geom_count(mapping=aes(x = RISK_MM, y = RainTomorrow, color = ..n..))#decently accurate Risk mm prediction as there are some predicted risk mm but no rain the next day.


#filter 3 
draught=filter(weather, RainToday=="No" & RainTomorrow=="No")
ggplot(data = draught) + geom_bar(mapping = aes(x = Rainfall, fill=Rainfall))#slight inaccuracy in data, recorded rainfall amount but no rain for 2 days
f=filter(draught, RISK_MM>0.4)
f2=arrange(f, Temp3pm)
#filter 4
rt=filter(weather, RainToday=="No")
MaxtempEvap=ggplot(rt, aes(x = Sunshine, y = WindGustSpeed))
ScatterplotMAxtempVevap=MaxtempEvap+geom_point() + labs(x="Sunshine", y="WindGustSpeed", title="Scatterplot of windgust speed and sunshine if it rains today")
ScatterplotMAxtempVevap#if no rain today, data is scattered around the low wind gust speed region and high sunshine region

#filter 5
filter5=filter(weather, MaxTemp>mean(MaxTemp))
ggplot(filter5, aes(x=Evaporation,y=MaxTemp))+geom_count()+geom_smooth()








#mutate1
x=mutate(weather, AverageTemp=(MinTemp+MaxTemp)/2)
y=select(x, RainToday, AverageTemp)
ggplot(data = y) + geom_boxplot(mapping=aes(x = RainToday, y = AverageTemp))#average temp is higher if rain today

#mutate2
addapressure=mutate(weather, AveragePressure=(Pressure9am+Pressure3pm)/2)
addapressure=tbl_df(addapressure)
aa=addapressure %>%
mutate(HighAvgPressure = ifelse(AveragePressure>mean(AveragePressure), "High", "Low")) %>%
ggplot(aes(x=WindGustSpeed, y=Evaporation, colour=HighAvgPressure)) + geom_point() + labs(title="Correlation between Evaporation, WindGustSpeed and Average pressure")
+geom_smooth(se=FALSE)#windgustspeed and eva high, not high avg pressure

#mutate3 x is average temp added
x=tbl_df(x)
x %>%
  mutate(HighAverageTemp = ifelse(AverageTemp>mean(AverageTemp), "High", "Low")) %>%
  ggplot(aes(x=Sunshine, y=Evaporation, colour=HighAverageTemp)) + geom_point() + labs(title="Correlation between Windspeed, Humidity  and Average Temperature at 9am")+geom_smooth(se=FALSE)#low avg temp region = high humidity and low windspeed, high avg temp region = low humidity and low windspeed, scattered greater then low temp region when windspeed>35 

#mutate 4
x=tbl_df(x)
x %>%
  mutate(Evaporationamt = ifelse(Evaporation>mean(Evaporation), "High", "Low")) %>%
  ggplot(aes(x=Evaporationamt, y=AverageTemp, fill=Evaporationamt)) + geom_boxplot(outlier.colour = "red",outlier.shape = 2,outlier.size = 3) + labs(title="Correlation between average temperature and evaporation level")#Mean of average temp high, evaporation amt above average

#mutate 5 
weather=tbl_df(weather)
weather %>%mutate(AverageHumidity = (Humidity9am+Humidity3pm)/2) %>%
  ggplot(weather, mapping=aes(x=AverageHumidity))+ geom_bar(stat="count", width=0.7, fill="steelblue")+theme_minimal()+labs(title="Average humidity spread")#amount of rows with the average humidity for the day with around 60 has the most spread
  


#summarise group by 1

x %>%
  group_by (WindDir9am, na.rm=TRUE) %>%
  summarize(meanAverageTemp=mean(AverageTemp)) %>%
  ggplot(aes(x=WindDir9am, y=meanAverageTemp, fill= WindDir9am)) + geom_bar(stat="identity") + theme_classic()+labs(title ="Mean of average temperature of the dataset group by WindDirection at 9 am")

#summarise group by 2 
avg_pressure=summarize(x,  mean(Pressure9am), mean(Pressure3pm))
xx=x %>%
  group_by (RainToday) %>%
  summarize(meanAverageEvaporation=mean(Evaporation)) %>%
  ggplot(aes(x=RainToday, y=meanAverageEvaporation, fill= RainToday)) + geom_bar(stat="identity") + theme_classic()+labs(title ="Mean of average temperature of the dataset group by whether it will rain on the day")
#summarise group by 3 
meanriskmm=summarize(x, mean(RISK_MM), max(RISK_MM))
x %>%
  mutate(HighOrLowTemp9am = ifelse(Temp9am>mean(Temp9am), "High", "Low")) %>%
  group_by (HighOrLowTemp9am) %>%
  summarize(meanRisk_MM=mean(RISK_MM)) %>%
  ggplot(aes(x=HighOrLowTemp9am, y=meanRisk_MM, fill= HighOrLowTemp9am)) + geom_bar(stat="identity") + theme_classic()+labs(title ="Count of numbers of high average temp from the dataset in comparision with the mean of Risk_MM at 9am")
#summarise 4 
meanrainfall=summarize(x, mean(Rainfall), max(Rainfall), median(Rainfall))
x %>%
  mutate(HighOrLowTemp9am = ifelse(Temp9am>mean(Temp9am), "High", "Low")) %>%
  group_by (HighOrLowTemp9am) %>%
  summarize(meanRisk_MM=mean(RISK_MM)) %>%
  ggplot(aes(x=HighOrLowTemp9am, y=meanRisk_MM, fill= HighOrLowTemp9am)) + geom_bar(stat="identity") + theme_classic()+labs(title ="Count of numbers of high average temp from the dataset in comparision with the mean of Risk_MM at 9am")
#summarise 5 
meanevaporation=summarize(x, mean(Evaporation), max(Evaporation), min(Evaporation), median(Evaporation))# highly occured data for evaporation almost simillar to the average, shows that evaporation in this dataset is fairly even

#arrange and show first few rows
arrange1=arrange(x, Sunshine) %>%
  group_by(Sunshine)
arrange1head=head(arrange1, 20)
ggplot(arrange1head,aes(y=AverageTemp,x=WindGustSpeed))+geom_point()+geom_smooth()

#arrange 2 and show first few rows
arrange2=arrange(x,AverageTemp) %>%
  group_by(AverageTemp)
arrange2head=head(arrange2,20)
ggplot(arrange2head,aes(y=Pressure9am,x=MaxTemp))+geom_point(aes(colour=factor(RainTomorrow)))#lowest avg temp in data, more rain tomorrow then no, pressure increases with max temp

#arrange 3 and show first few rows
arrange3=arrange(x,AverageTemp) %>%
  group_by(AverageTemp)
arrange3head=head(arrange3,20)
ggplot(arrange3head,aes(y=Sunshine,x=MinTemp))+geom_point(aes(colour=factor(RainToday)))

#arrange 4
arrange4=arrange(x,WindGustSpeed)
arrange4head=head(arrange4, 20)
ggplot(arrange4head,aes(y=WindGustSpeed,x=Rainfall))+geom_count()

#arrange 5 
arrange5=arrange(x,desc(RISK_MM))
arrange5head=head(arrange5, 20)
ggplot(arrange5head,aes(x=RainToday,y=RISK_MM))+geom_count()

#select
select1=select(x, Sunshine, Evaporation) 
ggplot(x, aes(x=Sunshine, fill=Evaporation))+geom_histogram()
ggplot(x, aes(x=Evaporation, fill=Sunshine))+geom_histogram()
#select 2
select2=select(x, Evaporation, WindGustDir) 
ggplot(x, aes(x=WindGustSpeed))+geom_histogram()
pre=filter(x, WindGustSpeed>85)
#select 3
select3=select(x, Evaporation, WindGustDir) 
ggplot(x, aes(x=WindGustDir, fill=WindGustDir))+geom_histogram(stat="count")
filter3.1=filter(x, WindGustDir=="WSW")
ggplot(filter3.1, aes(x=WindGustDir, fill=WindGustDir))+geom_histogram(stat="count")
#select 4
select4=select(x, AverageTemp, RISK_MM)
ggplot(x, aes(x=AverageTemp))+geom_histogram()
#select 5
select5=select(x, Temp3pm, Evaporation)
ggplot(select5, aes(x=Evaporation, y=Temp3pm))+geom_line()




