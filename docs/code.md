
ACTIVITY MONITOR DATA (Updated 20211026)

``` r
knitr::opts_chunk$set(echo = TRUE, 
                      eval = FALSE)
```

## \#DATA SETUP

Load packages and import data

``` r
library(readxl)    
library(tidyverse)
library(dplyr)
library(clock)
library(lubridate)
library(hms)
setwd('~/Desktop')


##IMPORT the data
sheets <- readxl::excel_sheets('puppies2.xlsx')
activity_df <- do.call(rbind, lapply(sheets, function(X) transform(readxl::read_excel('puppies2.xlsx', sheet = X), sheetname = X)))
activity_df<-activity_df %>% mutate(time=as_hms(activity_df$Time)) %>% na.omit() %>%  select(-sheetname,-Time)
activity_df$Date <- as.Date(activity_df$Date, format= "%Y-%m-%d")
```

Removing dates where collar is off

``` r
#removing dates where collar is off
  #Anya
filtered_dates <- activity_df[!(activity_df$Dog.name == "Anya" & activity_df$Date < "2019-10-02")&!(activity_df$Dog.name == "Anya" & activity_df$Date == "2019-10-16")&!(activity_df$Dog.name == "Anya" & activity_df$Date > "2020-11-18"),]
  #Aries
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Aries" & filtered_dates$Date < "2019-10-02")&!(filtered_dates$Dog.name == "Aries" & filtered_dates$Date == "2019-10-16")&!(filtered_dates$Dog.name == "Aries" & filtered_dates$Date > "2020-11-18"),]
  #Weston
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Weston" & filtered_dates$Date < "2019-10-02")&!(filtered_dates$Dog.name == "Weston" & filtered_dates$Date == "2019-10-16")&!(filtered_dates$Dog.name == "Weston" & filtered_dates$Date > "2020-11-18"),]
  #Ying
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Ying" & filtered_dates$Date < "2019-10-02")&!(filtered_dates$Dog.name == "Ying" & filtered_dates$Date == "2019-10-16")&!(filtered_dates$Dog.name == "Ying" & filtered_dates$Date > "2020-11-18"),]
  #Yolanda
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Yolanda" & filtered_dates$Date < "2019-10-02")&!(filtered_dates$Dog.name == "Yolanda" & filtered_dates$Date == "2019-10-16")&!(filtered_dates$Dog.name == "Yolanda" & filtered_dates$Date > "2019-11-18"),]
  #Zax
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Zax" & filtered_dates$Date < "2019-10-02")&!(filtered_dates$Dog.name == "Zax" & filtered_dates$Date == "2019-10-16")&!(filtered_dates$Dog.name == "Zax" & filtered_dates$Date > "2019-11-18"),]
  #Zina
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Zina" & filtered_dates$Date < "2019-10-02")&!(filtered_dates$Dog.name == "Zina" & filtered_dates$Date == "2019-10-16")&!(filtered_dates$Dog.name == "Zina" & filtered_dates$Date > "2019-11-18"),]
  #Arthur
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Arthur" & filtered_dates$Date < "2020-2-08")&!(filtered_dates$Dog.name == "Arthur" & filtered_dates$Date > "2020-03-10"),]
  #Aurora
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Aurora" & filtered_dates$Date < "2020-2-08")&!(filtered_dates$Dog.name == "Aurora" & filtered_dates$Date > "2020-03-10"),]
  #Westley
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Westley" & filtered_dates$Date < "2020-2-01")&!(filtered_dates$Dog.name == "Westley" & filtered_dates$Date > "2020-03-10"),]
  #Wisdom
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Wisdom" & filtered_dates$Date < "2020-2-01")&!(filtered_dates$Dog.name == "Wisdom" & filtered_dates$Date > "2020-03-10"),]
  #Yonder
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Yonder" & filtered_dates$Date < "2020-2-06")&!(filtered_dates$Dog.name == "Yonder" & filtered_dates$Date > "2020-03-10"),]
  #Zindel
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Zindel" & filtered_dates$Date < "2020-2-06")&!(filtered_dates$Dog.name == "Zindel" & filtered_dates$Date > "2020-03-10"),]
  #Zola
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Zola" & filtered_dates$Date < "2020-2-06")&!(filtered_dates$Dog.name == "Zola" & filtered_dates$Date > "2020-03-10"),]
  #Barley
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Barley" & filtered_dates$Date < "2020-8-06")&!(filtered_dates$Dog.name == "Barley" & filtered_dates$Date > "2020-09-30"),]
  #Kristoff
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Kristoff" & filtered_dates$Date < "2020-07-24")&!(filtered_dates$Dog.name == "Kristoff" & filtered_dates$Date > "2020-09-22"),]
  #Sparky
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Sparky" & filtered_dates$Date < "2020-10-08")&!(filtered_dates$Dog.name == "Sparky" & filtered_dates$Date > "2020-12-15"),]
  #Sassy
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Sassy" & filtered_dates$Date < "2020-10-08")&!(filtered_dates$Dog.name == "Sassy" & filtered_dates$Date > "2020-12-14"),]
  #Stanley
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Stanley" & filtered_dates$Date < "2020-10-08")&!(filtered_dates$Dog.name == "Stanley" & filtered_dates$Date > "2020-12-15"),]
  #Rainbow
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Rainbow" & filtered_dates$Date < "2020-10-08")&!(filtered_dates$Dog.name == "Rainbow" & filtered_dates$Date > "2020-12-07"),]
  #Jack
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Jack" & filtered_dates$Date < "2021-02-06")&!(filtered_dates$Dog.name == "Jack" & filtered_dates$Date > "2021-04-19"),]
  #Lily
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Lily" & filtered_dates$Date < "2021-2-06")&!(filtered_dates$Dog.name == "Lily" & filtered_dates$Date > "2021-04-20"),]
  #Leo
filtered_dates <- filtered_dates[!(filtered_dates$Dog.name == "Leo" & filtered_dates$Date < "2021-2-06")&!(filtered_dates$Dog.name == "Leo" & filtered_dates$Date > "2021-04-19"),]
```

Rename columns

``` r
#renamecolumns
filtered_dates<-rename(filtered_dates, "name"=Dog.name)
filtered_dates<-rename(filtered_dates, activity_counts="Activity.Data")
```

Delete sleeping hours (10pm-8am)

``` r
#delete sleeping hours (10pm-8am)
filtered_datetime<- filtered_dates %>% filter(hour(time)>7)
filtered_datetime<- filtered_datetime %>% filter(hour(time)<23)
```

Adding DOB, weeks old, rearing (home vs DPK), & sex columns

``` r
#addingDOB
newdf<-filtered_datetime %>% mutate(DOB =
                     case_when(name == "Anya" ~ "2019-06-29",
                               name == "Aries" ~ "2019-06-29",
                                name == "Weston" ~ "2019-06-26",
                                name == "Ying" ~ "2019-06-27",
                                name == "Yolanda" ~ "2019-06-27",
                                name == "Zina" ~ "2019-06-28",
                                name == "Zax" ~ "2019-06-28",
                                name == "Arthur" ~ "2019-12-12",
                                name == "Aurora" ~ "2019-12-12",
                                name == "Westley" ~ "2019-12-06",
                                name == "Wisdom" ~ "2019-12-06",
                                name == "Yonder" ~ "2019-12-11",
                                name == "Zola" ~ "2019-12-11",
                                name == "Zindel" ~ "2019-12-11",
                                name == "Kristoff" ~ "2020-05-05",
                                name == "Barley" ~ "2020-05-06",
                                name == "Rainbow" ~ "2020-08-02",
                                name == "Sassy" ~ "2020-08-12",
                                name == "Sparky" ~ "2020-08-12",
                                name == "Stanley" ~ "2020-08-12",
                                name == "Lily" ~ "2020-12-11",
                                name == "Leo" ~ "2020-12-11",
                                name == "Jack" ~ "2020-12-11"))
#make DOB in date format
newdf$DOB <- as.Date(newdf$DOB, format= "%Y-%m-%d")

# exact weeks / age column
newdf<-newdf %>% mutate(exactage =
                       difftime(Date, DOB, units="weeks"))
#make weeks old column (rounded)
newdf<-newdf%>% mutate(weeksold =
                         case_when(exactage >= 8 & exactage < 9 ~8,
                                   exactage >= 9 & exactage < 10 ~9,
                                   exactage >= 10 & exactage < 11 ~10,
                                   exactage >= 11 & exactage < 12 ~11,
                                   exactage >= 12 & exactage < 13 ~12,
                                   exactage >= 13 & exactage < 14 ~13,
                                   exactage >= 14 & exactage < 15 ~14,
                                   exactage >= 15 & exactage < 16 ~15,
                                   exactage >= 16 & exactage < 17 ~16,
                                   exactage >= 17 & exactage < 18 ~17,
                                   exactage >= 18 & exactage < 19 ~18,
                                   exactage >= 19 & exactage < 20 ~19,
                                   exactage >= 20 ~20,)) 

#adding rearing column (home based vs. DPK) 
#note: Spring 2020 puppies are "home-based" from 8-10 weeks
newdf<-newdf %>% mutate(rearing =
                          case_when(name == "Anya" ~ "DPK",
                                    name == "Aries" ~ "DPK",
                                    name == "Weston" ~ "DPK",
                                    name == "Ying" ~ "DPK",
                                    name == "Yolanda" ~ "DPK",
                                    name == "Zina" ~ "DPK",
                                    name == "Zax" ~ "DPK",
                                    name == "Arthur" & Date <"2020-02-17"  ~ "home",
                                    name == "Arthur" & Date >="2020-02-17" ~ "DPK",
                                    name == "Aurora" & Date <"2020-02-17"  ~ "home",
                                    name == "Aurora" & Date >="2020-02-17" ~ "DPK",
                                    name == "Westley" & Date <"2020-02-17"  ~ "home",
                                    name == "Westley" & Date >="2020-02-17" ~ "DPK",
                                    name == "Wisdom" & Date <"2020-02-17"  ~ "home",
                                    name == "Wisdom" & Date >="2020-02-17" ~ "DPK",
                                    name == "Yonder" & Date <"2020-02-17"  ~ "home",
                                    name == "Yonder" & Date >="2020-02-17" ~ "DPK",
                                    name == "Zindel" & Date <"2020-02-17"  ~ "home",
                                    name == "Zindel" & Date >="2020-02-17" ~ "DPK",
                                    name == "Zola" & Date <"2020-02-17"  ~ "home",
                                    name == "Zola" & Date >="2020-02-17" ~ "DPK",
                                    name == "Kristoff" ~ "home",
                                    name == "Barley" ~ "home",
                                    name == "Rainbow" ~ "home",
                                    name == "Sassy" ~ "home",
                                    name == "Sparky" ~ "home",
                                    name == "Stanley" ~ "home",
                                    name == "Lily" ~ "home",
                                    name == "Leo" ~ "home",
                                    name == "Jack" ~ "home"))
#adding sex column 
newdf<-newdf %>% mutate(sex =
                          case_when(name == "Anya" ~ "F",
                                    name == "Aries" ~ "M",
                                    name == "Weston" ~ "M",
                                    name == "Ying" ~ "M",
                                    name == "Yolanda" ~ "F",
                                    name == "Zina" ~ "F",
                                    name == "Zax" ~ "M",
                                    name == "Arthur" ~ "M",
                                    name == "Aurora" ~ "F",
                                    name == "Westley" ~ "M",
                                    name == "Wisdom" ~ "F",
                                    name == "Yonder" ~ "F",
                                    name == "Zola" ~ "F",
                                    name == "Zindel" ~ "M",
                                    name == "Kristoff" ~ "M",
                                    name == "Barley" ~ "M",
                                    name == "Rainbow" ~ "F",
                                    name == "Sassy" ~ "F",
                                    name == "Sparky" ~ "M",
                                    name == "Stanley" ~ "M",
                                    name == "Lily" ~ "F",
                                    name == "Leo" ~ "M",
                                    name == "Jack" ~ "M"))
```

Make hour column

``` r
newdf<-newdf %>% mutate(hour=
                         (hour(time)))
```

Make combined column for date and time

``` r
newdf <- newdf %>% mutate(datetime=as.POSIXct(paste(Date,time), format="%Y-%m-%d %H:%M:%S"))
```

Create 5 minute intervals labels

``` r
newdf <- newdf %>% mutate(interval=cut(newdf$datetime, breaks = "5 min"))
newdf <- newdf %>% mutate(intervaltime=as.POSIXct(newdf$interval))
newdf <- newdf %>% mutate(intervaltime=format(newdf$interval, format = "%H:%M:%S"))

#turn interval into datetime class
newdf$intervaltime2<-strptime(newdf$intervaltime, format='%Y-%m-%d %H:%M:%S')

#make column "intervaltime" just for interval time
newdf$intervaltime<-strftime(newdf$intervaltime2, '%H:%M')
```

For each puppy, find an average of activity for each 5 minute interval
(make one mean activity count for each interval)

``` r
intervaldf<-newdf %>% 
     group_by(intervaltime,weeksold,name,rearing,sex) %>% 
   summarise(interval_avg_activity=mean(activity_counts))

intervaldf2<-newdf %>% 
     group_by(intervaltime,Date,weeksold,name,rearing,sex) %>% 
   summarise(interval_avg_activity=mean(activity_counts))
```

Deviance from weekly means (for each puppy) for each (5 min interval)
data point (creates indiv\_weekly\_dev df)

``` r
#means for each puppy for each week of age
weeklymeanforage_bypuppy<-intervaldf %>% group_by(name, weeksold) %>% 
  summarize("weeklymeanforage_bypuppy"=mean(interval_avg_activity), N=n()) 
# merge two data frames: weekly mean for each puppy and hourly mean
#places each weekly mean next to its corresponding interval data point
indiv_weekly_dev<- merge(intervaldf,weeklymeanforage_bypuppy,by=c("name","weeksold"))
#deviance from the weekly mean (for each puppy) for each (5 min interval) data point 
indiv_weekly_dev<-indiv_weekly_dev %>% mutate(meandeviance=(interval_avg_activity-weeklymeanforage_bypuppy))
```

Find an average of activity for each week for each 5 minute interval
(make one mean activity count for each interval) (not by puppy, for the
entire group)

``` r
weeklyintervaldf<-indiv_weekly_dev %>% 
     group_by(intervaltime,weeksold) %>% 
   summarise(interval_avg_deviance=mean(meandeviance),
             interval_weekly_avg=mean(interval_avg_activity))
```

| \#PLOTTING ACTIVITY Setup                                                                                                                                                                          |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Store weeks old as a factor                                                                                                                                                                        |
| `r weeklyintervaldf$weeksold<-as.factor(weeklyintervaldf$weeksold) indiv_weekly_dev$weeksold<-as.factor(indiv_weekly_dev$weeksold)` List of desired hourly time breaks for graph x axis tick marks |
| `r hours_list<-c("08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00")`                                                         |
| \#MEAN DEVIANCE GRAPHS                                                                                                                                                                             |

INDIVIDUAL GRAPHS FOR 1 INDIVIDUAL PUPPY FOR 1 INDIVIDUAL WEEK

``` r
#Leo, 8 weeks
ggplot(subset(indiv_weekly_dev,weeksold=="8" & name=="Leo"), aes(x=intervaltime, y=meandeviance, group=1)) + ylim(-500,2500) + geom_line() + ggtitle("Leo: 8 weeks") + xlab("Time") + ylab("Activity Counts (mean deviance)") + labs(colour = "Age (weeks)") +
  scale_x_discrete(breaks=hours_list)
```

GRAPH OF MEAN DEVIANCE (GROUPED BY PUPPY) FOR 1 SPECIFIC WEEK

``` r
ggplot(subset(indiv_weekly_dev,weeksold=="8"), aes(x=intervaltime, y=meandeviance, group='name')) + geom_line(aes(colour=name)) + ggtitle("Mean Deviance: 8 weeks") + xlab("Time")+ ylab("Mean Deviance") + labs(colour = "Puppy") + scale_x_discrete(breaks=hours_list)
```

COMPARE TWO PUPPIES’ MEAN DEVIANCE FOR 1 SPECIFIC WEEK

``` r
ggplot(subset(indiv_weekly_dev,name=="Rainbow"|name=="Stanley"&weeksold==12), aes(x=intervaltime, y=meandeviance, group=name)) + geom_line(aes(colour=name)) + ggtitle("Mean Deviance: 12 weeks") + xlab("Time")+ ylab("Mean Deviance") + labs(colour = "Puppy") + scale_x_discrete(breaks=hours_list)
```

GROUP AVG FOR 1 INDIVIDUAL WEEK

``` r
#8 weeks
ggplot(subset(weeklyintervaldf,weeksold=="8"), aes(x=intervaltime, y=interval_avg_deviance, group=1)) + geom_line() + ggtitle("Group: 8 weeks") + xlab("Time") + ylab("Activity Counts (mean deviance)") + labs(colour = "Age (weeks)") + scale_x_discrete(breaks=hours_list)
```

GRAPHS OF ALL WEEKS OVERLAID (USING ENTIRE GROUP MEAN DEVIANCE)

``` r
ggplot(weeklyintervaldf, aes(x=intervaltime, y=interval_avg_deviance, group=weeksold)) + geom_line(aes(colour=weeksold)) + ggtitle("Group: 8-20 weeks") + xlab("Time")+ ylab("Activity Counts (mean deviance)") + labs(colour = "Age (weeks)") + scale_x_discrete(breaks=hours_list)
```

GRAPHS OF ALL WEEKS FOR 1 INDIVIDUAL PUPPY

``` r
ggplot(subset(indiv_weekly_dev,name=="Rainbow"), aes(x=intervaltime, y=meandeviance, group=weeksold)) + geom_line(aes(colour=weeksold)) + ggtitle("Rainbow: 8-20 weeks") + xlab("Time") + ylab("Activity Counts (mean deviance)") + labs(colour = "Age (weeks)") + scale_x_discrete(breaks=hours_list)
```

GRAPHS OF GROUP DEVIANCE: 8 VS 20 WEEKS

``` r
ggplot((subset(weeklyintervaldf,weeksold=="8" | weeksold=="20")), aes(x=intervaltime, y=interval_avg_deviance, group=weeksold)) + geom_line(aes(colour=weeksold)) + ggtitle("Group: 8 vs. 20 weeks") + xlab("Time") + ylab("Activity Counts (mean deviance)") +ylab("Activity Counts (mean deviance)") + labs(colour = "Age (weeks)") + scale_x_discrete(breaks=hours_list)
```

GRAPHS OF GROUP DEVIANCE: 8, 14, & 20 WEEKS

``` r
ggplot((subset(weeklyintervaldf,weeksold=="8" | weeksold=="14"| weeksold=="20")), aes(x=intervaltime, y=interval_avg_deviance, group=weeksold)) + geom_line(aes(colour=weeksold)) + ggtitle("Group: 8, 14, & 20 weeks") + xlab("Time") + ylab("Activity Counts (mean deviance)") +ylab("Activity Counts (mean deviance)") + labs(colour = "Age (weeks)") + scale_x_discrete(breaks=hours_list)
```

## \#MEAN ACTIVITY COUNT GRAPHS (raw mean activity, not deviance)

INDIVIDUAL GRAPH OF RAW (MEAN) ACTIVITY COUNTS FOR 1 INDIVIDUAL PUPPY
FOR 1 INDIVIDUAL WEEK

``` r
#Arthur, 8 weeks
#can add ylim(-500,2500)
ggplot(subset(intervaldf,weeksold=="8" & name=="Sassy"), aes(x=intervaltime, y=interval_avg_activity, group=1)) + geom_line() + ggtitle("Arthur: 8 weeks") + xlab("Time") + ylab("Mean Activity Counts") + labs(colour = "Age (weeks)") +
  scale_x_discrete(breaks=hours_list)
```

GRAPH OF RAW (MEAN) ACTIVITY COUNTS GROUPED BY PUPPY) FOR 1 SPECIFIC
WEEK

``` r
ggplot(subset(intervaldf,weeksold=="8"), aes(x=intervaltime, y=interval_avg_activity, group='name')) + geom_line(aes(colour=name)) + ggtitle("Group Activity Counts: 8 weeks") + xlab("Time")+ ylab("Mean Activity Counts") + labs(colour = "Puppy") + scale_x_discrete(breaks=hours_list)

ggplot(subset(intervaldf,weeksold=="18"), aes(x=intervaltime, y=interval_avg_activity, group='name')) + geom_line(aes(colour=name)) + ggtitle("Group Activity Counts: 18 weeks") + xlab("Time")+ ylab("Mean Activity Counts") + labs(colour = "Puppy") + scale_x_discrete(breaks=hours_list)
```

COMPARE TWO PUPPIES’ RAW MEAN ACTIVITY COUNTS FOR 1 SPECIFIC WEEK

``` r
###graph is now fixed!
ggplot(subset(intervaldf,name=="Sassy"&weeksold==8|name=="Arthur"&weeksold==8), aes(x=intervaltime, y=interval_avg_activity, group=name)) + geom_line(aes(colour=name)) + ggtitle("Mean Activity Counts: 8 weeks") + xlab("Time")+ ylab("Mean Activity Counts") + labs(colour = "Puppy") + scale_x_discrete(breaks=hours_list)
```

GROUP AVG OF RAW (MEAN) ACTIVITY COUNTS FOR 1 INDIVIDUAL WEEK

``` r
#8 weeks
ggplot(subset(weeklyintervaldf,weeksold=="8"), aes(x=intervaltime, y=interval_weekly_avg, group=1)) + geom_line() + ggtitle("Group: 8 weeks") + xlab("Time") + ylab("Mean Activity Counts") + labs(colour = "Age (weeks)") + scale_x_discrete(breaks=hours_list)

#20 weeks
ggplot(subset(weeklyintervaldf,weeksold=="20"), aes(x=intervaltime, y=interval_weekly_avg, group=1)) + geom_line() + ggtitle("Group: 20 weeks") + xlab("Time") + ylab("Mean Activity Counts") + labs(colour = "Age (weeks)") + scale_x_discrete(breaks=hours_list)
```

GRAPHS OF ALL WEEKS OVERLAID (USING ENTIRE GROUP RAW MEAN ACTIVITY
COUNTS)

``` r
ggplot(weeklyintervaldf, aes(x=intervaltime, y=interval_weekly_avg, group=weeksold)) + geom_line(aes(colour=weeksold)) + ggtitle("Group: 8-20 weeks") + xlab("Time")+ ylab("Mean Activity Counts") + labs(colour = "Age (weeks)") + scale_x_discrete(breaks=hours_list)
```

GRAPHS OF GROUP: 8 VS 20 WEEKS (RAW MEAN ACTIVITY COUNTS)

``` r
ggplot((subset(weeklyintervaldf,weeksold=="8" | weeksold=="20")), aes(x=intervaltime, y=interval_weekly_avg, group=weeksold)) + geom_line(aes(colour=weeksold)) + ggtitle("Group: 8 vs. 20 weeks") + xlab("Time") + ylab("Mean Activity Counts") + labs(colour = "Age (weeks)") + scale_x_discrete(breaks=hours_list)
```

GRAPHS OF GROUP: 8, 14, & 20 WEEKS

``` r
ggplot((subset(weeklyintervaldf,weeksold=="8" | weeksold=="14"| weeksold=="20")), aes(x=intervaltime, y=interval_weekly_avg, group=weeksold)) + geom_line(aes(colour=weeksold)) + ggtitle("Group: 8, 14, & 20 weeks") + xlab("Time") + ylab("Mean Activity Counts") + labs(colour = "Age (weeks)") + scale_x_discrete(breaks=hours_list)
```

| \#CALCULATE SLEEP BOUTS                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Rest/wake thresholds                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| \`\`\`r \#set threshold for sleeping/awake sleepbouts&lt;-indiv\_weekly\_dev %&gt;% mutate(state=case\_when(interval\_avg\_activity &gt;= 100 \~ “awake”,interval\_avg\_activity &lt;100 \~ “rest”))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| \#make column for interval end time library(lubridate) sleepbouts &lt;-sleepbouts %&gt;% mutate(intervaltime2=as.POSIXct(intervaltime, format=“%H:%M”)) sleepbouts &lt;-sleepbouts %&gt;% mutate(intervalend=(intervaltime2+(5\*60))) sleepbouts &lt;-sleepbouts %&gt;% mutate(intervalend=strftime(intervalend, format=“%H:%M”)) sleepbouts &lt;-sleepbouts %&gt;% select(-intervaltime2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| \#assign group numbers to unique sleep periods sleepbouts&lt;-sleepbouts %&gt;% mutate(timeperiod = hm(intervaltime), timeperiod\_end = hm(intervalend)) %&gt;% group\_by(name,weeksold) %&gt;% arrange(intervaltime) %&gt;% mutate (group = data.table::rleid(state))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| view(sleepbouts) \`\`\`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Intervals of resting and wakefulness                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| \`\`\`r sleepbout\_intervals&lt;-sleepbouts %&gt;% group\_by(name,weeksold,group) %&gt;% summarise(intervaltime = paste(first(intervaltime), last(intervalend), sep = ‘-’), state = first(state), timeperiod = last(timeperiod\_end) - first(timeperiod))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| \#Convert timeperiod class to minutes library(lubridate) sleepbout\_intervals&lt;-sleepbout\_intervals %&gt;% mutate(timeperiod\_secs=time\_length(timeperiod), timeperiod\_mins=timeperiod\_secs/60) %&gt;% select(-timeperiod\_secs)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| view(sleepbout\_intervals)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| \#Find average & total length of rest vs awake states for each week sleepbout\_weeklyavg &lt;- sleepbout\_intervals %&gt;% group\_by(weeksold,state) %&gt;% summarize(Mean\_mins=mean(timeperiod\_mins), N=n(),Total\_mins=sum(timeperiod\_mins)) view(sleepbout\_weeklyavg) \`\`\`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| Calculate rest/wake duration (avg and total) for each puppy for each week, add rearing & sex info                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| \`\`\`r sleepbout\_weeklyavg\_bypuppy &lt;- sleepbout\_intervals %&gt;% group\_by(weeksold,name,state) %&gt;% summarize(Mean\_mins=mean(timeperiod\_mins), N=n(),Total\_mins=sum(timeperiod\_mins))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| sleepbout\_weeklyavg\_bypuppy*w**e**e**k**s**o**l**d* &lt;  − *a**s*.*c**h**a**r**a**c**t**e**r*(*s**l**e**e**p**b**o**u**t*<sub>*w*</sub>*e**e**k**l**y**a**v**g*<sub>*b*</sub>*y**p**u**p**p**y*weeksold) sleepbout\_weeklyavg\_bypuppy&lt;-sleepbout\_weeklyavg\_bypuppy %&gt;% mutate(rearing = case\_when(name == “Anya” \~ “DPK”, name == “Aries” \~ “DPK”, name == “Weston” \~ “DPK”, name == “Ying” \~ “DPK”, name == “Yolanda” \~ “DPK”, name == “Zina” \~ “DPK”, name == “Zax” \~ “DPK”, name == “Arthur” & weeksold &lt;10 \~ “home”, name == “Arthur” & weeksold &gt;9 \~ “DPK”, name == “Aurora” & weeksold &lt;10 \~ “home”, name == “Aurora” & weeksold &gt;9 \~ “DPK”, name == “Westley” & weeksold &lt;10 \~ “home”, name == “Westley” & weeksold &gt;9 \~ “DPK”, name == “Wisdom” & weeksold &lt;10 \~ “home”, name == “Wisdom” & weeksold &gt;9 \~ “DPK”, name == “Yonder” & weeksold &lt;10 \~ “home”, name == “Yonder” & weeksold &gt;9 \~ “DPK”, name == “Zindel” & weeksold &lt; 10 \~ “home”, name == “Zindel” & weeksold &gt;9 \~ “DPK”, name == “Zola” & weeksold &lt;10 \~ “home”, name == “Zola” & weeksold &gt;9 \~ “DPK”, name == “Kristoff” \~ “home”, name == “Barley” \~ “home”, name == “Rainbow” \~ “home”, name == “Sassy” \~ “home”, name == “Sparky” \~ “home”, name == “Stanley” \~ “home”, name == “Lily” \~ “home”, name == “Leo” \~ “home”, name == “Jack” \~ “home”), sex=case\_when(name == “Anya” \~ “F”, name == “Aries” \~ “M”, name == “Weston” \~ “M”, name == “Ying” \~ “M”, name == “Yolanda” \~ “F”, name == “Zina” \~ “F”, name == “Zax” \~ “M”, name == “Arthur” \~ “M”, name == “Aurora” \~ “F”, name == “Westley” \~ “M”, name == “Wisdom” \~ “F”, name == “Yonder” \~ “F”, name == “Zola” \~ “F”, name == “Zindel” \~ “M”, name == “Kristoff” \~ “M”, name == “Barley” \~ “M”, name == “Rainbow” \~ “F”, name == “Sassy” \~ “F”, name == “Sparky” \~ “M”, name == “Stanley” \~ “M”, name == “Lily” \~ “F”, name == “Leo” \~ “M”, name == “Jack” \~ “M”)) sleepbout\_weeklyavg\_bypuppy*w**e**e**k**s**o**l**d* &lt;  − *a**s*.*n**u**m**e**r**i**c*(*a**s*.*c**h**a**r**a**c**t**e**r*(*s**l**e**e**p**b**o**u**t*<sub>*w*</sub>*e**e**k**l**y**a**v**g*<sub>*b*</sub>*y**p**u**p**p**y*weeksold))            |
| view(sleepbout\_weeklyavg\_bypuppy) \`\`\` Compare home vs. DPK pups                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| `r sleepbout_weeklyavg_rearing <- sleepbout_weeklyavg_bypuppy %>% group_by(weeksold,state,rearing) %>% summarize(Mean_mins=mean(Mean_mins), N=n(),Total_mins=sum(Total_mins)) sleepbout_weeklyavg_rearing$weeksold<-as.numeric(as.character(sleepbout_weeklyavg_rearing$weeksold))`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| Without collapsing across days:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| \`\`\`r \#set threshold for sleeping/awake sleepbouts2&lt;-intervaldf2 %&gt;% mutate(state=case\_when(interval\_avg\_activity &gt;= 100 \~ “awake”,interval\_avg\_activity &lt;100 \~ “rest”))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| \#make column for interval end time library(lubridate) sleepbouts2 &lt;-sleepbouts2 %&gt;% mutate(intervaltime2=as.POSIXct(intervaltime, format=“%H:%M”)) sleepbouts2 &lt;-sleepbouts2 %&gt;% mutate(intervalend=(intervaltime2+(5\*60))) sleepbouts2 &lt;-sleepbouts2 %&gt;% mutate(intervalend=strftime(intervalend, format=“%H:%M”)) sleepbouts2 &lt;-sleepbouts2 %&gt;% select(-intervaltime2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| \#assign group numbers to unique sleep periods sleepbouts2&lt;-sleepbouts2 %&gt;% mutate(timeperiod = hm(intervaltime), timeperiod\_end = hm(intervalend)) %&gt;% group\_by(name,weeksold) %&gt;% arrange(Date,name, intervaltime) %&gt;% mutate (group = data.table::rleid(state))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| view(sleepbouts2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| \#Intervals of resting and wakefulness for each **DAY** sleepbout\_intervals2&lt;-sleepbouts2 %&gt;% group\_by(name,Date,weeksold,group) %&gt;% summarise(intervaltime = paste(first(intervaltime), last(intervalend), sep = ‘-’), state = first(state), timeperiod = last(timeperiod\_end) - first(timeperiod))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| library(lubridate) sleepbout\_intervals2&lt;-sleepbout\_intervals2 %&gt;% mutate(timeperiod\_secs=time\_length(timeperiod), timeperiod\_mins=timeperiod\_secs/60) %&gt;% select(-timeperiod\_secs) \#\#\#\#start here \#collapse to get average in each interval per week sleepbout\_intervals2\_collapsed\_bypuppy&lt;-sleepbout\_intervals2 %&gt;% group\_by(weeksold,name,intervaltime,state) %&gt;%summarise(interval\_avg\_activity=mean(timeperiod\_mins))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| sleepbout\_intervals2\_collapsed&lt;-sleepbout\_intervals2 %&gt;% group\_by(weeksold,intervaltime,state) %&gt;%summarise(interval\_avg\_activity=mean(timeperiod\_mins))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| sleepbout\_weeklyavg2 &lt;- sleepbout\_intervals2 %&gt;% group\_by(weeksold,state) %&gt;% summarize(Mean\_mins=mean(timeperiod\_mins), N=n(),Total\_mins=sum(timeperiod\_mins))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| sleepbout\_weeklyavg\_bypuppy2 &lt;- sleepbout\_intervals2 %&gt;% group\_by(weeksold,name,state) %&gt;% summarize(Mean\_mins=mean(timeperiod\_mins), N=n(),Total\_mins=sum(timeperiod\_mins))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| sleepbout\_weeklyavg\_bypuppy2*w**e**e**k**s**o**l**d* &lt;  − *a**s*.*c**h**a**r**a**c**t**e**r*(*s**l**e**e**p**b**o**u**t*<sub>*w*</sub>*e**e**k**l**y**a**v**g*<sub>*b*</sub>*y**p**u**p**p**y*2weeksold) sleepbout\_weeklyavg\_bypuppy2&lt;-sleepbout\_weeklyavg\_bypuppy2 %&gt;% mutate(rearing = case\_when(name == “Anya” \~ “DPK”, name == “Aries” \~ “DPK”, name == “Weston” \~ “DPK”, name == “Ying” \~ “DPK”, name == “Yolanda” \~ “DPK”, name == “Zina” \~ “DPK”, name == “Zax” \~ “DPK”, name == “Arthur” & weeksold &lt;10 \~ “home”, name == “Arthur” & weeksold &gt;9 \~ “DPK”, name == “Aurora” & weeksold &lt;10 \~ “home”, name == “Aurora” & weeksold &gt;9 \~ “DPK”, name == “Westley” & weeksold &lt;10 \~ “home”, name == “Westley” & weeksold &gt;9 \~ “DPK”, name == “Wisdom” & weeksold &lt;10 \~ “home”, name == “Wisdom” & weeksold &gt;9 \~ “DPK”, name == “Yonder” & weeksold &lt;10 \~ “home”, name == “Yonder” & weeksold &gt;9 \~ “DPK”, name == “Zindel” & weeksold &lt; 10 \~ “home”, name == “Zindel” & weeksold &gt;9 \~ “DPK”, name == “Zola” & weeksold &lt;10 \~ “home”, name == “Zola” & weeksold &gt;9 \~ “DPK”, name == “Kristoff” \~ “home”, name == “Barley” \~ “home”, name == “Rainbow” \~ “home”, name == “Sassy” \~ “home”, name == “Sparky” \~ “home”, name == “Stanley” \~ “home”, name == “Lily” \~ “home”, name == “Leo” \~ “home”, name == “Jack” \~ “home”), sex=case\_when(name == “Anya” \~ “F”, name == “Aries” \~ “M”, name == “Weston” \~ “M”, name == “Ying” \~ “M”, name == “Yolanda” \~ “F”, name == “Zina” \~ “F”, name == “Zax” \~ “M”, name == “Arthur” \~ “M”, name == “Aurora” \~ “F”, name == “Westley” \~ “M”, name == “Wisdom” \~ “F”, name == “Yonder” \~ “F”, name == “Zola” \~ “F”, name == “Zindel” \~ “M”, name == “Kristoff” \~ “M”, name == “Barley” \~ “M”, name == “Rainbow” \~ “F”, name == “Sassy” \~ “F”, name == “Sparky” \~ “M”, name == “Stanley” \~ “M”, name == “Lily” \~ “F”, name == “Leo” \~ “M”, name == “Jack” \~ “M”)) sleepbout\_weeklyavg\_bypuppy*w**e**e**k**s**o**l**d* &lt;  − *a**s*.*n**u**m**e**r**i**c*(*a**s*.*c**h**a**r**a**c**t**e**r*(*s**l**e**e**p**b**o**u**t*<sub>*w*</sub>*e**e**k**l**y**a**v**g*<sub>*b*</sub>*y**p**u**p**p**y*weeksold)) \`\`\` |

## \#PLOTTING REST/WAKE BOUTS

Compare average rest/wake bout durations in 1 individual week

``` r
ggplot(subset(sleepbout_weeklyavg,weeksold=="20"), aes(x=state, y=Mean_mins))+ geom_col(aes(fill=state)) + ggtitle("Average Sleep/Awake Bout Duration: 20 weeks") + xlab("State")+ ylab("Mean State Duration (min)") + labs(colour = "State")
```

Compare average rest/awake bout durations for each week

``` r
ggplot(subset(sleepbout_weeklyavg,state=="awake"), aes(x=weeksold, y=Mean_mins))+ geom_col(aes(fill=weeksold)) + ggtitle("Avg Duration of Awake Bouts by Week") + xlab("Weeks Old")+ ylab("Mean State Duration (min)") + labs(colour = "Age (weeks)")

ggplot(subset(sleepbout_weeklyavg,state=="rest"), aes(x=weeksold, y=Mean_mins))+ geom_col(aes(fill=weeksold)) + ggtitle("Avg Duration of Rest Bouts by Week") + xlab("Weeks Old")+ ylab("Mean State Duration (min)") + labs(colour = "Age (weeks)")
```

Total Minutes of Rest by Week

``` r
ggplot(subset(sleepbout_weeklyavg,state=="rest"), aes(x=weeksold, y=Total_mins))+ geom_col(aes(fill=weeksold)) + ggtitle("Average Total Rest by Week") + xlab("Weeks Old")+ ylab("Total Rest (min)") + labs(colour = "Age (weeks)")
```

AVG REST HOME VS. DPK

``` r
weeks<-c(8:20)

ggplot(subset(sleepbout_weeklyavg_rearing,state=="rest"), aes(x=weeksold, y=Mean_mins, colour=rearing, group=rearing)) + geom_point() + geom_line() + ggtitle("Avg Duration of Rest Bouts by Week: Home vs. DPK") + xlab("Weeks Old")+ ylab("Avg State Duration (min)") + labs(colour = "Rearing") + scale_x_continuous(breaks=weeks)

ggplot(subset(sleepbout_weeklyavg_rearing,state=="rest"), aes(x=weeksold, y=Total_mins, colour=rearing, group=rearing)) + geom_point() + geom_line() + ggtitle("Average Total Rest by Week: Home vs. DPK") + xlab("Weeks Old")+ ylab("Rest (mins)") + labs(colour = "Rearing") + scale_x_continuous(breaks=weeks)
```

## \#BY PUPPY

Bar graph: Mean State Duration for 1 puppy by weeks

``` r
ggplot(subset(sleepbout_weeklyavg_bypuppy, name=="Leo"), aes(x=weeksold, y=Mean_mins))+ geom_col(aes(fill=state)) + ggtitle("Leo: Mean Rest State Duration by Week") + xlab("Weeks Old")+ ylab("Mean State Duration (min)") + labs(colour = "Age (weeks)")+ scale_x_continuous(breaks=weeks)
```

Line graph: Mean State Duration for 1 puppy by weeks

``` r
ggplot(subset(sleepbout_weeklyavg_bypuppy, name=="Leo"), aes(x=weeksold, y=Mean_mins, group=state))+ geom_line(aes(colour=state)) + ggtitle("Leo: Mean Rest State Duration by Week") + xlab("Weeks Old")+ ylab("Mean State Duration (min)") + labs(colour = "Age (weeks)")+ scale_x_continuous(breaks=weeks)
```

Line graph: mean rest state duration by week for each puppy

``` r
ggplot(subset(sleepbout_weeklyavg_bypuppy, state=="rest"), aes(x=weeksold, y=Mean_mins, group=name))+ geom_line(aes(colour=name)) + ggtitle("Mean Rest State Duration by Week") + xlab("Weeks Old")+ ylab("Mean State Duration (min)") + labs(colour = "Age (weeks)")+ scale_x_continuous(breaks=weeks)
```

## \#EXPORT

EXPORT dfs individually

EXPORT dfs as sheets on one doc
