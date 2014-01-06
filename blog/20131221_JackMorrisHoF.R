# Jack Morris and the Hall of Fame

# Reference:  http://baseballwithr.wordpress.com/2013/12/21/jack-morris-an…e-hall-of-fame/

# Need 2013 Lahman's database (http://www.seanlahman.com/baseball-archive/statistics/)

options(stringsAsFactors=F)
#set working dir
setwd("your/directory/containing/Lahman/DB")

#read master and HOF data
master = read.csv("Master.csv")
HoF = read.csv("HallOfFame.csv")

#select pitcher
pitcher = "Jack Morris"
#get HOF voting data
pitHoFid = subset(master, paste(nameFirst, nameLast)==pitcher)$hofID
pitHOF = subset(HoF, hofID==pitHoFid)
pitHOF$pct = 100 * pitHOF$votes / pitHOF$ballots
#plot voting path
library(ggplot2)
ggplot(data=pitHOF, aes(x=yearid, y=pct)) +
  geom_line(size=1.2) +
  ylim(c(0,100)) +
  geom_hline(yintercept=75, linetype=2) +
  geom_text(x=min(pitHOF$yearid), y=77, label="induction threshold", hjust=0) +
  xlab("year of ballot") +
  ylab("percentage of favorable ballots") +
  ggtitle(pitcher) +
  scale_x_continuous(breaks=min(pitHOF$yearid):max(pitHOF$yearid))

# get pitching data
pitching = read.csv("Pitching.csv")
# season totals by pitcher
library(doBy)
pitching = summaryBy(ER + IPouts + SO + BB ~ playerID + yearID
                     , data=pitching, FUN=sum, keep.names=T)
pitching$ERA = pitching$ER * 27 / pitching$IPouts
pitching$K9 = pitching$SO * 27 / pitching$IPouts
pitching$W9 = pitching$BB * 27 / pitching$IPouts

# get selected pitcher's data
pitID = subset(master, paste(nameFirst, nameLast)==pitcher)$playerID
pitData = subset(pitching, playerID==pitID)

# get contemporaries of selected pitcher (qualifying only)
contemporaries = subset(pitching
                         , yearID >= min(pitData$yearID) 
                         & yearID <= max(pitData$yearID)
                         & IPouts >= 162*3)

# compare ERA with contemporaries
ggplot(data=pitData, aes(x=factor(yearID), y=ERA)) +
  geom_boxplot(data=contemporaries, aes(x=factor(yearID), y=ERA)) +
  geom_point(data=contemporaries, aes(x=factor(yearID), y=ERA), position=position_jitter(width = 0.15), alpha=.6) +
  geom_point(col="blue", size=5) +
  xlab("season") +
  ggtitle(paste(pitcher, "(ERA)"))

# compare K9 with contemporaries
ggplot(data=pitData, aes(x=factor(yearID), y=K9)) +
  geom_boxplot(data=contemporaries, aes(x=factor(yearID), y=K9)) +
  geom_point(data=contemporaries, aes(x=factor(yearID), y=K9), position=position_jitter(width = 0.15), alpha=.6) +
  geom_point(col="blue", size=5) +
  xlab("season") +
  ggtitle(paste(pitcher, "(K/9)"))

# compare W9 with contemporaries (aes_string)
ggplot(data=pitData, aes(x=factor(yearID), y=W9)) +
  geom_boxplot(data=contemporaries, aes(x=factor(yearID), y=W9)) +
  geom_point(data=contemporaries, aes(x=factor(yearID), y=W9), position=position_jitter(width = 0.15), alpha=.6) +
  geom_point(col="blue", size=5) +
  xlab("season") +
  ggtitle(paste(pitcher, "(W/9)"))
