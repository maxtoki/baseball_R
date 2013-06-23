#####################################################
# Chapter 5:  Value of Plays Using Run Expectancy
#
# datafiles:  all2001.csv, fields.csv, roster2011.csv
# R packages:  plyr, MASS
#
#####################################################

##################################################
# 5.2  Runs Scored in the Remainder of the Inning
##################################################

data2011 <- read.csv("all2011.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data2011) <- fields[, "Header"]

data2011$RUNS <- with(data2011, AWAY_SCORE_CT + HOME_SCORE_CT)
data2011$HALF.INNING <- with(data2011, 
                            paste(GAME_ID, INN_CT, BAT_HOME_ID))

data2011$RUNS.SCORED <- with(data2011, (BAT_DEST_ID > 3) +
  (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

RUNS.SCORED.INNING <- aggregate(data2011$RUNS.SCORED, 
                        list(HALF.INNING = data2011$HALF.INNING), sum)

RUNS.SCORED.START <- aggregate(data2011$RUNS, 
                       list(HALF.INNING = data2011$HALF.INNING), "[", 1)

MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
data2011 <- merge(data2011, MAX)
N <- ncol(data2011)
names(data2011)[N] <- "MAX.RUNS"

data2011$RUNS.ROI <- data2011$MAX.RUNS - data2011$RUNS

##################################################
# 5.3  Creating the Matrix
##################################################

get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)                      
}

RUNNER1 <- ifelse(as.character(data2011[,"BASE1_RUN_ID"])=="", 0, 1)
RUNNER2 <- ifelse(as.character(data2011[,"BASE2_RUN_ID"])=="", 0, 1)
RUNNER3 <- ifelse(as.character(data2011[,"BASE3_RUN_ID"])=="", 0, 1)
data2011$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2011$OUTS_CT)

NRUNNER1 <- with(data2011, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
NRUNNER2 <- with(data2011, as.numeric(RUN1_DEST_ID==2 | RUN2_DEST_ID==2 | BAT_DEST_ID==2))
NRUNNER3 <- with(data2011, as.numeric(RUN1_DEST_ID==3 | RUN2_DEST_ID==3 |
  RUN3_DEST_ID==3 | BAT_DEST_ID==3))
NOUTS <- with(data2011, OUTS_CT + EVENT_OUTS_CT)

data2011$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

data2011 <- subset(data2011, (STATE!=NEW.STATE) | (RUNS.SCORED>0))

library(plyr)
data.outs <- ddply(data2011, .(HALF.INNING), summarize,
                  Outs.Inning = sum(EVENT_OUTS_CT))
data2011 <- merge(data2011, data.outs)
data2011C <- subset(data2011, Outs.Inning == 3)

RUNS <- with(data2011C, aggregate(RUNS.ROI, list(STATE), mean))
RUNS$Outs <- substr(RUNS$Group, 5, 5)
RUNS <- RUNS[order(RUNS$Outs), ]

RUNS.out <- matrix(round(RUNS$x, 2), 8, 3)
dimnames(RUNS.out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")

RUNS.2002 <- matrix(c(.51, 1.40, 1.14,  1.96, .90, 1.84, 1.51, 2.33,
               .27,  .94,  .68,  1.36, .54, 1.18,  .94, 1.51,
               .10,  .36,  .32,   .63, .23, .52,   .45, .78),
               8, 3)
dimnames(RUNS.2002) <- dimnames(RUNS.out)

cbind(RUNS.out, RUNS.2002)

##################################################
# 5.4  Measuring Success of a Batting Play
##################################################

RUNS.POTENTIAL <- matrix(c(RUNS$x, rep(0, 8)), 32, 1)
dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$Group, "000 3","001 3",
                                   "010 3","011 3","100 3","101 3","110 3","111 3") 
data2011$RUNS.STATE <- RUNS.POTENTIAL[data2011$STATE,]
data2011$RUNS.NEW.STATE <- RUNS.POTENTIAL[data2011$NEW.STATE,]
data2011$RUNS.VALUE <- data2011$RUNS.NEW.STATE - data2011$RUNS.STATE + 
  data2011$RUNS.SCORED

##################################################
# 5.5  Albert Pujols
##################################################

Roster <- read.csv("roster2011.csv")
albert.id <- subset(Roster, First.Name == "Albert" &
                     Last.Name == "Pujols")$Player.ID
albert.id <- as.character(albert.id)
albert <- subset(data2011, BAT_ID==albert.id)
albert <- subset(albert, BAT_EVENT_FL==TRUE)

albert[1:2, c("STATE", "NEW.STATE", "RUNS.VALUE")]

albert$RUNNERS <- substr(albert$STATE, 1, 3)
table(albert$RUNNERS)

with(albert, stripchart(RUNS.VALUE ~ RUNNERS, vertical=TRUE, jitter=0.2,
                        xlab="RUNNERS", method="jitter", pch=1, cex = 0.8))
abline(h=0)

A.runs <- aggregate(albert$RUNS.VALUE, list(albert$RUNNERS), sum)
names(A.runs)[2] <- "RUNS"
A.PA <- aggregate(albert$RUNS.VALUE, list(albert$RUNNERS), length)
names(A.PA)[2] <- "PA"
A <- merge(A.PA, A.runs)
A

sum(A$RUNS)

##################################################
# 5.6  Opportunity and Success for All Hitters
##################################################

data2011b <- subset(data2011, BAT_EVENT_FL == TRUE)

runs.sums <- aggregate(data2011b$RUNS.VALUE, list(data2011b$BAT_ID), sum)
runs.pa <- aggregate(data2011b$RUNS.VALUE, list(data2011b$BAT_ID), length)
runs.start <- aggregate(data2011b$RUNS.STATE, list(data2011b$BAT_ID), sum)
names(runs.sums) <- c("Batter", "Runs")
names(runs.pa) <- c("Batter", "PA")
names(runs.start) <- c("Batter", "Runs.Start")
runs <- merge(runs.sums, runs.pa)
runs <- merge(runs, runs.start)

runs400 <- subset(runs, PA >=400)
head(runs400)

with(runs400, plot(Runs.Start, Runs))
with(runs400, lines(lowess(Runs.Start, Runs)))
abline(h=0)

runs400.top <- subset(runs400, Runs >=40)
roster2011 <- read.csv("roster2011.csv")  
runs400.top <- merge(runs400.top, 
              roster2011, by.x="Batter", by.y="Player.ID")
with(runs400.top, text(Runs.Start, Runs, Last.Name, pos=1))

##################################################
# 5.7  Position in the Batting Lineup
##################################################

get.batting.pos <- function(batter){
  TB <- table(subset(data2011,BAT_ID==batter)$BAT_LINEUP_ID)
  names(TB)[TB==max(TB)][1]}

position <- sapply(as.character(runs400$Batter), get.batting.pos)

with(runs400, plot(Runs.Start, Runs, type="n"))
with(runs400, lines(lowess(Runs.Start, Runs)))
abline(h=0)
with(runs400, text(Runs.Start, Runs, position))

AP <- subset(runs400, Batter==albert.id)
points(AP$Runs.Start, AP$Runs, pch=19, cex=3)

##################################################
# 5.8 Run Values of Different Base Hits
##################################################

d.homerun <- subset(data2011, EVENT_CD==23)

table(d.homerun$STATE)

round(prop.table(table(d.homerun$STATE)), 3)

library(MASS)
truehist(d.homerun$RUNS.VALUE)

subset(d.homerun, RUNS.VALUE==max(RUNS.VALUE))[1, 
      c("STATE", "NEW.STATE", "RUNS.VALUE")]

mean.HR <- mean(d.homerun$RUNS.VALUE)
mean.HR

abline(v = mean.HR, lwd=3)
text(1.5, 5, "Mean Runs Value", pos=4)

d.single <- subset(data2011, EVENT_CD == 20)
#library(MASS)
truehist(d.single$RUNS.VALUE)

table(d.single$STATE)

subset(d.single, d.single$RUNS.VALUE==
  max(d.single$RUNS.VALUE))[, c("STATE", "NEW.STATE", "RUNS.VALUE")]

subset(d.single, d.single$RUNS.VALUE == min(d.single$RUNS.VALUE))[
  , c("STATE", "NEW.STATE", "RUNS.VALUE")]

mean.single <- mean(d.single$RUNS.VALUE)
mean.single
abline(v = mean.single, lwd=3)
text(.5, 5, "Mean Runs Value", pos=4)

##################################################
# 5.9  Value of Base Stealing
##################################################

stealing <- subset(data2011, EVENT_CD==6 | EVENT_CD==4)

table(stealing$EVENT_CD)

table(stealing$STATE)

#library(MASS)
truehist(stealing$RUNS.VALUE)

stealing.1001 <- subset(stealing, STATE=="100 1")

table(stealing.1001$EVENT_CD)

with(stealing.1001, table(NEW.STATE))

mean(stealing.1001$RUNS.VALUE)

#######################################################





