#################################################
# Chapter 4 - The Relation Between Runs and Wins
# 
# Needs .csv files from the Lahman's database
#         (placed in the "lahman" subfolder)
#
#################################################

# Section 4.2  The Teams Table in Lahman's Database

teams <- read.csv("lahman/teams.csv")
tail(teams)

myteams <- subset(teams, yearID > 2000)[ , c("teamID", "yearID",
  "lgID", "G", "W", "L", "R", "RA")]

tail(myteams)

myteams$RD <- with(myteams, R - RA)
myteams$Wpct <- with(myteams, W / (W + L))

plot(myteams$RD, myteams$Wpct,
  xlab="run differential",
  ylab="winning percentage")

# Section 4.3 Linear Regression

linfit <- lm(Wpct ~ RD, data=myteams)

abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

myteams$linWpct <- predict(linfit)
myteams$linResiduals <- residuals(linfit)

plot(myteams$RD, myteams$linResiduals,
  xlab="run differential",
  ylab="residual")
abline(h=0, lty=3)
points(c(68, 88), c(.0749, -.0733), pch=19)
text(68, .0749, "LAA '08", pos=4, cex=.8)
text(88, -.0733, "CLE '06", pos=4, cex=.8)

mean(myteams$linResiduals)

linRMSE <- sqrt(mean(myteams$linResiduals ^ 2))
linRMSE

nrow(subset(myteams, abs(linResiduals) < linRMSE)) /
  nrow(myteams)

nrow(subset(myteams, abs(linResiduals) < 2 * linRMSE)) /
  nrow(myteams)

# Section 4.4 The Pythagorean Formula for Winning Percentage

myteams$pytWpct <- with(myteams, R ^ 2 / (R ^ 2 + RA ^ 2))

myteams$pytResiduals <- myteams$Wpct - myteams$pytWpct
sqrt(mean(myteams$pytResiduals ^ 2))

# Section 4.5  The Exponent in the Pythagorean Formula

myteams$logWratio <- log(myteams$W / myteams$L)
myteams$logRratio <- log(myteams$R / myteams$RA)
pytFit <- lm(logWratio ~ 0 + logRratio, data=myteams)
pytFit

# Section 4.6  Good and Bad Predictions by the Pythagorean Formula

gl2011 <- read.table("gl2011.txt", sep=",")
glheaders <- read.csv("game_log_header.csv")
names(gl2011) <- names(glheaders)
BOS2011 <- subset(gl2011, HomeTeam=="BOS" | VisitingTeam=="BOS")[
  , c("VisitingTeam", "HomeTeam", "VisitorRunsScored",
  "HomeRunsScore")]
head(BOS2011)

BOS2011$ScoreDiff <- with(BOS2011, ifelse(HomeTeam == "BOS",
  HomeRunsScore - VisitorRunsScored,
  VisitorRunsScored - HomeRunsScore))
BOS2011$W <- BOS2011$ScoreDiff > 0

aggregate(abs(BOS2011$ScoreDiff), list(W=BOS2011$W), summary)

results <- gl2011[,c("VisitingTeam", "HomeTeam",
  "VisitorRunsScored", "HomeRunsScore")]
results$winner <- ifelse(results$HomeRunsScore >
   results$VisitorRunsScored, as.character(results$HomeTeam),
   as.character(results$VisitingTeam))
results$diff <- abs(results$VisitorRunsScored -
  results$HomeRunsScore)

onerungames <- subset(results, diff == 1)
onerunwins <- as.data.frame(table(onerungames$winner))
names(onerunwins) <- c("teamID", "onerunW")

teams2011 <- subset(myteams, yearID == 2011)
teams2011[teams2011$teamID == "LAA", "teamID"] <- "ANA"
teams2011 <- merge(teams2011, onerunwins)
plot(teams2011$onerunW, teams2011$pytResiduals,
  xlab="one run wins",
  ylab="Pythagorean residuals")

identify(teams2011$onerunW, teams2011$pytResiduals,
  labels=teams2011$teamID)
  #...identify data points by mouse-clickin on the plot
  #...then press ESC to finish

pit <- read.csv("lahman/pitching.csv")
top_closers <- subset(pit, GF > 50 & ERA < 2.5)[ ,c("playerID",
  "yearID", "teamID")]

teams_top_closers <- merge(myteams, top_closers)
summary(teams_top_closers$pytResiduals)

# Section 4.7  How Many Runs for a Win?

D(expression(G * R ^ 2 / (R ^ 2 + RA ^ 2)), "R")

IR <- function(RS=5, RA=5){
  round((RS ^ 2 + RA ^ 2)^2 / (2 * RS * RA ^ 2), 1)
}

IRtable <- expand.grid(RS=seq(3, 6, .5), RA=seq(3, 6, .5))
rbind(head(IRtable), tail(IRtable))

IRtable$IRW <- IR(IRtable$RS, IRtable$RA)
xtabs(IRW ~ RS + RA, data=IRtable)

################################################