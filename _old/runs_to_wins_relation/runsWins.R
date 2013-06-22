#####################################
# runs-to-wins relation chapter     #
#                                   #
# required packages:                #
#                                   #
# files used:                       #
# teams.csv (Lahman)                #
# pitching.csv (Lahman)             #
# gl2011.txt (Retrosheet game logs) #
# game_log_header.csv (headers for  #
#                      RS game logs)#
#####################################

# set the working directory (where lahmans' csv files are saved)
setwd("set-working-folder")


########################################################
# exploring run-differential/winning-percentage relation

# read the teams files and show the final lines of the data frame
teams <- read.csv("lahman/teams.csv")
tail(teams)

# subset to seasons since 2001
myteams <- subset(teams, yearID > 2000)[,c("teamID", "yearID", 
                                           "lgID", "G", "W", "L", "R", "RA")]
tail(myteams)

# calculate run differential and winning percentage
myteams$RD <- with(myteams, R - RA)
myteams$Wpct <- with(myteams, W / (W + L))

# scatter plot
plot(myteams$RD, myteams$Wpct,
     xlab = "run differential",
     ylab = "winning percentage")


###################
# linear regression

# fit linear model
linfit <- lm(Wpct ~ RD, data = myteams)
linfit
# add fitted line to previous plot
abline(a = coef(linfit)[1],
       b= coef(linfit)[2],
       lwd=2)

# calculate estimated Wpct and residuals
myteams$linWpct <- predict(linfit)
myteams$linResiduals <- residuals(linfit)

plot(myteams$RD, myteams$linResiduals,
     xlab = "run differential",
     ylab = "residual")
abline(h=0, lty=3)  
points(c(68,88), c(.0749, -.0733), pch=19)	
text(68, .0749, "LAA '08", pos=4, cex=.8)	
text(88, -.0733, "CLE '06", pos=4, cex=.8)	

#...average error
mean(myteams$linResiduals)

#...root mean square error
linRMSE <- sqrt(mean(myteams$linResiduals ^ 2))
linRMSE

#...proportions of residuals falling between -/+ RMSE
nrow(subset(myteams, abs(linResiduals) < linRMSE)) / 
  nrow(myteams)
#...proportions of residuals falling between -/+ 2*RMSE
nrow(subset(myteams, abs(linResiduals) < 2 * linRMSE)) / 
  nrow(myteams)



################################
# Bill James Pythagorean formula

# calculate pythagorean Wpct
myteams$pytWpct <- with(myteams, R ^ 2 / (R ^ 2 + RA ^ 2))

# calculate residuals and RMSE
myteams$pytResiduals <- myteams$Wpct - myteams$pytWpct
sqrt(mean(myteams$pytResiduals ^ 2))

# estimate the exponent (instead of fixed 2)
myteams$logWratio <- log(myteams$W / myteams$L)
myteams$logRratio <- log(myteams$R / myteams$RA)
pytFit <- lm(logWratio ~ 0 + logRratio, data = myteams)
pytFit


############################
# 2011 Boston Red Sox
# underperforming Pythagoras

# read the game log file
gl2011 <- read.table("retrosheet/gl2011.txt", sep=",")  
#...read the headers
glheaders <- read.csv("game_log_header.csv")	
#...apply the headers to the dataset
names(gl2011) <- names(glheaders)	
#select runs info for Sox games
BOS2011 <- subset(gl2011, HomeTeam=="BOS" | VisitingTeam=="BOS")[
  , c("VisitingTeam", "HomeTeam", "VisitorRunsScored", "HomeRunsScore")]		
head(BOS2011)
#...run differential (positive means Sox win)
BOS2011$ScoreDiff <- with(BOS2011, ifelse(HomeTeam=="BOS", HomeRunsScore - VisitorRunsScored, VisitorRunsScored - HomeRunsScore))  
BOS2011$W <- BOS2011$ScoreDiff > 0	#game won flag
#...stats for scoring differential on wins and losses
aggregate(abs(BOS2011$ScoreDiff), list(W=BOS2011$W), summary)


####################################
# winning close games and Pythagoras

# get game-by-game results for 2011
results <- gl2011[,c("VisitingTeam", "HomeTeam", "VisitorRunsScored", "HomeRunsScore")]
#...winning team ID
results$winner <- ifelse(results$HomeRunsScore > results$VisitorRunsScored, results$HomeTeam, results$VisitingTeam)
#...run differential
results$diff <- abs(results$VisitorRunsScored - results$HomeRunsScore)

# games decided by one run
onerungames <- subset(results, diff==1)
# data frame containing one-run wins by team
onerunwins <- as.data.frame(table(onerungames$winner))
names(onerunwins) <- c("teamID", "onerunW")
# get seasonal data for 2011 (lahman)
teams2011 <- subset(myteams, yearID == 2011)
#...recode for Angels ID inconsistency
teams2011[teams2011$teamID == "LAA", "teamID"] <- "ANA"
#...merge data coming from Lahman and Game Logs
teams2011 <- merge(teams2011, onerunwins)
#...plot one-run wins vs pythagorean residuals
plot(teams2011$onerunW, teams2011$pytResiduals,
     xlab = "one run wins",
     ylab = "Pythagorean residuals")
#...identify 2 data points
identify(teams2011$onerunW, teams2011$pytResiduals, labels=teams2011$teamID)

# Residuals for teams with top closers
pit <- read.csv("lahman/pitching.csv")
top_closers <- subset(pit, GF > 50 & ERA < 2.5)[,c("playerID", "yearID", "teamID")]
teams_top_closers <- merge(myteams, top_closers)
summary(teams_top_closers$pytResiduals)



##########################
# How many runs for a win?

# incremental runs per win (Caola's Formula -- needs simplification)
D(expression(G * R ^ 2 / (R ^ 2 + RA ^ 2)), "R")

# function for calculating incremental runs
IR <- function(RS = 5, RA = 5){
  round((RS ^ 2 + RA ^ 2)^2 / (2 * RS * RA ^ 2), 1)
}

# prepare table containing various runs scored / runs allowed scenarios
IRtable <- expand.grid(RS = seq(3, 6, .5), RA = seq(3, 6, .5))
rbind(head(IRtable), tail(IRtable))

# calculate incremental runs for the various scenarios
IRtable$IRW <- IR(IRtable$RS, IRtable$RA)
#,,,display in tabular form
xtabs(IRW ~ RS + RA, data = IRtable)