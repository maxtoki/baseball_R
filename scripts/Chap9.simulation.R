##############################################
# Chapter 9 Simulation 
#
# use data files all2001.csv, fields.csv
# use R packages car, plyr, LearnBayes
# use special function one.simulation.68
###############################################

###############################################
# 9.2 Simulating a Half-Inning
###############################################

# 9.2.1 Review of work in runs expectancy

data2011 <- read.csv("all2011.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data2011) <- fields[, "Header"]

data2011$HALF.INNING <- with(data2011, 
                             paste(GAME_ID, INN_CT, BAT_HOME_ID))
data2011$RUNS.SCORED <- with(data2011, (BAT_DEST_ID > 3) +
                               (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)                      
}

RUNNER1 <- ifelse(as.character(data2011[,"BASE1_RUN_ID"])=="", 0, 1)
RUNNER2 <- ifelse(as.character(data2011[,"BASE2_RUN_ID"])=="", 0, 1)
RUNNER3 <- ifelse(as.character(data2011[,"BASE3_RUN_ID"])=="", 0, 1)
data2011$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2011$OUTS_CT)

NRUNNER1 <- with(data2011, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
NRUNNER2 <- with(data2011, as.numeric(RUN1_DEST_ID==2 | 
                                        RUN2_DEST_ID==2 | BAT_DEST_ID==2))
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

data2011C <- subset(data2011, BAT_EVENT_FL == TRUE)

library(car)
data2011C$NEW.STATE <- recode(data2011C$NEW.STATE,
                              "c('000 3', '100 3', '010 3', '001 3',
                 '110 3', '101 3', '011 3', '111 3') = '3'")

# 9.2.3 Computing the transition probabilities

T.matrix <- with(data2011C, table(STATE, NEW.STATE))

P.matrix <- prop.table(T.matrix, 1)

P.matrix <- rbind(P.matrix, c(rep(0, 24), 1))

margin.table(P.matrix, 1)

P1 <- round(P.matrix["000 0", ], 3)
data.frame(Prob = P1[P1 > 0])

P2 <- round(P.matrix["010 2", ], 3)
data.frame(Prob = P2[P2 > 0])

# 9.2.4 Simulating the Markov chain

count.runners.outs <- function(s)
  sum(as.numeric(strsplit(s,"")[[1]]), na.rm = TRUE)
runners.outs <- sapply(dimnames(T.matrix)[[1]], count.runners.outs)[-25]
R <- outer(runners.outs + 1, runners.outs, FUN="-")
dimnames(R)[[1]] <- dimnames(T.matrix)[[1]][-25]
dimnames(R)[[2]] <- dimnames(T.matrix)[[1]][-25]
R <- cbind(R, rep(0, 24))

# simulate Markov Chain -- replicate runs expectancy table

simulate.half.inning <- function(P, R, start=1){
  s <- start; path <- NULL; runs <- 0
  while(s < 25){
    s.new <- sample(1:25, 1, prob = P[s, ])
    path <- c(path, s.new)
    runs <- runs + R[s, s.new]
    s <- s.new
  }
  runs
}

simulate.half.inning(P.matrix,R,1)

RUNS <- replicate(10000, simulate.half.inning(T.matrix, R))
mean(RUNS)

RUNS.j = function(j){
  mean(replicate(10000, simulate.half.inning(T.matrix, R, j)))
}

RUNS.j <- function(j){
  mean(replicate(10000, simulate.half.inning(T.matrix, R, j)))
}
Runs.Expectancy <- sapply(1:24, RUNS.j)
Runs.Expectancy <- t(round(matrix(Runs.Expectancy, 3, 8), 2))
dimnames(Runs.Expectancy)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(Runs.Expectancy)[[1]] <- c("000", "001", "010", "011", "100", "101", 
                                    "110", "111")
Runs.Expectancy

Runs <- matrix(
  c(0.47, 0.25, 0.10, 1.45, 0.94, 0.32,   
    1.06, 0.65, 0.31, 1.93, 1.34, 0.54,   
    0.84, 0.50, 0.22, 1.75, 1.15, 0.49,   
    1.41, 0.87, 0.42, 2.17, 1.47, 0.76), 
  8, 3, byrow=TRUE)

Runs - Runs.Expectancy

# 9.2.5 Beyond runs expectancy

P.matrix.3 <- P.matrix %*% P.matrix %*% P.matrix
sorted.P <- sort(round(P.matrix.3["000 0", ], 3), decreasing=TRUE)
head(data.frame(Prob = sorted.P))

# how many batters?

Q <- P.matrix[-25, -25]
N <- solve(diag(rep(1, 24)) - Q)

N.0000 <- round(N["000 0", ], 2)
head(data.frame(N = N.0000))

sum(N.0000)

Length = round(t(N %*% rep(1, 24)), 2)
data.frame(L = Length[1, 1:8])

# 9.2.6 Transition probabilities for individual teams

data2011C$HOME_TEAM_ID <- with(data2011C, substr(GAME_ID, 1, 3))
data2011C$BATTING.TEAM <- with(data2011C, 
                               ifelse(BAT_HOME_ID==0, 
                                      as.character(AWAY_TEAM_ID), 
                                      as.character(HOME_TEAM_ID)))

Team.T <- with(data2011C, table(BATTING.TEAM, STATE, NEW.STATE))

d.state <- subset(data2011C, STATE == '100 2')
Team.T.S <- with(d.state, table(BATTING.TEAM, NEW.STATE))

WAS.Trans <- Team.T.S["WAS", ]
WAS.n <- sum(WAS.Trans)
P.WAS <- WAS.Trans / WAS.n

ALL.Trans <- with(subset(data2011C, STATE=='100 2'), 
                 table(NEW.STATE))
P.ALL <- ALL.Trans / sum(ALL.Trans)
K <- 1274

P.EST <- WAS.n / (K + WAS.n) * P.WAS + K / (K + WAS.n) * P.ALL

data.frame(WAS = round(P.WAS, 4), 
           ALL = round(c(P.ALL), 4), 
           EST = round(c(P.EST), 4))

## simulating the Bradley-Terry model
# many simulations of 1968 season

##########################################
# 9.3 Simulating a Baseball Season
##########################################

# 9.3.6 Function to simulate one season

source("../scripts/one.simulation.68.R")

RESULTS <- one.simulation.68(0.20)
RESULTS

display.standings <- function(RESULTS, league){
  Standings <- subset(RESULTS, League == league)[, c("Team", "Wins")]
  Standings$Losses <- 162 - Standings$Wins
  Standings[order(Standings$Wins, decreasing=TRUE), ]
}

cbind(display.standings(RESULTS, 1), display.standings(RESULTS, 2))

with(RESULTS, as.character(Team[Winner.Lg == 1]))

with(RESULTS, as.character(Team[Winner.WS == 1]))

# 9.3.7 Simulating many seasons

Many.Results <- NULL
for(j in 1:1000)
  Many.Results <- rbind(Many.Results, one.simulation.68(0.20))

with(Many.Results, smoothScatter(Talent, Wins))

Results.avg <- subset(Many.Results, Talent > -0.05 & Talent < 0.05)
hist(Results.avg$Wins)

fit1 <- glm(Winner.WS ~ Talent, data = Many.Results, family=binomial)
fit2 <- glm(Winner.Lg ~ Talent, data = Many.Results, family=binomial)

# removed from the book?
lp <- predict(fit1, data.frame(Talent = seq(-0.4, 0.4, 0.1)))
exp(lp) / (1 + exp(lp))
# removed from the book?
lp <- predict(fit2, data.frame(Talent = seq(-0.4, 0.4, 0.1)))
exp(lp) / (1 + exp(lp))

b1 <- coef(fit1)
curve(exp(b1[1] + b1[2] * x) / (1 + exp(b1[1] + b1[2] * x)),
      -0.4, 0.4, xlab = "Talent", ylab = "Probability", lwd=2,
      ylim = c(0, 1))
b2 <- coef(fit2)
curve(exp(b2[1] + b2[2] * x) / (1 + exp(b2[1] + b2[2] * x)),
      add = TRUE, lwd=2, lty=2)
legend(-0.2, 0.8, legend=c("Win Pennant", "Win World Series"),
       lwd=2, lty=c(1, 2))

#############################################################

P <- matrix(c(.3, .7,  0,  0, 
               0, .3, .7,  0,
               0,  0, .3, .7, 
               0,  0,  0,  1), 4, 4, byrow=TRUE)

simulate.half.inning <- function(P, start=1){
  s <- start; path <- NULL; 
  while(s < 4){
    s.new <- sample(1:4, 1, prob = P[s, ])
    path <- c(path, s.new)
    s <- s.new
  }
  length(path)
}

solve(diag(c(1,1,1))- P[-4,-4])