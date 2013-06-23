############################################
# Chapter 10 - Exploring Streaky Behavior
#
# uses data files dimmagio.1941.csv, all2011.csv
#                 fields.csv, roster2011.csv
#
# use R packages MASS, ggplot2, TTR
#
############################################

# Section 10.2 The Great Streak

joe <- read.csv("dimaggio.1941.csv")

sum(joe$H) / sum(joe$AB)

# TO DO:  remove games with no official at-bats

joe$HIT <- ifelse(joe$H > 0, 1, 0)

joe$HIT

streaks <- function(y){
  n <- length(y)
  where <- c(0, y, 0) == 0
  location.zeros <- (0:(n+1))[where]
  streak.lengths <- diff(location.zeros) - 1
  streak.lengths[streak.lengths > 0]
}

streaks(joe$HIT)

joe$NO.HIT <- 1 - joe$HIT
streaks(joe$NO.HIT)

moving.average <- function(H, AB, width){
  require(TTR)
  game <- 1:length(H)
  P <- data.frame(Game = SMA(game, width), 
          Average = SMA(H, width) / SMA(AB, width))
  P[complete.cases(P), ]
}

plot(moving.average(joe$H, joe$AB, 10), type="l")
abline(h=sum(joe$H) / sum(joe$AB))
game.hits <- (1:nrow(joe))[as.logical(joe$HIT)]
rug(game.hits)

# Section 10.3 - Streaks in Individual At-bats

data2011 <- read.csv("all2011.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data2011) <- fields[, "Header"]

ichiro.AB <- subset(data2011, BAT_ID == "suzui001" & AB_FL==TRUE)

ichiro.AB$HIT <- ifelse(ichiro.AB$H_FL > 0, 1, 0)
ichiro.AB$DATE <- substr(ichiro.AB$GAME_ID, 4, 12)
ichiro.AB <- ichiro.AB[order(ichiro.AB$DATE), ]

streaks(ichiro.AB$HIT)

streaks(1 - ichiro.AB$HIT)

table(streaks(1 - ichiro.AB$HIT))

ichiro.AB$AB <- 1
plot(moving.average(ichiro.AB$HIT, ichiro.AB$AB, 30), type="l", xlab="AB")
abline(h = mean(ichiro.AB$HIT))
rug((1:nrow(ichiro))[ichiro.AB$HIT == TRUE])

longest.ofer <- function(batter, d){
  d.AB <- subset(d, BAT_ID == batter & AB_FL==TRUE)
  d.AB$HIT <- ifelse(d.AB$H_FL > 0, 1, 0)
  d.AB$DATE <- substr(d.AB$GAME_ID, 4, 12)
  d.AB <- d.AB[order(d.AB$DATE), ]
  max(streaks(1 - d.AB$HIT))
}

longest.ofer("suzui001", data2011)

A <- aggregate(data2011$AB_FL, list(Player = data2011$BAT_ID), sum)
players.400 <- A$Player[A$x >= 400]
S <- sapply(players.400, longest.ofer, data2011)
S <- data.frame(Player=players.400, Streak=S)
rownames(S) <- NULL

roster2011 <- read.csv("roster2011.csv")
S1 <- merge(S, roster2011, by.x="Player", by.y="Player.ID")

S.ordered <- S1[order(S1$Streak, decreasing=TRUE), ]
head(S.ordered)

ichiro.AB <- subset(data2011, BAT_ID == "suzui001" & AB_FL==TRUE)
ichiro.AB$HIT <- ifelse(ichiro.AB$H_FL > 0, 1, 0)
ichiro.AB$DATE <- substr(ichiro.AB$GAME_ID, 4, 12)
ichiro.AB <- ichiro.AB[order(ichiro.AB$DATE), ]

source("../scripts/streaks.R")
st <- streaks(1 - ichiro.AB$HIT)
sum(st ^ 2)

random.mix <- function(y){
  # source("streaks.R")
  clump.stat <- function(sp) sum(sp^2)
  mixed <- sample(y)
  clump.stat(streaks(1 - mixed))
}

ST <- replicate(1000, random.mix(ichiro.AB$HIT))

library(MASS)
truehist(ST)
abline(v = 3047, lwd=3)
text(3250, 0.0016, "Suzuki", cex=1.5)

clump.test <- function(playerid, data){
  player.AB <- subset(data, BAT_ID == playerid & AB_FL==TRUE)
  player.AB$HIT <- ifelse(player.AB$H_FL > 0, 1, 0)
  player.AB$DATE <- substr(player.AB$GAME_ID, 4, 12)
  player.AB <- player.AB[order(player.AB$DATE), ]
  ST <- replicate(1000, random.mix(player.AB$HIT))
  truehist(ST, xlab="Clumpiness Statistic")
  stat <- sum((streaks(1 - player.AB$HIT))^2)
  abline(v = stat, lwd=3)
  text(stat * 1.05, 0.0010, "OBSERVED", cex=1.2)
}

clump.test("ibanr001", data2011)

# Section 10.4 Local Patterns of Weighted On-Base Average

sum.woba <- function(event){
  s <- 0.72 * (event==14) + 0.75 * (event==16) +
  0.90 * (event==20) + 0.92 * (event==18) +
  1.24 * (event==21) + 1.56 * (event==22) +
  1.95 * (event==23)
  c(sum(s), length(s))
}

data2011a <- subset(data2011, BAT_EVENT_FL==TRUE)
S <- with(data2011a,
  aggregate(EVENT_CD, list(Player=BAT_ID, Game=GAME_ID), sum.woba))
head(S)

regroup <- function(d, g){
  n <- dim(d)[1]
  n.g <- floor(n / g); n.r <- n - n.g * g
  gp <- rep(1:n.g, each=g, length.out=n)
  if(n.r>0) {
    i <- (n.g * g + 1):n
    gp[i] <- rep(n.g, length(i))
  }
  aggregate(d, list(gp), sum)[,-1]
}

suzuki <- subset(S, Player=="suzui001")
suzuki$Date <- with(suzuki, substr(Game, 4, 12))
suzuki <- suzuki[order(suzuki$Date), ]
head(suzuki)

regroup(suzuki$x, 5)

get.streak.data <- function(playerid, S, g=5){
  S.player <- subset(S, Player == playerid)
  S.player$Date <- with(S.player, substr(Game, 4, 12))
  S.player <- S.player[order(S.player$Date), ]
  S.player.gp <- regroup(S.player$x, g)
  s.woba.avg <- with(S.player.gp, V1 / V2)
  c(N = sum(S.player.gp$V2),
             Mean = mean(s.woba.avg), SD = sd(s.woba.avg))
}

get.streak.data("suzui001", S, 5)

player.list <- unique(S$Player)
Results <- data.frame(Player = player.list,
                t(sapply(unique(S$Player), 
                get.streak.data, S)))

Results.500 <- subset(Results, N >=500)

Master <- read.csv("lahman/Master.csv")
with(Results.500, plot(Mean, SD))
ids <- with(Results.500, identify(Mean, SD, n=2, Player, plot=FALSE))
  #...identify data points by mouse clicking on the plot
  #,,,then press ESC

pids <- as.character(Results.500[ids, "Player"])
with(Results.500, text(Mean[ids], SD[ids], 
      subset(Master, retroID %in% pids)$nameLast, pos=2))


get.streak.data2 <- function(playerid, S, g=5){
  S.player <- subset(S, Player == playerid)
  S.player$Date <- with(S.player, substr(Game, 4, 12))
  S.player <- S.player[order(S.player$Date), ]
  S.player.gp <- regroup(S.player$x, g)
  s.woba.avg <- with(S.player.gp, V1 / V2)
  data.frame(Period = 1:length(s.woba.avg), wOBA = s.woba.avg)
}

d1 <- get.streak.data2("suzuk001", S)
d2 <- get.streak.data2("uptoj001", S)
d <- rbind(data.frame(Player = "Suzuki", d1),
           data.frame(Player = "Upton", d2))
library(ggplot2)
ggplot(d, aes(Period, wOBA)) +
  geom_line(size=1) + facet_grid(Player ~ .)

###############################################################
