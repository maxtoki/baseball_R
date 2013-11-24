####################################################
# Chapter 8 - Career Trajectories
#
# uses .csv files from the Lahman's database
#         (placed in the "lahman" subfolder)
# uses R packages car, plyr, ggplot2
####################################################

# Section 8.2 Mickey Mantle's Career Trajectory

Batting <- read.csv("lahman/Batting.csv")
Master <- read.csv("lahman/Master.csv")

mantle.info <- subset(Master, nameFirst=="Mickey" & nameLast=="Mantle")
mantle.id <- as.character(mantle.info$playerID)

library(car)
Batting$SF <- recode(Batting$SF, "NA = 0")
Batting$HBP <- recode(Batting$HBP, "NA = 0")

get.birthyear <- function(player.id){
  playerline <- subset(Master, playerID == player.id)
  birthyear <- playerline$birthYear
  birthmonth <- playerline$birthMonth
  ifelse(birthmonth >= 7, birthyear, birthyear + 1)
}

get.birthyear(mantle.id)

get.stats <- function(player.id){
  d <- subset(Batting, playerID==player.id)
  byear <- get.birthyear(player.id)
  d$Age <- d$yearID - byear
  d$SLG <- with(d, (H - X2B - X3B - HR +
                    2 * X2B + 3 * X3B + 4 * HR) / AB)
  d$OBP <- with(d, (H + BB) / (H + AB + BB + SF))
  d$OPS <- with(d, SLG + OBP)
  d
}

Mantle <- get.stats(mantle.id)

with(Mantle, plot(Age, OPS, cex=1.5, pch=19))

fit.model <- function(d){
  fit <- lm(OPS ~ I(Age - 30) + I((Age - 30)^2), data = d)
  b <- coef(fit)
  Age.max <- 30 - b[2] / b[3] / 2
  Max <- b[1] - b[2] ^ 2 / b[3] / 4
  list(fit = fit, 
       Age.max = Age.max, Max = Max)
}

F2 <- fit.model(Mantle)
coef(F2$fit)
c(F2$Age.max, F2$Max)

lines(Mantle$Age, predict(F2$fit, Age=Mantle$Age), lwd=3)
abline(v = F2$Age.max, lwd=3, lty=2, col="grey")
abline(h = F2$Max, lwd=3, lty=2, col="grey")
text(29, .72, "Peak.age" , cex=2)
text(20, 1, "Max", cex=2)

summary(F2$fit)

# Section 8.3 - Comparing Trajectories

Fielding <- read.csv("lahman/Fielding.csv")

library(plyr)
AB.totals <- ddply(Batting, .(playerID), 
                  summarize, 
                  Career.AB = sum(AB, na.rm = TRUE))
Batting <- merge(Batting, AB.totals)
Batting.2000 <- subset(Batting, Career.AB >=2000)

find.position <- function(p){
  positions <- c("OF", "1B", "2B", "SS", "3B", "C", "P", "DH")
  d <- subset(Fielding, playerID == p)
  count.games <- function(po)
    sum(subset(d, POS == po)$G)
  FLD <- sapply(positions, count.games)
  positions[FLD == max(FLD)][1]
}

PLAYER <- as.character(unique(Batting.2000$playerID))
POSITIONS <- sapply(PLAYER, find.position) #takes long time to complete!
Fielding.2000 <- data.frame(playerID = names(POSITIONS),
                           POS = POSITIONS)
Batting.2000 <- merge(Batting.2000, Fielding.2000)

# comparing career statistics

library(plyr)
C.totals <- ddply(Batting, .(playerID), 
                  summarize, 
                  C.G = sum(G, na.rm = TRUE),
                  C.AB = sum(AB, na.rm = TRUE),
                  C.R = sum(R, na.rm = TRUE),
                  C.H = sum(H, na.rm = TRUE),
                  C.2B = sum(X2B, na.rm = TRUE),
                  C.3B = sum(X3B, na.rm = TRUE),
                  C.HR = sum(HR, na.rm = TRUE),
                  C.RBI = sum(RBI, na.rm = TRUE),
                  C.BB = sum(BB, na.rm = TRUE),
                  C.SO = sum(SO, na.rm = TRUE),
                  C.SB = sum(SB, na.rm = TRUE))

C.totals$C.AVG <- with(C.totals, C.H / C.AB)
C.totals$C.SLG <- with(C.totals, 
          (C.H - C.2B - C.3B - C.HR + 2 * C.2B +
             3 * C.3B + 4 * C.HR) / C.AB)

C.totals <- merge(C.totals, Fielding.2000)
C.totals$Value.POS <- with(C.totals, 
                          ifelse(POS=="C", 240,
                          ifelse(POS=="SS", 168,
                          ifelse(POS=="2B", 132,
                          ifelse(POS=="3B", 84,
                          ifelse(POS=="OF", 48,
                          ifelse(POS=="1B", 12, 0)))))))
                          

# computing similarity scores

similar <- function(p, number=10){
   P <- subset(C.totals, playerID == p)
   C.totals$SS <- with(C.totals,
              1000 -
              floor(abs(C.G - P$C.G) / 20) -
              floor(abs(C.AB - P$C.AB) / 75) -
              floor(abs(C.R - P$C.R) / 10) -
              floor(abs(C.H - P$C.H) / 15) -
              floor(abs(C.2B - P$C.2B) / 5) -
              floor(abs(C.3B - P$C.3B) / 4) -
              floor(abs(C.HR - P$C.HR) / 2) -
              floor(abs(C.RBI - P$C.RBI) / 10) -
              floor(abs(C.BB - P$C.BB) / 25) -
              floor(abs(C.SO - P$C.SO) / 150) -
              floor(abs(C.SB - P$C.SB) / 20) - 
              floor(abs(C.AVG - P$C.AVG) / 0.001) - 
              floor(abs(C.SLG - P$C.SLG) / 0.002) -
              abs(Value.POS - P$Value.POS))
  C.totals <- C.totals[order(C.totals$SS, decreasing = TRUE), ]  
  C.totals[1:number, ]
}

similar(mantle.id, 6)

# defining age, OBP, SLG, and OPS variables

collapse.stint <- function(d){
  G <- sum(d$G); AB <- sum(d$AB); R <- sum(d$R)
  H <- sum(d$H); X2B <- sum(d$X2B); X3B <- sum(d$X3B)
  HR <- sum(d$HR); RBI <- sum(d$RBI); SB <- sum(d$SB)
  CS <- sum(d$CS); BB <- sum(d$BB); SH <- sum(d$SH)
  SF <- sum(d$SF); HBP <- sum(d$HBP)
  SLG <- (H - X2B - X3B - HR + 2 * X2B +
            3 * X3B + 4 * HR) / AB
  OBP <- (H + BB + HBP) / (AB + BB + HBP + SF)
  OPS <- SLG + OBP
  data.frame(G = G, AB = AB, R = R, H = H, X2B = X2B,
             X3B = X3B, HR = HR, RBI = RBI, SB = SB,
             CS = CS, BB = BB, HBP = HBP, SH = SH, SF = SF, 
             SLG = SLG, OBP = OBP, OPS = OPS,
             Career.AB = d$Career.AB[1], POS = d$POS[1])
}

Batting.2000 <- ddply(Batting.2000, 
                     .(playerID, yearID), collapse.stint) #requires a long time to complete

player.list <- as.character(unique(Batting.2000$playerID))
birthyears <- sapply(player.list, get.birthyear) #requires a long time to complete
Batting.2000 <- merge(Batting.2000,
                      data.frame(playerID=player.list,
                                 Birthyear=birthyears))
Batting.2000$Age <- with(Batting.2000, yearID - Birthyear)

Batting.2000 <- Batting.2000[complete.cases(Batting.2000$Age), ]

# fitting and plotting trajectories

fit.traj <- function(d){
  fit <- lm(OPS ~ I(Age - 30) + I((Age - 30)^2), data = d)
  data.frame(Age = d$Age, Fit = predict(fit, Age = d$Age))
}

plot.traj2 <- function(first, last, n.similar = 5, ncol){
  require(plyr)
  require(ggplot2)
  get.name <- function(playerid){
    d1 <- subset(Master, playerID == playerid)
    with(d1, paste(nameFirst, nameLast))
  }
  player.id <- subset(Master, 
                nameFirst == first & nameLast == last)$playerID
  player.id <- as.character(player.id)
  player.list <- as.character(similar(player.id, n.similar)$playerID)
  Batting.new <- subset(Batting.2000, playerID %in% player.list)
  
  F2 <- ddply(Batting.new, .(playerID), fit.traj)
  F2 <- merge(F2, 
            data.frame(playerID = player.list,
            Name = sapply(as.character(player.list), get.name)))

  print(ggplot(F2, aes(Age, Fit)) + geom_line(size=1.5) +
    facet_wrap(~ Name, ncol=ncol) + theme_bw())
  return(Batting.new)
}

d <- plot.traj2("Mickey", "Mantle", 6, 2)

d <- plot.traj2("Derek", "Jeter", 9, 3)

fit.traj2 <- function(d){
  f <- lm(OPS ~ I(Age - 30) + I((Age - 30)^2), data = d)
  b <- coef(f)
  Age.max <- round(30 - b[2] / b[3] / 2, 1)
  Max <- round(b[1] - b[2] ^ 2 / b[3] / 4, 3)
  data.frame(Age.max = Age.max, Max = Max, 
             Curve = round(b[3], 5))
}

d <- plot.traj2("Derek", "Jeter", 9, 3)
S <- ddply(d, .(playerID), fit.traj2)
S

with(S, plot(Age.max, Curve, pch=19, cex=1.5,
             xlab="Peak Age", ylab="Curvature",
             xlim=c(27, 36), ylim=c(-0.0035, 0)))
S$lastNames <- as.character(subset(Master, 
                    playerID %in% S$playerID)$nameLast)
with(S, text(Age.max, Curve, lastNames, pos=3))

# Section 8.4 General Patterns of Peak Age

library(plyr)
midcareers <- ddply(Batting.2000, .(playerID), 
                  summarize, 
                  Midyear = (min(yearID) + max(yearID))/2)
Batting.2000 <- merge(Batting.2000, midcareers)

fit.traj3 <- function(d){
  b <- coef(lm(OPS ~ I(Age - 30) + I((Age - 30)^2), data = d))
  data.frame(A = b[1], B = b[2], C = b[3],
             Midyear = d$Midyear[1], Career.AB = d$Career.AB[1])
}
Beta.coef <- ddply(Batting.2000, .(playerID), fit.traj3)

Beta.coef$Peak.age <- with(Beta.coef, 30 - B / 2 / C)

with(Beta.coef,
     plot(Midyear, Peak.age, ylim=c(20, 40),
          xlab = "Mid Career", ylab = "Peak Age"))
i <- is.finite(Beta.coef$Peak.age)
with(Beta.coef,
     lines(lowess(Midyear[i], Peak.age[i]), lwd=3))

with(Beta.coef, 
     plot(log2(Career.AB[i]), Peak.age[i], ylim=c(20, 40),
          xlab = "log2 Career AB", ylab = "Peak Age"))
with(Beta.coef,
     lines(lowess(log2(Career.AB[i]), Peak.age[i]), lwd=3))

# Section 8.5 Trajectories and Fielding Position

Batting.2000a <- subset(Batting.2000, Midyear >= 1985 & Midyear <= 1995)

fit.traj4 <- function(d){
  b <- coef(lm(OPS ~ I(Age - 30) + I((Age - 30)^2), data = d))
  data.frame(A = b[1], B = b[2], C = b[3],
             Peak.Age = 30 - b[2] / 2 / b[3],
             Midyear = d$Midyear[1], Career.AB = d$Career.AB[1],
             Position = d$POS[1])
}

Beta.estimates <- ddply(Batting.2000a, .(playerID), fit.traj4)

Beta.estimates1 <- subset(Beta.estimates, Position %in% 
                 c("1B", "2B", "3B", "SS", "C", "OF"))

Beta.estimates1$Position <- Beta.estimates1$Position[ , drop=TRUE] 

Beta.estimates1 <- merge(Beta.estimates1, Master)

stripchart(Peak.Age ~ Position, data=Beta.estimates1,
           xlim=c(20, 40), method="jitter", pch=1)

special <- with(Beta.estimates1, identify(Peak.Age, Position, 
                     n=5, labels=nameLast))

dnew <- subset(Batting.2000, 
              playerID %in% Beta.estimates1$playerID[special])
dnew <- merge(dnew, Master)

ggplot(dnew, aes(Age, OPS)) + geom_point(size=4) + 
  facet_wrap(~ nameLast, ncol=2) + ylim(0.4, 1.05) +
  stat_smooth(method="lm", se=FALSE, size=1.5,
              formula=y ~ poly(x, 2, raw=TRUE))  + theme_bw()

################################################################