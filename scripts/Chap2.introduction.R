##############################################
# Chapter 2 - Introduction to R
#
# use data files spahn.csv, NLbatting.csv,
#                NLpitching.csv, munson.csv
#                Batting.csv
#
# needed packages: plyr
###############################################

##############################################
# Section 2.3 Vectors
##############################################

W <- c(8, 21, 15, 21, 21, 22, 14)
L <- c(5, 10, 12, 14, 17, 14, 19)

Win.Pct <- 100 * W / (W + L)
Win.Pct

Year <- seq(1946, 1952)
Year <- 1946 : 1952

Age <- Year - 1921
plot(Age, Win.Pct)

mean(Win.Pct)

100 * sum(W) / (sum(W) + sum(L))

sort(W)

cumsum(W)

summary(Win.Pct)

W[c(1, 2, 5)]

W[1:4]

W[-c(1, 6)]

Win.Pct > 60

(W > 20) & (Win.Pct > 60)

Year[Win.Pct == max(Win.Pct)]

Year[W + L > 30]

#########################################
# Section 2.4 Objects and Containers in R
#########################################

NL <- c("FLA", "STL", "HOU", "STL", "COL",
        "PHI", "PHI", "SFG", "STL", "SFG")

AL <- c("NYY", "BOS", "CHW", "DET", "BOS",
        "TBR", "NYY", "TEX", "TEX", "DET")

Winner <- c("NL", "AL", "AL", "NL", "NL",
            "NL", "AL", "NL", "NL", "NL")

N.Games <- c(6, 4, 4, 5, 4, 5, 6, 5, 7, 4)
Year <- 2003 : 2012

results <- matrix(c(NL, AL), 10, 2)
results

dimnames(results)[[1]] <- Year
dimnames(results)[[2]] <- c("NL Team", "AL Team")
results

table(Winner)

barplot(table(Winner))

table(NL)

NL2 <- factor(NL, levels=c("FLA", "PHI", "HOU", "STL", "COL", "SFG"))

str(NL2)

table(NL2)

World.Series <- list(Winner=Winner, Number.Games=N.Games,
                     Seasons="2003 to 2012")

World.Series$Number.Games

World.Series[[2]]

World.Series["Number.Games"]

# Section 2.5 Collection of R Commands

################################################
# illustration of a R script
# put following code in a file "world.series.R
# run by typing
# source("World.Series.R", echo=TRUE)

N.Games <- c(6, 4, 4, 5, 4, 5, 6, 5, 7, 4)
Winner <- c("NL", "AL", "AL", "NL", "NL",
            "NL", "AL", "NL", "NL", "NL")
table(Winner)
barplot(table(Winner))
by(N.Games, Winner, summary)
################################################

hr.rates <- function(age, hr, ab){
  rates <- round(100 * hr / ab, 1)
  list(x=age, y=rates)
}

HR <- c(13, 23, 21, 27, 37, 52, 34, 42, 31, 40, 54)
AB <- c(341, 549, 461, 543, 517, 533, 474, 519, 541, 527, 514)
Age <- 19 : 29
hr.rates(Age, HR, AB)

plot(hr.rates(Age, HR, AB))

# Section 2.6 Reading and Writing Data in R

getwd()

spahn <- read.csv("spahn.csv")

HR <- c(13, 23, 21, 27, 37, 52, 34, 42, 31, 40, 54)
AB <- c(341, 549, 461, 543, 517, 533, 474, 519, 541, 527, 514)
Age <- 19 : 29
HR.Rates <- hr.rates(Age, HR, AB)
Mantle <- cbind(Age, HR, AB, Rates=HR.Rates$y)

write.csv(Mantle, "..\output\mantle.csv", row.names=FALSE)

# Section 2.7 Data Frames

spahn[1:3, 1:10]

spahn[1, ]

spahn[1:10, c("Age", "W", "L", "ERA")]

summary(spahn$ERA)

spahn$Age[spahn$ERA == min(spahn$ERA)]

#####################################

spahn$FIP <- with(spahn, (13 * HR + 3 * BB - 2 * SO) / IP)

pos <- order(spahn$FIP)
head(spahn[pos, c("Year", "Age", "W", "L", "ERA", "FIP")])

spahn1 <- subset(spahn, Tm == "BSN" | Tm == "MLN")

spahn1$Tm <- factor(spahn1$Tm, levels=c("BSN", "MLN"))

by(spahn1[, c("W.L", "ERA", "WHIP", "FIP")], spahn1$Tm, summary)

# this command assumes that you already have two data frames
# NLbatting and ALbatting into R

NLbatting <- read.csv("NLbatting.csv")
ALbatting <- read.csv("ALbatting.csv")
batting <- rbind(NLbatting, ALbatting)

NLpitching <- read.csv("NLpitching.csv")
NL <- merge(NLbatting, NLpitching, by="Tm")

NL.150 <- subset(NLbatting, HR > 150)

# Section 2.8 Packages

install.packages("Lahman")
library(Lahman)
?Batting

# Section 2.9 Splitting, applying, and combining data

Batting <- read.csv("Batting.csv")

Batting.60 <- subset(Batting, yearID >= 1960 & yearID <= 1969)

compute.hr <- function(pid){
  d <- subset(Batting.60, playerID == pid)
  sum(d$HR)
}

players <- unique(Batting.60$playerID)
S <- sapply(players, compute.hr)

R <- data.frame(Player=players, HR=S)
R <- R[order(R$HR, decreasing=TRUE), ]
head(R)

library(plyr)
dataframe.AB <- ddply(Batting, .(playerID), summarize,
                      Career.AB=sum(AB, na.rm=TRUE))

Batting <- merge(Batting, dataframe.AB, by="playerID")

Batting.5000 <- subset(Batting, Career.AB >= 5000)

ab.hr.so <- function(d){
  c.AB <- sum(d$AB, na.rm=TRUE)
  c.HR <- sum(d$HR, na.rm=TRUE)
  c.SO <- sum(d$SO, na.rm=TRUE)
  data.frame(AB=c.AB, HR=c.HR, SO=c.SO)
}

aaron <- subset(Batting.5000, playerID == "aaronha01")
ab.hr.so(aaron)

d.5000 <- ddply(Batting.5000, .(playerID), ab.hr.so)

head(d.5000)

with(d.5000, plot(HR/AB, SO/AB))
with(d.5000, lines(lowess(HR/AB, SO/AB)))

# Section 2.10 Getting Help

?dotchart

??dotchart

##################################################################