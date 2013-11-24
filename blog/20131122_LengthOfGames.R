# Length of Baseball Games

# http://baseballwithr.wordpress.com/2013/11/22/length-of-baseball-games/

# need play-by-play Retrosheet datafile all2012.csv
# and game log Retrosheet datafile gl2012.txt in a "data" folder
# in the current working directory

# read in play-by-play data

season <- 2012
file.name <- paste("data/all", season, ".csv", sep="")
plays <- read.csv(file.name, header=FALSE)
fields <- read.csv("data/fields.csv")
names(plays) <- fields[, "Header"]

# create variables pseq and n.pitches

plays$pseq <- gsub("[.>123N+*]", "", plays$PITCH_SEQ_TX)
plays$n.pitches <- nchar(plays$pseq)

# read in game logs data file 

file.name <- paste("data/gl", season, ".txt", sep="")
games <- read.csv(file.name, header=FALSE)
headers <- read.csv("data/game_log_header.csv")
names(games) <- names(headers)

# add GAME_ID variable to game logs, computes number of pitches
# for each plate appearance

games$GAME_ID <- with(games, paste(HomeTeam, Date, "0", sep=""))
library(plyr)
game.pitches <- ddply(plays, .(GAME_ID), summarize,
                      Pitches = sum(n.pitches))

# merge game time and number of pitches information

DATA <- merge(game.pitches, 
              games[, c("GAME_ID", "Duration")], 
              by="GAME_ID")

# plots number of pitches and game times; overlays
# a least-squares fit

head(DATA)
with(DATA, smoothScatter(Pitches, Duration))
fit <- lm(Duration ~ Pitches, data=DATA)
abline(fit, lwd=3, col="red")
fit

