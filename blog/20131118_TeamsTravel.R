# How much do teams travel?

# http://baseballwithr.wordpress.com/2013/11/18/how-much-do-teams-travel/

# need game log Retrosheet datafile gl2012.txt 
# and Seamheads Ballpark database files (namely Parks.xls)


# resubmit last week code to get lat/lon of ballparks

options(stringsAsFactors=F)
 
#set the season
season = 2012
 
# download the 2012 Game Log file, unzip it, and place it in a folder of choice
# then modify the following line accordingly
games = read.table(paste("your/gamelogs/folder/gl", season, ".txt", sep=""), sep=",")
#download game_log_header.csv from our GitHub repository (https://github.com/maxtoki/baseball_R)
glheaders = read.csv("game_log_header.csv")
names(games) = names(glheaders)
 
#vector of unique park identifiers
parks = unique(games$ParkID)
 
#get park info from KJOK database @ SeamHeads
library(xlsx)
allparks = read.xlsx("your/path/to/KJOK/database/Parks.xlsx", 1)
 
#keep only parks used for the season
parksinfo = subset(allparks, PARKID %in% parks)
 
#get lat/lon coordinates of ballparks
library(ggmap)
locations = geocode(paste(parksinfo$CITY, parksinfo$STATE, sep=", "))
parksinfo = cbind(parksinfo, locations)


# compute distances between any two ballparks

library(geosphere)
# matrix of distances
distances = distm(locations)
rownames(distances) = parksinfo$PARKID
colnames(distances) = parksinfo$PARKID
# transform matrix to data frame
distances = as.data.frame(as.table(distances))
names(distances) = c("fromPARKID", "toPARKID", "meters")
# calculate distance in miles
distances$miles = distances$meters * 0.000621371


# function to compute teams' seasonal travel

getTravel = function(team, gamesdata, distancematrix){
  #select games (home+away) played by team
  #and keep the game number for the team and the park identifier
  homegames = subset(gamesdata, HomeTeam==team)[,c("HomeTeamGameNumber", "ParkID")]
  awaygames = subset(gamesdata, VisitingTeam==team)[,c("VisitingTeamGameNumber", "ParkID")]
  #rename the HomeTeamGameNumber and VisitingTeamGameNumber columns to make them consistent
  names(homegames)[1] = "gameNumber"
  names(awaygames)[1] = "gameNumber"
  #combine the homegames and awaygames data frames by rows
  allgames = rbind(homegames, awaygames)
  #identify the previous game "gameNumber"
  allgames$previousGame = allgames$gameNumber - 1
  #merge the allgames data frame with itself
  #matching gameNumber with previousGame
  #to compute where the previous game was played
  allgames = merge(allgames, allgames[,c("gameNumber", "ParkID")], by.x="previousGame", by.y="gameNumber", suffixes=c("", "previous"))
  #merge the allgames data frame with the dataframe containig park-to-park distances
  #to get game by game travel
  allgames = merge(allgames, distancematrix, by.x=c("ParkIDprevious", "ParkID"), by.y=c("fromPARKID", "toPARKID"))
  #compute total travel for the season
  sum(allgames$miles)
}

# compute travel for 2012 teams

seasonTravel = data.frame(teamID = unique(games$VisitingTeam))
seasonTravel$miles = sapply(seasonTravel$teamID, function(x) getTravel(x, games, distances))