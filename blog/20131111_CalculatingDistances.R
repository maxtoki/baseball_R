# Calculating distances in R

# http://baseballwithr.wordpress.com/2013/11/11/calculating-distaces-in-r/

# need game log Retrosheet datafile gl2012.txt 
# and Seamheads Ballpark database files (namely Parks.xls)


# geocode Jim's and Max's homeplaces

library(ggmap)
JA = geocode("Findlay, Ohio")
MM = geocode("Sasso Marconi, Bologna")

# calculate distance

library(geosphere)
distHaversine(JA, MM) * 0.000621371

# read game log data

options(stringsAsFactors=F)
season = 2012
# download the 2012 Game Log file, unzip it, and place it in a folder of choice
# then modify the following line accordingly
games = read.table(paste("your/gamelogs/folder/gl", season, ".txt", sep=""), sep=",")
#download game_log_header.csv from our GitHub repository
glheaders = read.csv("game_log_header.csv")
names(games) = names(glheaders)

# get park ids used for the season

parks = unique(games$ParkID)

# read ballparks data

#get park info from KJOK database @ SeamHeads
library(xlsx)
allparks = read.xlsx("your/path/to/KJOK/database/Parks.xlsx", 1)
#keep only parks used for the season
parksinfo = subset(allparks, PARKID %in% parks)

# geocode ballparks

locations = geocode(paste(parksinfo$CITY, parksinfo$STATE, sep=", "))
parksinfo = cbind(parksinfo, locations)

# draw ballparks on a map

map = get_map("united states", zoom=4, maptype="roadmap")
ggmap(map) +
  geom_point(data=parksinfo, aes(x=lon, y=lat), size=3) +
  geom_text(data=parksinfo, aes(x=lon, y=lat, label=NAME), size=4, col="blue", vjust=-.5)
