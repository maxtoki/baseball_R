#####################################
# MLBAM/PITCHf/x appendix           #
#                                   #
# required packages:                #
# XML                               #
# plyr                              #
#                                   #
# files used:                       #
#####################################

# load XML package
library(XML)
# pitch by pitch URL for one game
gameUrl <- "http://gd2.mlb.com/components/game/mlb/year_2012/month_06/day_13/gid_2012_06_13_houmlb_sfnmlb_1/inning/inning_all.xml"
# parse the XML document
xmlGame <- xmlParse(gameUrl)
# parse the inning nodes
xmlInnings <- getNodeSet(xmlGame, "//inning")
#...how many innings in this game?
length(xmlInnings)

# attributes of the first inning
xmlAttrs(xmlInnings[[1]])
# load plyr package
library(plyr)
# attribures of every inning
ldply(xmlInnings, xmlAttrs)

# wrapper function for grabbing XML documents
grabXML <- function(XML.parsed, field){
  parse.field <- getNodeSet(XML.parsed, paste("//", field, sep=""))
  results <- t(sapply(parse.field, function(x) xmlAttrs(x)))
  if(typeof(results)=="list"){
    do.call(rbind.fill, lapply(lapply(results, t), data.frame, stringsAsFactors=F))
  } else {
    as.data.frame(results, stringsAsFactors=F)
  }
}

# example of using grabXML
pitchesData <- grabXML(xmlGame, "pitch")
#...dimensions of the data frame obtained
dim(pitchesData)


##############################
# calculating pitch trajectory

# position of a pitch at time t
pitchloc <- function(t, x0, ax, vx0, y0, ay, vy0, z0, az, vz0) {
  x <- x0 + vx0 * t + 0.5 * ax * I(t ^ 2)
  y <- y0 + vy0 * t + 0.5 * ay * I(t ^ 2)
  z <- z0 + vz0 * t + 0.5 * az * I(t ^ 2)  
  if(length(t) == 1) {
    loc<-c(x, y, z)
  } else {
    loc <- cbind(x, y, z)
  }
  return(loc)
}

# trajectory: position calculated at time intervals of .01 (default) seconds
pitch.trajectory <- function(x0, ax, vx0, y0, ay, vy0, z0, az, vz0,
                             interval=.01) {
  cross.plate <- (-1 * vy0 - sqrt(I(vy0 ^ 2) - 2 * y0 * ay)) / ay
  tracking <- t(sapply(seq(0, cross.plate, interval), pitchloc, x0 = x0, 
                       ax = ax, vx0 = vx0, y0 = y0, ay = ay, vy0 = vy0, z0 = z0, az = az, 
                       vz0 = vz0))
  colnames(tracking) <- c("x", "y", "z")
  tracking <- data.frame(tracking)
  return(tracking)    
}


################################
# cross-referencing players' IDs

# Baseball Prospectus' source:
players <- read.csv("http://www.baseballprospectus.com/sortable/playerids/playerid_list.csv")
head(players)