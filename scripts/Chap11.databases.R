##############################################
# Chapter 11 Learning About Park Eects by 
#                    Database Management Tools 
#
# RMySQL version of the codes
#
# use data files game_log_header.csv
#
# use R packages RMySQL*, lattice, ggplot2
#
# *notes for installation on windows: 
#    http://tinyurl.com/RMySQL1 
#    and http://tinyurl.com/RMySQL2
#
# MySQL installation needed (see book chapter)
#
###############################################


# Section 11.3 Connecting R to MySQL

library(RMySQL)
conn <- dbConnect(MySQL(), user='username', pwd='password'
                  , dbname='RBaseball')
dbDisconnect(conn=conn)

# Section 11.4 Filling a MySQL Game Log Database from R

load.gamelog <- function(season){
  download.file(
    url=paste("http://www.retrosheet.org/gamelogs/gl", season
              , ".zip", sep="")
    , destfile=paste("gl", season, ".zip", sep="")
  )
  unzip(paste("gl", season, ".zip", sep=""))
  gamelog <- read.table(paste("gl", season, ".txt", sep="")
                        , sep=",", stringsAsFactors=F)
  file.remove(paste("gl", season, ".zip", sep=""))
  file.remove(paste("gl", season, ".txt", sep=""))
  gamelog
}

gl2012 <- load.gamelog(2012)
glheaders <- read.csv("game_log_header.csv")
names(gl2012) <- names(glheaders)

library(RMySQL)
conn <- dbConnect(MySQL(), user='username'
                  , dbname='RBaseball')
dbWriteTable(conn, name="gamelogs", value=gl2012
             , append=TRUE, row.names=F)
dbDisconnect(conn=conn)

appendGameLogs <- function(
  start=1871, end=2012, connPackage="RODBC"
  , headersFile="game_log_header.csv"
  , dbTableName="gamelogs", ...
){
  require(package=connPackage, character.only=TRUE)
  glheaders <- read.csv(headersFile)
  if(connPackage == "RMySQL"){
    conn <- dbConnect(MySQL(), ...)
  } else {
    conn <- odbcConnect(...)
  }
  for(season in start:end){
    print(paste(Sys.time(), "working on season:", season))
    flush.console()
    gamelogs <- load.gamelog(season)
    glheaders <- read.csv(headersFile)
    names(gamelogs) <- names(glheaders)
    gamelogs$GAME_ID <- paste(gamelogs$HomeTeam, gamelogs$Date
                              , gamelogs$DoubleHeader, sep="")
    gamelogs$YEAR_ID <- substr(gamelogs$Date, 1, 4)
    if(connPackage == "RMySQL"){
      dbWriteTable(conn, name=dbTableName, value=gamelogs
                   , append=TRUE, row.names=F)
    } else {
      sqlSave(conn, dat=gamelogs, tablename=dbTableName
              , append=TRUE, rownames=FALSE)
    }
  }
}

appendGameLogs(start=1873, end=1874, connPackage="RMySQL"
               , dbname="rbaseball", user="user", password="password")


# Section 11.5 Querying Data from R

conn <- dbConnect(MySQL(), user='username', pwd='password'
                  , dbname='RBaseball')
chiAttendance <- dbGetQuery(conn, "
select date, hometeam, dayofweek
                            , attendance
                            from gamelogs
                            where date > 20000101
                            and hometeam in ('CHN', 'CHA')
                            ")

chiAttendance$attendence <- ifelse(chiAttendance$attendence == 0
                                   , NA, chiAttendance$attendence)
chiAttendance$dayofweek <- factor(chiAttendance$dayofweek
                                  , levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
avgAtt <- aggregate(attendence ~ hometeam + dayofweek
                    , data=chiAttendance, FUN=mean)

library(lattice)
xyplot(attendence ~ dayofweek, data=avgAtt
       , groups=hometeam
       , pch=c("S", "C"), cex=2, col= "black"
       , xlab="day of week"
       , ylab="attendance"
)



rockies.games <- dbGetQuery(conn, "select year_id, date, parkid
  , visitingteam, hometeam
  , visitorrunsscored as awR
  , homerunsscore as hmR
  from gamelogs
  where (hometeam='COL'
  or visitingteam='COL')
  and year_id > 1994")

rockies.games$runs <- rockies.games$awR + rockies.games$hmR
rockies.games$coors <- (rockies.games$parkid == "DEN02")

library(ggplot2)
ggplot(aes(x=year_id, y=runs, linetype=coors), data=rockies.games) +
  stat_summary(fun.data="mean_cl_boot") +
  xlab("season") +
  ylab("runs per game (both teams combined)") +
  scale_linetype_discrete(name="location"
                          , labels=c("other parks", "Coors Field"))


# Section 11.6 Baseball Data as MySQL Dumps
shell("mysql < C:/Baseball/Datadumps/lahman2012.sql -u username
  -p password")

shell("mysql < C:/Baseball/Datadumps/2010s_Retrosheet.sql
  -u username -p password RBaseball")

shell("mysql < C:/Baseball/Datadumps/pbp2.sql
  -u username -p password RBaseball")

# Section 11.7 Calculating Basic Park Factors
options(stringsAsFactors=F)
hrPF <- dbGetQuery(conn, "
  select away_team_id, home_team_id, event_cd
  from events
  where year_id=1996
  and event_cd in (2, 18, 19, 20, 21, 22, 23)
")

hrPF$event_fl <- ifelse(hrPF$event_cd == 23, 1, 0)
evAway <- aggregate(event_fl ~ away_team_id, data=hrPF
                    , FUN=mean)
evHome <- aggregate(event_fl ~ home_team_id, data=hrPF
                    , FUN=mean)
evCompare <- merge(evAway, evHome, by.x="away_team_id"
                   , by.y="home_team_id", suffixes=c("_A", "_H"))
names(evCompare)[1] <- "team_id"
head(evCompare)

evCompare$PF <- round(100 * evCompare$event_fl_H
                      / evCompare$event_fl_A)
evCompare <- evCompare[order(-evCompare$PF),]
head(evCompare)
tail(evCompare)

andres <- sqlQuery(conn, "
  select away_team_id, home_team_id, event_cd
  from events
  where year_id=1996
  and event_cd in (2, 18, 19, 20, 21, 22, 23)
  and bat_id='galaa001'
")

andres$event_fl <- ifelse(andres$event_cd == 23, 1, 0)

andres <- merge(andres, evCompare[,c("team_id", "PF")]
                , by.x="home_team_id"
                , by.y="team_id")
andresPF <- mean(andres$PF)
andresPF
47 / andresPF * 100


dbDisconnect(conn=conn)
