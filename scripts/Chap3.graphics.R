##############################################
# Chapter 3 Traditional Graphics
#
# Needs datafiles hofbatting.csv, all1998.csv,
#                 fields.csv, retrosheetIDs.csv
#   plus .csv files from the Lahman's database
#         (placed in the "lahman" subfolder)
# 
##############################################

# Section 3.1 Introduction

hof <- read.csv("hofbatting.csv")

hof$MidCareer <- with(hof, (From + To) / 2)

hof$Era <- cut(hof$MidCareer,
        breaks = c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
        labels = c("19th Century", "Lively Ball", "Dead Ball",
                    "Integration", "Expansion", "Free Agency", 
                    "Long Ball"))

T.Era <- table(hof$Era)
T.Era

barplot(T.Era)
barplot(table(hof$Era), xlab="Era", ylab="Frequency", 
        main="Era of the Nonpitching Hall of Famers")

plot(table(hof$Era))
pie(table(hof$Era))

# Section 3.3 Saving Graphs

png("../output/bargraph.png")
barplot(table(hof$Era), xlab="Era", ylab="Frequency",
        main="Era of the Nonpitching Hall of Famers")
dev.off()

pdf("../output/graphs.pdf")
barplot(table(hof$Era))
plot(table(hof$Era))
dev.off()

# Section 2.4 Dot plots

T.Era <- table(hof$Era)
dotchart(as.numeric(T.Era), labels=names(T.Era), xlab="Frequency")

hof.500 <- subset(hof, HR >= 500)
hof.500 <- hof.500[order(hof.500$OPS), ]
dotchart(hof.500$OPS, labels=hof.500$X, xlab="OPS")

# Section 2.5 Numeric Variable: Stripchart and Histogram

windows(width=7, height=3.5)
stripchart(hof$MidCareer, method="jitter", pch=1, 
           xlab="Mid Career")
dev.off()

hist(hof$MidCareer, xlab="Mid Career", main="")

hist(hof$MidCareer, xlab="Mid Career", main="",
     breaks=seq(1880, 2000, by=20))

# Section 2.6 Two Numeric Variables

with(hof, plot(MidCareer, OPS))
with(hof, lines(lowess(MidCareer, OPS, f=0.3)))
with(hof, identify(MidCareer, OPS, X, n=4))
  #... identify points on the plot by mouse-clicking 
  #... then press ESC

with(hof, plot(OBP, SLG))

with(hof, plot(OBP, SLG, xlim=c(0.25, 0.50), 
               ylim=c(0.28, 0.75), pch=19,
               xlab="On Base Percentage",
               ylab="Slugging Percentage"))

curve(.7 - x, add = TRUE)
curve(.8 - x, add = TRUE)
curve(.9 - x, add = TRUE)
curve(1.0 - x, add = TRUE)

text(.27, .42, "OPS = 0.7")
text(.27, .52, "OPS = 0.8")
text(.27, .62, "OPS = 0.9")
text(.27, .72, "OPS = 1.0")

with(hof, identify(OBP, SLG, X, n=6))
  #... identify points on the plot by mouse-clicking 
  #... then press ESC


# Section 3.7 A Numeric Variable and a Factor Variable

hof$HR.Rate <- with(hof, HR / AB)

stripchart(HR.Rate ~ Era, data=hof)

par(plt = c(.2, .94, .145, .883))
stripchart(HR.Rate ~ Era, data = hof, 
          method="jitter", pch=1, las=2)

par(plt=c(.2, .94, .145, .883))
boxplot(HR.Rate ~ Era, data=hof, las=2,
        horizontal=TRUE, xlab="HR Rate")

# Section 3.8 Comparing Ruth, Aaron, Bonds, and A-Rod

master <- read.csv("lahman/Master.csv")

getinfo <- function(firstname, lastname){
  playerline <- subset(master,
                       nameFirst==firstname & nameLast==lastname)
  name.code <- as.character(playerline$playerID)
  birthyear <- playerline$birthYear
  birthmonth <- playerline$birthMonth
  birthday <- playerline$birthDay
  byear <- ifelse(birthmonth <= 6, birthyear, birthyear + 1)
  list(name.code=name.code, byear=byear)}

ruth.info <- getinfo("Babe", "Ruth")
aaron.info <- getinfo("Hank", "Aaron")
bonds.info <- getinfo("Barry", "Bonds")
arod.info <- getinfo("Alex", "Rodriguez")
ruth.info

batting <- read.csv("lahman/Batting.csv")

ruth.data <- subset(batting, playerID == ruth.info$name.code)
ruth.data$Age <- ruth.data$yearID - ruth.info$byear

aaron.data <- subset(batting, playerID == aaron.info$name.code)
aaron.data$Age <- aaron.data$yearID - aaron.info$byear
bonds.data <- subset(batting, playerID == bonds.info$name.code)
bonds.data$Age <- bonds.data$yearID - bonds.info$byear
arod.data <- subset(batting, playerID == arod.info$name.code)
arod.data$Age <- arod.data$yearID - arod.info$byear

cumsum(c(1, 2, 3, 4))

with(ruth.data, plot(Age, cumsum(HR), type="l", lty=3, lwd=2,
                     xlab="Age", ylab="Career Home Runs",
                     xlim=c(18, 45), ylim=c(0, 800)))
with(aaron.data, lines(Age, cumsum(HR), lty=2, lwd=2))
with(bonds.data, lines(Age, cumsum(HR), lty=1, lwd=2))
with(arod.data, lines(Age, cumsum(HR), lty=4, lwd=2))
legend(20, 700, legend=c("Bonds", "Aaron", "Ruth", "ARod"),
       lty=1 : 4, lwd=2)

# Section 3.9 The 1998 Home Run Race

data1998 <- read.csv("all1998.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data1998) <- fields[, "Header"]

retro.ids <- read.csv("retrosheetIDs.csv")
sosa.id <- as.character(subset(retro.ids,
                               FIRST=="Sammy" & LAST=="Sosa")$ID)

mac.id <- as.character(subset(retro.ids,
                              FIRST=="Mark" & LAST=="McGwire")$ID)

sosa.data <- subset(data1998, BAT_ID == sosa.id)
mac.data <- subset(data1998, BAT_ID == mac.id)

createdata <- function(d){
  d$Date <- as.Date(substr(d$GAME_ID, 4, 11),
                    format="%Y%m%d")
  d <- d[order(d$Date), ]
  d$HR <- ifelse(d$EVENT_CD == 23, 1, 0)
  d$cumHR <- cumsum(d$HR)
  d[, c("Date", "cumHR")]
}

mac.hr <- createdata(mac.data)
sosa.hr <- createdata(sosa.data)
head(sosa.hr)

plot(mac.hr, type="l", lwd=2, ylab="Home Runs in the Season")
lines(sosa.hr, lwd=2, col="grey")
abline(h=62, lty=3)
text(10440, 65, "62")
legend(10440, 20, legend=c("McGwire (70)", "Sosa (66)"),
       lwd=2, col=c("black", "grey"))

#########################################################