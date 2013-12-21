# Regression of OPS Stats

# Reference:  http://baseballwithr.wordpress.com/2013/12/18/regression-of-ops-stats/

# Need 2013 Lahman's database (http://www.seanlahman.com/baseball-archive/statistics/)


Batting <- read.csv("C:/Users/Jim/Desktop/lahman2013/Batting.csv")

# look at hitting data for last two years

Batting.12.13 <- subset(Batting, yearID==2012 | yearID==2013)

# collapse over stint variable

sum.function <- function(d){
  d1 <- d[, 6:23]
  apply(d1, 2, sum)
}
D <- ddply(Batting.12.13, .(playerID, yearID), sum.function)

# compute the OBP, SLG, and OPS stats

Batting.12.13$X1B <- with(Batting.12.13, H - X2B - X3B - HR)
Batting.12.13$SLG <- with(Batting.12.13,
                         (X1B + 2 * X2B + 3 * X3B + 4 * HR) / AB)
Batting.12.13$OBP <- with(Batting.12.13,
                         (H + BB + HBP) / (AB + BB + HBP + SF))
Batting.12.13$OPS <- with(Batting.12.13, SLG + OBP)

# merge the 2012 and 2013 stats

Batting.2012 <- subset(Batting.12.13, yearID==2012)
Batting.2013 <- subset(Batting.12.13, yearID==2013)
merged.Batting <- merge(Batting.2012, Batting.2013, by="playerID")

# limit to players with at least 300 AB each year

merged.Batting.300 <- subset(merged.Batting, 
                             AB.x >= 300 & AB.y >=300)

# construct graph

merged.Batting.300$Improvement <- with(merged.Batting.300, 
                                       OPS.y - OPS.x)

with(merged.Batting.300, 
     plot(OPS.x, Improvement, pch=".",
      xlab="2012 OPS", ylab="Improvement in 2013 OPS", 
      main="OPS for 2 Years for MLB Batters With Minimum 300 AB"))
fit <- lm(Improvement ~ OPS.x, data=merged.Batting.300)
abline(fit, lwd=3, col="red")
with(merged.Batting.300, 
     text(OPS.x, Improvement, substr(playerID, 1, 5)))


