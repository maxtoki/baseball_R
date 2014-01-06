# Shrinking Batting Averages

# Reference: http://baseballwithr.wordpress.com/2014/01/03/shrinking-batting-averages/

# function shrink will compute improved estimates at true batting rates
# for all players with at least Lower.Bound opportunities

# be sure that packages Lahman, LearnBayes, and plyr have been installed
# before running this function

shrink <- function(year, n.var, d.var, Lower.Bound=100){
  # load required packages (each should be installed first)
  require(Lahman)
  require(LearnBayes)
  require(plyr)
  # data handling part
  Batting.year <- subset(Batting, yearID==year)
  data <- Batting.year[, c("playerID", "stint", n.var, d.var)]
  data <- data[complete.cases(data), ]
  # collapse over stint variable
  sum.function <- function(d){
    apply(d[, c(n.var, d.var)], 2, sum)}
  data <- ddply(data, .(playerID), sum.function) 
  # collect (n.var, d.var) data for all players with Lower.Bound opportunities
  data.LB <- data[data[, d.var] >= Lower.Bound, ]
  # fit the multilevel model
  fit <- laplace(betabinexch, c(0, 2), data.LB[, -1])
  p.all <- exp(fit$mode[1]) / (1 + exp(fit$mode[1]))
  K <- exp(fit$mode[2])
  # compute the multilevel model estimate
  data.LB$observed <- data.LB[, 2] / data.LB[, 3]
  data.LB$estimate <- (data.LB[, 2] + K * p.all) / (data.LB[, 3] + K)
  # graph the original and improved estimates against opportunities
  plot(data.LB[, 3], data.LB$observed,
     xlab="OPPORTUNITY", ylab="ESTIMATE",
     main=paste("(", n.var, ",", d.var,") Data in", year, "Season:",
                "pALL =", round(p.all, 3),",K =", round(K)))
  points(data.LB[, 3], data.LB$estimate, pch=19, col="red")
  abline(h=p.all, lwd=2, col="blue")
  legend("topleft", legend=c("ACTUAL", "IMPROVED"), 
         pch=c(1, 19), col=c("black", "red"))
  list(data=data.LB, p.ALL=p.all, K=K)
}

# example 1 -- estimating AVG = H / AB for all players in 2012 season with at least
# 200 at-bats.
# the output (stuff) is a list containing
# - data - data frame of player ids, y, n, actual and improved estimates
# - K - estiamted value of K from model
# - p.ALL - estimated value of p.ALL from model

year <- 2012
n.var <- "H"; d.var <- "AB"
Lower.Bound <- 200
stuff <- shrink(year, n.var, d.var, Lower.Bound)

# example 2 -- estimating strikeout outs SO / AB for all players in 2012 season
# with at least 200 at bats

year <- 2012
n.var <- "SO"; d.var <- "AB"
Lower.Bound <- 200
stuff <- shrink(year, n.var, d.var, Lower.Bound)

# note that the shrinkage of the individual rates towards the overall rate is modest,
# since much of the variability in observed rates is due to differences in
# strikeout abilities

# example 3 - estimate HR rates from (famous) 1998 season
# again focus on players with at least 200 at-bats

year <- 1998
n.var <- "HR"; d.var <- "AB"
Lower.Bound <- 200
stuff <- shrink(year, n.var, d.var, Lower.Bound)

# here shrinkage is modest since there is substantial variation in true HR rates

# example 4 - look at GIDP rates in 2012 season

year <- 2012
n.var <- "GIDP"; d.var <- "AB"
Lower.Bound <- 200
stuff <- shrink(year, n.var, d.var, Lower.Bound)

# here we have high shrinkage since there is small variation in players' true
# GIDP rates

# example 5 -- look at doubles rate X2B / H (proportion of hits that are doubles)

year <- 2012
n.var <- "X2B"; d.var <- "H"
Lower.Bound <- 100
stuff <- shrink(year, n.var, d.var, Lower.Bound)

# high shrinkage -- players have similar true doubles rate

# example 6 -- look at hr rate HR / H (proportion of hits that are homeruns)

year <- 2012
n.var <- "HR"; d.var <- "H"
Lower.Bound <- 100
stuff <- shrink(year, n.var, d.var, Lower.Bound)

#############################################################################
