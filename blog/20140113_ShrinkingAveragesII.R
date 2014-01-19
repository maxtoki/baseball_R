# R Script for Jan 13, 2014 and Jan 20, 2014 blogs

# function shrink will compute improved estimates at true batting rates
# for all players with at least Lower.Bound opportunities

# be sure that packages Lahman, LearnBayes, and plyr have been installed
# before running this function

# start with function introduced in last blog posting

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

# illustrate it for 2012 hit data, all batters with at least 200 AB

year <- 2012
n.var <- "H"; d.var <- "AB"
Lower.Bound <- 200
s.out <- shrink(year, n.var, d.var, Lower.Bound)

str(s.out)

# illustrate it for 1941 hit data

d1941.H <- shrink(1941, "H", "AB")

# different pattern if we look at 1941 home run rates

d1941.HR <- shrink(1941, "HR", "AB")

# use ggplot2 to construct better graphs for displaying the shrinkage

ggplot.shrink <- function(stuff){
  require(ggplot2)
  require(grid)
  names(stuff$data)[2:3] <- c("y", "n")
  ggplot(stuff$data, aes(x=n, xend=n, 
                         y=observed, yend=estimate)) + 
    geom_segment(arrow = arrow(length = unit(0.3, "cm"))) +
    geom_hline(yintercept=stuff$p.ALL, color="red") +
    labs(title = paste("Shrinkage of Multilevel Estimates", 
                       ", K =", round(stuff$K))) +
    theme(plot.title = element_text(size = rel(2))) +
    xlab("OPPORTUNITIES") + ylab("ESTIMATE") 
}

ggplot.shrink(d1941.H)

ggplot.shrink(d1941.HR)
