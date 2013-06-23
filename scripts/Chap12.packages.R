#####################################################################
# Chapter 12 - Exploring Fielding metrics with Contributed R Packages
#
# use data files FanGraphs Leaderboard.csv, teamD.csv,
#                 jeter_rollins.csv, 
#
#   plus .xls files from Wizardry's Appendix
#         (placed in the "wizardry" subfolder)
#
# use R packages XLConnect, doBy, stringdist, plyr,
#                 weights, ellipse, psych, reshape2,
#                 ggplot2, directlabels
# 
#####################################################################


# Section 12.2.3 Reading an Excel spreadsheet (package XLConnect)

library(XLConnect)
xlwzr <- loadWorkbook("wizardry/Appendix_C_Shortstop.xls")
wzr <- readWorksheet(xlwzr, sheet = 1, startRow = 7)
wzr <- subset(wzr, Year == 2009)
wzr$Name <- paste(wzr$First, wzr$Last)
head(wzr)

# Section 12.2.4 Summarizing multiple columns (package doBy)

subset(wzr, Name == "Orlando Cabrera")

library(doBy)
wzr <- summaryBy(IP + Runs + v.Tm ~ Name, data = wzr, FUN = sum
                 , keep.names = TRUE)

head(wzr)

# Section 12.2.5 Finding the most similar string (stringdist)

fg <- read.csv("FanGraphs Leaderboard.csv")
names(fg)[1] <- "Name"
head(fg)

fg.mismatches <- setdiff(fg$Name, wzr$Name)
wzr.mismatches <- setdiff(wzr$Name, fg$Name)

fg.mismatches
wzr.mismatches

library(stringdist)
dm <- stringdistmatrix(fg.mismatches, wzr.mismatches)
dm

index.min <- function(v) which(v == min(v))
idx <- apply(dm, MARGIN = 1, FUN = index.min)
names.mapped <- data.frame(fgName = fg.mismatches
                           , wzrName = wzr.mismatches[idx])
names.mapped

wzr <- merge(wzr, names.mapped, by.x = "Name", by.y = "wzrName"
             , all.x = TRUE)
wzr$fgName <- ifelse(is.na(wzr$fgName), wzr$Name, wzr$fgName)

# Section 12.2.6 Applying a function on multiple columns (plyr)

defense <- merge(fg, wzr[,-1], by.x = "Name", by.y = "fgName")
defense <- defense[,c("Name", "Inn", "Plays", "UZR", "DRS"
                      , "TZL",  "RZR", "FSR", "Runs")]

coalesce <- function(x, dflt = 0) ifelse(is.na(x), dflt, x)

library(plyr)
coalesceColumns <- colwise(coalesce)
defense <- coalesceColumns(defense)

# 12.2.7 Weighted correlations (weights)

round(cor(defense[,-1:-3]),3)

library(weights)
round(wtd.cor(defense[,-1:-3], weight=defense$Inn)$correlation, 3)

# 12.2.8 Displaying correlation matrices (ellipse)

library(ellipse)
Dcor <- wtd.cor(defense[,-1:-3], weight=defense$Inn)$correlation
plotcorr(Dcor)

# 12.2.9 Evaluating the fielding metrics (psych)

teamD <- read.csv("teamD.csv")
Dcor <- wtd.cor(teamD[,c("DER", "UZR", "DRS", "TZL", "RZR"
                         , "FSR", "Runs")], weight=teamD$IP)

library(psych)
sortedCor <- mat.sort(Dcor$correlation)
cor.plot(sortedCor, numbers = TRUE)

# 12.3 Comparing Two Shortstops

jetrol <- read.csv("jeter_rollins.csv")
head(jetrol)
names(jetrol)[1] <- "Season"

# package reshape

library(reshape2)
jetrol2 <- melt(jetrol
                , id.vars = c("Season", "Name")
                , measure.vars = c("DRS", "UZR")
                , variable.name = "fieldingMetric"
                , value.name = "runs")
head(jetrol2)

library(ggplot2)
p <- ggplot(jetrol2, aes(x = Season, y = runs
                         , col = fieldingMetric)) +
  geom_line() +
  facet_grid(. ~ Name) +
  scale_color_manual(name = "Fielding\nmetric"
                     , values = c("black", "grey70")) +
  scale_x_continuous(breaks = seq(2004, 2012, 4)) +
  geom_hline(yintercept = 0, lty = 3)

# direct labels

library(directlabels)
direct.label(p)

