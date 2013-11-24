# World Series Strikeouts

# Reference:  http://baseballwithr.wordpress.com/2013/11/02/world-series-strikeouts/

# Need to install the Lahman package
# install.packages("Lahman")

# load in the Lahman package

library(Lahman)

# look at the BattingPost data for the World Series for all
# seasons since 1903

wsdata <- subset(BattingPost, 
                 round=="WS" & yearID >= 1903)

# load in the plyr package

library(plyr)

# compute the number of at-bats and strikeouts for each series

so.data <- ddply(wsdata, .(yearID), summarize,
          AB = sum(AB, na.rm=TRUE), 
          SO = sum(SO, na.rm=TRUE))

# add data from the 2013 World Series

so.data <- rbind(so.data,
          data.frame(yearID = 2013,
                    AB = 194 + 201,
                    SO = 59 + 43))

# compute strikeout rates

so.data$SO.Rate <- with(so.data, 100 * SO / AB)

# plot strikeout rates across time and add smoothing curve

with(so.data, plot(yearID, SO.Rate,
            main="Strikeout Rates in World Series History"))
with(so.data, lines(lowess(yearID, SO.Rate)))

# manually add labels for 11 unusually points with the mouse

with(so.data, identify(yearID, SO.Rate, 
                       n=11, labels=yearID))
