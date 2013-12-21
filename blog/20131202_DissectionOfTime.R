# R Script for blog posterior on December 2, 2013

# focus on all games played on September 5, 2013

library(pitchRx)
dat <- scrapeFX(start = "2013-09-05", end = "2013-09-05")

pitches <- plyr::join(dat$pitch, dat$atbat, 
                      by = c("num", "url"), type = "inner")

# focus on the time stamp variable sv_id
# create a new variable time which is the number of seconds
# past midnight

pitches$hours <- as.numeric(substr(pitches$sv_id, 8, 9))
pitches$minutes <- as.numeric(substr(pitches$sv_id, 10, 11))
pitches$seconds <- as.numeric(substr(pitches$sv_id, 12, 13))
pitches$time <- with(pitches, 3600 * hours + 60 * minutes + seconds)

# game.id variable -- focus on game on Sept 5, 2013 between 
# Arizona and San Francisco

pitches$game.id <- substr(pitches$url, 66, 95)
pitches1 <- subset(pitches, game.id=="gid_2013_09_05_arimlb_sfnmlb_1")
pitches1 <- pitches1[order(pitches1$time), ]

# length.of.game <- diff(range(pitches1$time))
# 11466 seconds = 3 hours, 11 minutes, 6 seconds
# 284 pitches

# graph times between pitches using ggplot2

# create a new data frame

time.data <- data.frame(Time=diff(pitches1$time), 
                        Index=1:(length(pitches1$time) - 1), 
                        Inning=pitches1$inning[-1])

library(ggplot2)
ggplot(time.data, aes(Index, Time, label=Inning)) + 
  geom_text(size=6, color="blue") +
  geom_hline(yintercept=60) +
  geom_hline(yintercept=120) +
  geom_hline(yintercept=180) +
  geom_text(data = NULL, x = 25, y = 65, label = "1 MINUTE", size=8) +
  geom_text(data = NULL, x = 25, y = 125, label = "2 MINUTES", size=8) +
  geom_text(data = NULL, x = 25, y = 185, label = "3 MINUTES", size=8) +
  labs(title = "Times Between Pitches in a Baseball Game") +
  theme(plot.title = element_text(size = rel(2))) +
  theme(axis.title = element_text(size = rel(2))) +  
  theme(axis.text = element_text(size = rel(2))) +
  ylab("Time (Seconds)")

