###############################
# Chapter 6 - Advanced Graphics
# 
# required packages: 
#   lattice, ggplot2, jpeg
# 
# datafiles used: 
#   balls_strikes_count.Rdata 
#   Comerica.jpg 
# 
#############################

# Section 6.1 Introduction

load("balls_strikes_count.Rdata")

# Section 6.2 The lattice Package

library(lattice)

# set the theme to produce b/w figure (to be used for reproducing the book plots)

trellis.par.set(canonical.theme(color = FALSE))

# show sample rows of verlander data.frame

sampleRows <- sample(1:nrow(verlander), 20)
verlander[sampleRows,]

# histogram and density plot of speed

histogram(~ speed, data = verlander)

densityplot(~ speed, data = verlander, plot.points = FALSE)

#...conditional to pitch type

densityplot(~ speed | pitch_type, data = verlander, layout=c(1,5)
            , plot.points = F)

#...superposing

densityplot(~ speed, data = verlander, groups = pitch_type,
            plot.points = FALSE,
            auto.key = TRUE)

# scatterplots of speed trends

# subset on four-seam fastballs

F4verl <- subset(verlander, pitch_type == "FF")

# day of the year

F4verl$gameDay <- as.integer(format(F4verl$gamedate, format="%j"))

# mean speed by day of the year

dailySpeed <- aggregate(speed ~ gameDay + season, data = F4verl
                        , FUN = mean)

# scatter plot

xyplot(speed ~ gameDay | factor(season),
       data = dailySpeed,
       xlab = "day of the year",
       ylab = "pitch speed (mph)")

# Verlander's fastball/change-up speed differential

# subset Four-seamers and change-ups

speedFC <- subset(verlander, pitch_type %in% c("FF", "CH"))

# average speed by season and pitch type

avgspeedFC <- aggregate(speed ~ pitch_type + season,
                        data = speedFC, FUN = mean)

#...remove unused factor levels

avgspeedFC <- droplevels(avgspeedFC)
avgspeedFC

# dotplot

dotplot(factor(season) ~ speed, groups = pitch_type,
        data = avgspeedFC,
        pch = c("C", "F"), cex = 2)

# panel function: speed trend in the game

# average fastball speed by pitch count and season

avgSpeed <- aggregate(speed ~ pitches + season, data = F4verl,
                      FUN = mean)

#...overall average

avgSpeedComb <- mean(F4verl$speed)

# scatterplot of data (not annotated)

xyplot(speed ~ pitches | factor(season),
       data = avgSpeed)

# add the refenece lines (and text + arrows)

xyplot(speed ~ pitches | factor(season)
       , data = avgSpeed
       , panel = function(...){
         panel.xyplot(...)
         panel.abline(v = 100, lty = "dotted")
         panel.abline(h = avgSpeedComb)
         panel.text(25, 100, "avg. speed")
         panel.arrows(25, 99.5, 0, avgSpeedComb
                      , length = .1)
       }
)

# building a lattice plot step-by-step

# get data on Verlander's 2011 no-hitter

NoHit <- subset(verlander, gamedate == "2011-05-07")

# basic plot

xyplot(pz ~ px | batter_hand, data=NoHit, groups=pitch_type,
       auto.key = TRUE)

# isometric scales

xyplot(pz ~ px | batter_hand, data=NoHit, groups=pitch_type,
       auto.key = TRUE,
       aspect = "iso")

# limits and labels on axes

xyplot(pz ~ px | batter_hand, data=NoHit, groups=pitch_type,
       auto.key = TRUE,
       aspect = "iso",
       xlim = c(-2.2, 2.2),
       ylim = c(0, 5),
       xlab = "Horizontal Location\n(ft. from middle of plate)",
       ylab = "Vertical Location\n(ft. from ground)")

# prepare a custom legend (first a vector with pitch names)

pitchnames <- c("change-up", "curveball", "4S-fastball"
                , "2S-fastball", "slider")

myKey <- list(space = "right",
              border = TRUE,
              cex.title = .8,
              title = "pitch type",
              text = pitchnames,
              padding.text = 4
)

# define the boundaries of the strike zone

topKzone <- 3.5
botKzone <- 1.6
inKzone <- -.95
outKzone <- 0.95

# putting it all together

xyplot(pz ~ px | batter_hand, data=NoHit, groups=pitch_type,
       auto.key = myKey,
       aspect = "iso",
       xlim = c(-2.2, 2.2),
       ylim = c(0, 5),
       xlab = "horizontal location\n(ft. from middle of plate)",
       ylab = "vertical location\n(ft. from ground)",
       panel = function(...){
         panel.xyplot(...)
         panel.rect(inKzone, botKzone, outKzone, topKzone,
                    border = "black", lty = 3)
       }
)

# Section 6.3 The ggplot2 package

# show sample rows of cabrera data.frame

sampleRows <- sample(1:nrow(cabrera), 20)
cabrera[sampleRows,]

# load ggplot2 package

library(ggplot2)

# plot by layer

# map data to aesthetics (p0 won't produce a plot yet)

p0 <- ggplot(data = cabrera, aes(x = hitx, y = hity))

# add a plot layer (points)

p1 <- p0 + geom_point()
p1

# map outcome to color aesthetic

p0 <- ggplot(data = cabrera, aes(hitx, hity))

# add points

p1 <- p0 + geom_point(aes(colour = hit_outcome))

# impose isometric scales (uncomment to manually choose colors)

p2 <- p1 + coord_equal() # + scale_color_manual(values = c("black", "grey40", "grey80"))

#draw the plot

p2

# facet by season

p3 <- p2 + facet_wrap(~ season)
p3

# prepare data for drawing basepaths

bases <- data.frame(x = c(0, 90/sqrt(2), 0, -90/sqrt(2), 0),
                    y = c(0, 90/sqrt(2), 2 * 90/sqrt(2), 90/sqrt(2), 0)
)

# add the path and foul lines

p4 <- p3 + geom_path(data = bases, aes(x = x, y = y))
p4 +
  geom_segment(x = 0, xend = 300, y = 0, yend = 300) +
  geom_segment(x = 0, xend = -300, y = 0, yend = 300)

# combining multiple aesthetics

# get data for the final month of 2012

cabreraStretch <- subset(cabrera, gamedate > "2012-08-31")

p0 <- ggplot(data = cabreraStretch, aes(hitx, hity))
p1 <- p0 + geom_point(aes(shape = hit_outcome, colour = pitch_type
                          , size = speed))
p2 <- p1 + coord_equal()
p3 <- p2 + geom_path(aes(x = x, y = y), data = bases)
p4 <- p3 + guides(col = guide_legend(ncol = 2))
p4 +
  geom_segment(x = 0, xend = 300, y = 0, yend = 300) +
  geom_segment(x = 0, xend = -300, y = 0, yend = 300)

# smooth line with error bands

ggplot(F4verl, aes(pitches, speed)) +
  facet_wrap(~ season) +
  geom_line(stat = "hline", yintercept = "mean", lty = 3) +
  geom_point(aes(pitches, speed),
             data = F4verl[sample(1:nrow(F4verl), 1000),]) +
               geom_smooth(col = "black") +
               geom_vline(aes(xintercept = 100), col = "black", lty = 2)

# from cluttered scatter plot to hexagonal binning

# put strike zone boundaris in a data frame

kZone <- data.frame(
  x = c(inKzone, inKzone, outKzone, outKzone, inKzone)
  , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

# cluttered scatter plot

ggplot(F4verl, aes(px, pz)) +
  geom_point() +
  facet_wrap(~ batter_hand) +
  coord_equal() +
  geom_path(aes(x, y), data = kZone, lwd = 2, col = "white")

# hexagonal binning
# (requires the package hexbin to be installed)

ggplot(F4verl, aes(px, pz)) +
  stat_binhex() +
  facet_wrap(~ batter_hand) +
  coord_equal() +
  geom_path(aes(x, y), data = kZone, lwd = 2, col = "white", alpha = .3)

# adding a background image

# load jpeg package

library(jpeg)

# load the Comerica Park diagram

diamond <- readJPEG("Comerica.jpg")

# spray chart overlaid on jpeg image

ggplot(cabrera, aes(hitx, hity)) +
  coord_equal() +
  annotation_raster(diamond, -310, 305, -100, 480) +
  stat_binhex(alpha = .9, binwidth = c(5, 5)) +
  scale_fill_gradient(low = "grey70", high = "black")

###############################################################