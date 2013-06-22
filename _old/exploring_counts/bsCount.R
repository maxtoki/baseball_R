##############################
# ball/strike counts chapter #
#                            #
# required packages:         #
# lattice                    #
# plotrix                    #
#                            #
# files used:                #
# all2011.csv                #
# fields.csv                 #
##############################

# set working directory
setwd("H:/documenti/GitHub/baseball_R")

############################
# visualizing tOPS+ by count

# prepare mussina data frame (source: http://www.baseball-reference.com/players/split.cgi?id=mussimi01&year=Career&t=p#count)
mussina <- expand.grid(balls = 0:3, strikes = 0:2)
mussina$value <- c(100, 118, 157, 207, 72, 82, 114, 171, 30, 38, 
                  64, 122)
mussina

# writing the countmap function (requires plotrix package)
countmap <- function(data){
  require(plotrix)
  data <- xtabs(value ~ ., data)
  color2D.matplot(data, show.values = 2, axes = FALSE
                  , xlab = "", ylab = "")
  axis(side = 2, at = 3.5:0.5, labels = rownames(data), las = 1) 
  axis(side = 3, at = 0.5:2.5, labels = colnames(data)) 
  mtext(text = "balls", side = 2, line = 2, cex.lab = 1)
  mtext(text = "strikes", side = 3, line = 2, cex.lab = 1)
}
countmap(mussina)


##############################
# string manipulation examples

# counting the number of characters in a string
nchar("BBSBFFFX")
# vector of sample sequences
sequences <- c("BBX", "C11BBC1S", "1X")
# identify elements containing a string
grep("1", sequences) #returns the elements index
grepl("1", sequences) # returns TRUE/FALSE
grepl("11", sequences) # string longer than one character
# substitute a string with another string
gsub("1", "", sequences)

#######################################################
# Finding plate appearances going through a given count

# load Retrosheet events and apply column headers
pbp2011 <- read.csv("data/all2011.csv")
headers <- read.csv("data/fields.csv")
names(pbp2011) <- headers$Header

# create a pitch sequence without non-pitch codes
pbp2011$pseq <- gsub("[.>123N+*]", "", pbp2011$PITCH_SEQ_TX)

# create variables indicating sequences going through 1-0 and 0-1 (and checking the first few lines)
pbp2011$c10 <- grepl("^[BIPV]", pbp2011$pseq)
pbp2011$c01 <- grepl("^[CFKLMOQRST]", pbp2011$pseq)
pbp2011[1:10, c("PITCH_SEQ_TX", "c10", "c01")]


#############################
# Expected run value by count

# load and explore data
pbp11rc <- read.csv("data/pbp11rc.csv")
pbp11rc[1:5, c("GAME_ID", "EVENT_ID", "c00", "c10", "c20", "c11",
               "c01", "c30", "c21", "c31", "c02", "c12", "c22", "c32", 
               "RUNS.VALUE")]

# compare average run value on PAs going through 1-0 and 0-1 counts
ab10 <- subset(pbp11rc, c10 == 1)
ab01 <- subset(pbp11rc, c01 == 1)
c(mean(ab10$RUNS.VALUE), mean(ab01$RUNS.VALUE))

# average RV for every count
# ...prepare data frame
runs.by.count <- expand.grid(balls = 0:3, strikes = 0:2)
runs.by.count$value <- 0
#...function for calculating RV given balls and strikes
bs.count.run.value <- function(b, s){
  column.name <- paste("c", b, s, sep = "")
  mean(pbp11rc[pbp11rc[, column.name] == 1, "RUNS.VALUE"])
}
#...apply the funcion to every possible count
runs.by.count$value <- mapply(FUN = bs.count.run.value,
                           b = runs.by.count$balls,
                           s = runs.by.count$strikes
)
#...display the heatmap
countmap(runs.by.count)


###############################
# comparisons by previous count

# expected run value for PAs going through 2-2
count22 <- subset(pbp11rc, c22 == 1)
mean(count22$RUNS.VALUE)
#...expectancy conditional on the count after 2 pitches
count22$after2 <- ifelse(count22$c20 == 1, "2-0", 
                         ifelse(count22$c02 == 1, "0-2", "1-1"))
aggregate(RUNS.VALUE ~ after2, data = count22, FUN = mean)

# doing the same for PAs going through 1-1
count11 <- subset(pbp11rc, c11 == 1)
count11$after1 <- ifelse(count11$c10 == 1, "1-0", "0-1")
aggregate(RUNS.VALUE ~ after1, data = count11, FUN = mean)



##################################
# loading the pitch by pitch files

# remove everything from memory
rm(list=ls())
# load datasets
load("data/balls_strikes_count.Rdata")
#...list them
ls()


############################################
# cabrera's swings by location: scatter plot

# select sample from cabrera data.frame
sampCabrera <- cabrera[sample(1:nrow(cabrera), 500),]

# define the boundaries of the strike zone
topKzone <- 3.5 
botKzone <- 1.6
inKzone <- -.95
outKzone <- 0.95

# load lattice package 
library(lattice)

# (uncomment the following line to get a b/w figure)
#trellis.par.set(canonical.theme(color = FALSE))

# plot (with reference strike zone)
xyplot(pz ~ px, data = sampCabrera, groups = swung,
       aspect = "iso",
       xlab = "horizontal location (ft.)",
       ylab = "vertical location (ft.)",   
       auto.key = list(points = T, text = c("not swung", "swung")
                       , space = "right"),
       panel = function(...){
         panel.xyplot(...)
         panel.rect(inKzone, botKzone, outKzone, topKzone,
                     border="black")
       })


############################################
# cabrera's swings by location: contour plot

# fit polynomial surface
miggy.loess <- loess(swung ~ px + pz, data = cabrera, 
                     control = loess.control(surface = "direct"))

# estimate likelihood of swing on a grid 
#...prepare the grid
pred.area <- expand.grid(px = seq(-2, 2, 0.1), pz = seq(0, 6, 0.1))
#...estimate likelihood
pred.area$fit <- c(predict(miggy.loess, pred.area))
#...a few sanity checks
subset(pred.area, px == 0 & pz == 2.5)  #down Broadway
subset(pred.area, px == 0 & pz == 0)  #ball in the dirt
subset(pred.area, px == 2 & pz == 2.5)  #way outside

# draw the contour plot
contourplot(fit ~ px * pz, data = pred.area,
            at = c(.2, .4, .6, .8),
            aspect = "iso",
            xlim = c(-2, 2), ylim = c(0, 5),
            xlab = "horizontal location (ft.)",
            ylab = "vertical location (ft.)",
            panel = function(...){
              panel.contourplot(...)
              panel.rect(inKzone, botKzone, outKzone, topKzone,
                         border="black", lty = "dotted")
            })


####################################
# countour plot by ball/strike count

# create column containing the pitch count
cabrera$bscount <- paste(cabrera$balls, cabrera$strikes, sep = "-")
# estimate swing likelihood on 0-0
miggy00 <- subset(cabrera, bscount == "0-0")
miggy00loess <- loess(swung ~ px + pz, data = miggy00, control = 
  loess.control(surface = "direct"))
pred.area$fit00 <- c(predict(miggy00loess, pred.area))
# estimate swing likelihood on 2-0
miggy20 <- subset(cabrera, bscount == "2-0")
miggy20loess <- loess(swung ~ px + pz, data = miggy20, control = 
  loess.control(surface = "direct"))
pred.area$fit20 <- c(predict(miggy20loess, pred.area))
# estimate swing likelihood on 0-2
miggy02 <- subset(cabrera, bscount == "0-2")
miggy02loess <- loess(swung ~ px + pz, data = miggy02, control = 
  loess.control(surface = "direct"))
pred.area$fit02 <- c(predict(miggy02loess, pred.area))

# contour plot by count
contourplot(fit00 + fit02 + fit20 ~ px * pz, data = pred.area,
            at = c(.2, .4, .6),
            aspect = "iso",
            xlim = c(-2, 2), ylim = c(0, 5),
            xlab = "horizontal location (ft.)",
            ylab = "vertical location (ft.)",
            panel = function(...){
              panel.contourplot(...)
              panel.rect(inKzone, botKzone, outKzone, topKzone,
                         border = "black", lty = "dotted")
            })



##########################
# pitch selection by count

# pitch type frequency
table(verlander$pitch_type)
#...and proportion
round(100 * prop.table(table(verlander$pitch_type)))
#...conditioned to batter's handedness
type_verlander_hand <- with(verlander, table(pitch_type, 
                                             batter_hand))
round(100 * prop.table(type_verlander_hand, margin = 2))

# selection by count
# add ball/strike count column
verlander$bscount <- paste(verlander$balls, verlander$strikes, 
                           sep="-")
#...subset on righthanded opponents
verl_RHB <- subset(verlander, batter_hand == "R")
#...table
verl_type_cnt_R <- table(verl_RHB$bscount, verl_RHB$pitch_type)
round(100 * prop.table(verl_type_cnt_R, margin = 1))



###########################
# umpires behavior by count

# show a sample of rows from umpires data.frame
umpires[sample(1:nrow(umpires), 20),]

# likelihood of called strike by location (RHB, 0-0 count)
umpiresRHB <- subset(umpires, batter_hand == "R")
ump00 <- subset(umpiresRHB, balls == 0 & strikes == 0)
ump00smp <- ump00[sample(1:nrow(ump00), 3000),] 
ump00.loess <- loess(called_strike ~ px + pz, data = ump00smp, 
                     control = loess.control(surface = "direct"))
# likelihood of called strike by location (RHB, 3-0 count)
umpiresRHB <- subset(umpires, batter_hand == "R")
ump30 <- subset(umpiresRHB, balls == 3 & strikes == 0)
ump30smp <- ump30[sample(1:nrow(ump30), 3000),] 
ump30.loess <- loess(called_strike ~ px + pz, data = ump30smp, 
                     control = loess.control(surface = "direct"))
# likelihood of called strike by location (RHB, 0-0 count)
umpiresRHB <- subset(umpires, batter_hand == "R")
ump02 <- subset(umpiresRHB, balls == 0 & strikes == 2)
ump02smp <- ump02[sample(1:nrow(ump02), 3000),] 
ump02.loess <- loess(called_strike ~ px + pz, data = ump02smp, 
                     control = loess.control(surface = "direct"))

#contour lines as a data frame
ump00contour <- contourLines(x = seq(-2, 2, 0.1),
                             y = seq(0, 6, 0.1),
                             z = predict(ump00.loess, pred.area),
                             levels = c(.5))
ump00df <- as.data.frame(ump00contour)
ump00df$bscount <- "0-0"

ump30contour <- contourLines(x = seq(-2, 2, 0.1),
                             y = seq(0, 6, 0.1),
                             z = predict(ump30.loess, pred.area),
                             levels = c(.5))
ump30df <- as.data.frame(ump30contour)
ump30df$bscount <- "3-0"

ump02contour <- contourLines(x = seq(-2, 2, 0.1),
                             y = seq(0, 6, 0.1),
                             z = predict(ump02.loess, pred.area),
                             levels = c(.5))
ump02df <- as.data.frame(ump02contour)
ump02df$bscount <- "0-2"
#...bind contours data.frames together
umpireContours <- rbind(ump00df, ump02df, ump30df)

# prepare custom legend
myKey <- list(lines = TRUE
              , points = FALSE
              , space = "right"
              , title = "balls/strikes count"
              , cex.title = 1
              , padding = 4)

# draw contour plot
xyplot(y ~ x , data = umpireContours
       , groups = bscount
       , type = "l", aspect = "iso"
       , col = "black"
       , xlim = c(-2, 2), ylim = c(0, 5)
       , xlab = "horizontal location (ft.)"
       , ylab = "vertical location (ft.)"       
       , auto.key = myKey
       , panel = function(...){
         panel.xyplot(...)
         panel.rect(inKzone, botKzone, outKzone, topKzone,
                    border = "grey70", lwd=2)
       })
