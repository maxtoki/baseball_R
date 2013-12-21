# Plotting pitches – ggplot2 tips and tricks

# http://baseballwithr.wordpress.com/2013/12/12/plotting-pitches-ggplot2-tips-and-tricks/



# read in play-by-play data
library(pitchRx)

# get data from Apr. 1, 2013
dat = scrapeFX(start="2013-04-01", end="2013-04-01")

pitches = plyr::join(dat$pitch, dat$atbat, 
                      by = c("num", "url"), type = "inner")

# Clayton Kershaw
kershaw = subset(pitches, pitcher_name == "Clayton Kershaw")

# convert characters to numbers
kershaw$px = as.numeric(kershaw$px)
kershaw$pz = as.numeric(kershaw$pz)
kershaw$start_speed = as.numeric(kershaw$start_speed)

# ggplot
library(ggplot2)

# basic example
ggplot(data=kershaw, aes(x=px, y=pz, shape=type, col=pitch_type)) +
  geom_point()

# add faceting, equal coordinates
ggplot(data=kershaw, aes(x=px, y=pz, shape=type, col=pitch_type)) +
  geom_point() +
  facet_grid(. ~ stand) +
  coord_equal()

# k-zone path
topKzone = 3.5
botKzone = 1.6
inKzone = -.95
outKzone = 0.95
kZone = data.frame(
  x = c(inKzone, inKzone, outKzone, outKzone, inKzone)
  , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

# throws error (shape and col expected everywhere)
ggplot(data=kershaw, aes(x=px, y=pz, shape=type, col=pitch_type)) +
  geom_point() +
  facet_grid(. ~ stand) +
  coord_equal() +
  geom_path(aes(x, y), data = kZone)

# move aesthetics out of main ggplot call
ggplot() +
  geom_point(data=kershaw, aes(x=px, y=pz, shape=type, col=pitch_type)) +
  facet_grid(. ~ stand) +
  coord_equal() +
  geom_path(aes(x, y), data = kZone)


####################################
# working with labels, legends, etc.

p0 = ggplot() +
  geom_point(data=kershaw, aes(x=px, y=pz, shape=type, col=pitch_type)) +
  facet_grid(. ~ stand) +
  coord_equal() +
  geom_path(aes(x, y), data = kZone)

# axis labels
p1 = p0 +
  xlab("horizontal location\n(ft. from center of the plate)") +
  ylab("vertical location\n(ft. from ground)")
p1

# legend names, symbols, labels
p2 = p1 +
  scale_color_manual(name="pitch type"
                     , values=c("CU"="blue", "FF"="red", "SL"="black")
                     , labels=c("CU"="Curveball", "FF"="4-seam Fastball", "SL"="Slider"))
p2

p3 = p2 +
  scale_shape_manual(name="outcome"
                     , values=c("B"=0, "S"=1, "X"=4)
                     , labels=c("B"="ball", "S"="strike", "X"="ball in play"))
p3

# add strikezone line to the legend
p0 = ggplot() +
  geom_point(data=kershaw, aes(x=px, y=pz, shape=type, col=pitch_type)) +
  facet_grid(. ~ stand) +
  coord_equal() +
  geom_path(aes(x, y, linetype="kzone"), data = kZone)
#...rerun p1, p2, p3 calls above
p4 = p3 + scale_linetype_manual(name=""
                                 , values=c("kzone"=2)
                                 , labels=c("kzone"="rulebook K-zone"))
p4

# change facet labels
# (awkward if you don't want to mess with the dataframe)
opp_hand = list("L" = "vs. LHB", "R" = "vs. RHB")
opp_hand_labeller = function(variable,value){
  return(opp_hand[value])
}

p0 = ggplot() +
  geom_point(data=kershaw, aes(x=px, y=pz, shape=type, col=pitch_type)) +
  facet_grid(. ~ stand, labeller=opp_hand_labeller) +
  coord_equal() +
  geom_path(aes(x, y, linetype="kzone"), data = kZone)
#...rerun p1 to p4 above
p4