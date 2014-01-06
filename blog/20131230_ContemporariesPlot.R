# Graphically compare pitchers to contemporaries

# Reference: http://baseballwithr.wordpress.com/2013/12/30/graphically-compare-pitchers-to-contemporaries/

# Need 2013 Lahman's database (http://www.seanlahman.com/baseball-archive/statistics/)


#################
options(stringsAsFactors=F)
#set working dir
setwd("your/directory/containing/Lahman/DB")

#read data
master = read.csv("Master.csv")
pitching = read.csv("Pitching.csv")

# function
hofChart = function(pitcher, stat){
  require(doBy)
  require(ggplot2)
  
  # season totals by pitcher
  pitching = summaryBy(ER + IPouts + SO + BB ~ playerID + yearID
                        , data=pitching, FUN=sum, keep.names=T)

  # calculate stats (you can add your own too, e.g.: HR/9, FIP, ...)
  pitching$ERA = pitching$ER * 27 / pitching$IPouts
  pitching$K9 = pitching$SO * 27 / pitching$IPouts
  pitching$W9 = pitching$BB * 27 / pitching$IPouts
  
  # get selected pitcher's data
  pitID = subset(master, paste(nameFirst, nameLast)==pitcher)$playerID
  pitData = subset(pitching, playerID==pitID)
  
  # get contemporaries of selected pitcher (qualifying only)
  contemporaries = subset(pitching
                           , yearID >= min(pitData$yearID) 
                           & yearID <= max(pitData$yearID)
                           & IPouts >= 162*3)

  # compare stat with contemporaries
  ggplot(data=pitData, aes_string(x="factor(yearID)", y=stat)) +
    geom_boxplot(data=contemporaries, aes_string(x="factor(yearID)", y=stat)) +
    geom_point(data=contemporaries, aes_string(x="factor(yearID)", y=stat, col="'oth'", shape="'oth'", size="'oth'"), position=position_jitter(width = 0.15), alpha=.6) +
    geom_point(aes(col="sel", shape="sel", size="sel")) +
    xlab("season") +
    ggtitle(paste(pitcher, " vs his contemporaries (", stat, ")", sep="")) +
    scale_color_manual(values=c("oth"="black", "sel"="blue")
                       , labels=c("oth"="contemporaries", "sel"=pitcher)
                       , name="") +
    scale_shape_manual(values=c("oth"=1, "sel"=19)
                       , labels=c("oth"="contemporaries", "sel"=pitcher)
                       , name="") +
    scale_size_manual(values=c("oth"=2, "sel"=5)
                      , labels=c("oth"="contemporaries", "sel"=pitcher)
                      , name="")
}

hofChart("Roger Clemens", "ERA")
hofChart("Roger Clemens", "K9")
hofChart("Roger Clemens", "W9")

hofChart("Greg Maddux", "ERA")
hofChart("Greg Maddux", "K9")
hofChart("Greg Maddux", "W9")