one.simulation.68 <- function(s.talent = 0.20){

  make.schedule <- function(teams, k){
    n.teams <- length(teams)
    Home <- rep(gl(n.teams, n.teams, length=n.teams ^ 2, labels=teams), k)
    Visitor <- rep(gl(n.teams, 1, length=n.teams ^ 2, labels=teams), k)
    schedule <- data.frame(Home = Home, Visitor = Visitor)
    subset(schedule, Home != Visitor)
  }
  
  NL <- c("ATL", "CHN", "CIN", "HOU", "LAN", "NYN", "PHI", "PIT", "SFN", "SLN")
  AL <- c("BAL", "BOS", "CAL", "CHA", "CLE", "DET", "MIN", "NYA", "OAK", "WS2")
  teams <- c(NL, AL)
  league <- c(rep(1, 10), rep(2, 10))
  
  schedule <- rbind(make.schedule(NL, 9), 
                   make.schedule(AL, 9))

  # simulate talents 
  talents <- rnorm(20, 0, s.talent)  
  TAL <- data.frame(Team = teams, League = league, Talent = talents)

  # merge talents and win probs with schedule data frame
  SCH <- merge(schedule, TAL, by.x = "Home", by.y = "Team")
  names(SCH)[4] <- "Talent.Home"
  SCH <- merge(SCH, TAL, by.x = "Visitor", by.y = "Team")
  names(SCH)[6] <- "Talent.Visitor"
  SCH$prob.Home <- with(SCH,
          exp(Talent.Home) / (exp(Talent.Home) + exp(Talent.Visitor)))

  # play season of games
  SCH$outcome <- with(SCH, rbinom(nrow(SCH), 1, prob.Home)) 
  SCH$winner <- with(SCH, ifelse(outcome, as.character(Home), 
                   as.character(Visitor)))

  # compute number of games won for all teams
  wins <- table(SCH$winner)
  WIN <- data.frame(Team = names(wins), Wins = as.numeric(wins))

  RESULTS <- merge(TAL, WIN)

  win.league <- function(RR, league){
    wins <- RR$Wins * (RR$League == league)
    MAX <- max(wins)  
    if(sum(wins == MAX) > 1){
      prob <- exp(RR$Talent) * (wins == MAX)
      outcome <- c(rmultinom(1, 1, prob))
      RR$Winner.Lg <- RR$Winner.Lg + outcome
      }
  
    if(sum(wins == MAX) == 1){
    RR$Winner.Lg <- RR$Winner.Lg + as.numeric(wins == MAX)}
    RR
  }

# record if eligible for wild card (Wild), in playoffs (Playoff)
# in conference playoff (CS), World Series (WS), or winner (B)

  RESULTS$Winner.Lg <- 0; RESULTS$Winner.WS <- 0;  

  # figure out winners of each league
  for(j in 1:2)
    RESULTS <- win.league(RESULTS, j)

# play World Series

  teams <- (1:20)[RESULTS$Winner.Lg == 1]
  outcome <- c(rmultinom(1, 7, exp(RESULTS$Talent)[teams]))
  winner <- teams[1] * (diff(outcome) < 0) + teams[2] * (diff(outcome) > 0)
  RESULTS$Winner.WS[winner] <- 1

  # data frame has teams, division, talent, wins, and diff playoff results
  RESULTS
}

