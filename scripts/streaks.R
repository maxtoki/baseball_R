streaks=function(y){
  n = length(y)
  where = c(0, y, 0) == 0
  location.zeros = (0:(n+1))[where]
  streak.lengths = diff(location.zeros) - 1
  streak.lengths[streak.lengths > 0]
}