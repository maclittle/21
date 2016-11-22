whole.deck <- c(2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10)
cards.left <- whole.deck

risk <- 0.5
risk.list <- c()
trust <- 0.5
trust.list <- c()
hand <- 0
total.score <- 0
stop <- FALSE
fails <- 0
stop.list <- c()

initial.hand <- function(){
  start.ids <- sample(1:36, 2, replace= FALSE)
  start.hand <- cards.left[start.ids]
  cards.left <<- cards.left[-start.ids]
  hand <<- sum(start.hand)
}

draw <- function() {
  next.id <- sample(1:length(cards.left), 1, replace= FALSE)
  hand <<- hand + cards.left[next.id]
  cards.left <<- cards.left[-next.id]
}

play.round <- function() {
  initial.hand()
  while((hand<21) && (stop==FALSE)) {
    #if hand is less than 11 draw another card
    if (hand<11){
      draw()
    }
    #gen num from risk factor. if 1, draw. if not, stop==TRUE.
    risk.huh <- rbinom(1, 1, risk)
    if (risk.huh == 1){
      draw()
    }
    if (risk.huh == 0){
      stop <<- TRUE
    }
  }
  if((stop == TRUE)&& (hand<22)){
    total.score <<- total.score + hand
    stop.list <<- c(stop.list, hand)
    risk <<- risk + rnorm(1, mean=.03, sd = 0.005)
    if(risk>1){
      risk <<- 1
    }
  }
  if(hand>21){
    fails <<- fails + 1
    risk <<- risk - rnorm(1, mean=.1, sd = 0.05)
    if(risk<0){
      risk <<- 0
    }
  }
  hand <<- 0
  cards.left <<- whole.deck
  stop <<- FALSE
  risk.list <<- c(risk.list, risk)
}

#play a bunch of rounds and return final score, fails, and avg stop value from stop list
run.bunch <- function(n){
  for (i in 1:n){
    play.round()
  }
  print("risk:")
  print(risk)
  print("number of fails: ")
  print(fails)
  print("total score:")
  print(total.score)
  print("average stopping point of success:")
  print(mean(stop.list))
}

run.bunch(100)
plot(1:100, risk.list)
