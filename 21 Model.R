library(ggplot2)
library(DEoptim)

# Initialize the whole deck of cards
whole.deck <- c(2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10)
cards.left <- whole.deck

risk <- 0.5
risk.list <- c()
trust <- 0.1
trust.list <- c()
agree.count <- 0
hand <- 0
total.score <- 0
stop <- FALSE
fails <- 0
stop.list <- c()
experience.trust <- 0.03
experience.level <- 0

# Draws the initial hand of 2 cards
initial.hand <- function(){
  start.ids <- sample(1:36, 2, replace= FALSE)
  start.hand <- cards.left[start.ids]
  cards.left <<- cards.left[-start.ids]
  hand <<- sum(start.hand)
}

# Draws one card from the deck and adds it to your hand
draw <- function() {
  next.id <- sample(1:length(cards.left), 1, replace= FALSE)
  hand <<- hand + cards.left[next.id]
  cards.left <<- cards.left[-next.id]
}

# Plays a single round of 21
play.round <- function() {
  initial.hand()
  advice.taken <- FALSE
  # While hand is not over 21 and you haven't stopped
  while((hand<21) && (stop==FALSE)) {
    # If hand is less than 11 draw another card, unless...
    if (hand<11){
      # If the other agent tells you to stop even though it's safe, and you agree, stop
      bad.advice <- rbinom(1, 1, 0.1)
      if(trust>1){
        trust <<- 1
      }
      if(trust<0){
        trust <<- 0
      }
      agree <- rbinom(1, 1, trust)
      if((bad.advice== 1)&&(agree==1)){
        stop <<- TRUE
        agree.count <<- agree.count + 1
        advice.taken <- TRUE
      }
      # Otherwise draw another card
      if((bad.advice==0) || (agree==0)){
        draw()
      }
    }
    # Generate a number based on the risk factor. 1 means draw, 0 means stop
    if(risk>1){
      risk <<- 1
    }
    if(risk<0){
      risk <<- 0
    }
    risk.huh <- rbinom(1, 1, risk)
    # Predicts whether it's safe to draw or not
    good.advice <- rbinom(1, 1, 0.3)
    # If you agree to draw, draw
    if ((risk.huh == 1)&&(good.advice==1)){
      draw()
    }
    # If you agree to stop, stop
    if ((risk.huh == 0)&&(good.advice==0)){
      stop <<- TRUE
    }
    # If you want to draw and it says stop:
    if((risk.huh==1)&&(good.advice==0)){
      if(trust>1){
        trust <<- 1
      }
      if(trust<0){
        trust <<- 0
      }
      agree <- rbinom(1, 1, trust)
      if(agree==1){
        stop <<- TRUE
        agree.count <<- agree.count + 1
        advice.taken <- TRUE
      }
      if(agree==0){
        draw()
      }
    }
    # If you want to stop and it says draw:
    if((risk.huh==0)&&(good.advice==1)){
      if(trust>1){
        trust <<- 1
      }
      if(trust<0){
        trust <<- 0
      }
      agree <- rbinom(1, 1, trust)
      if(agree==1){
        draw()
        agree.count <<- agree.count + 1
        advice.taken <- TRUE
      }
      if(agree==0){
        stop <<- TRUE
      }
    }
  }
  # If you stop and hand is below 22, add hand score to total score
  if((stop == TRUE)&& (hand<22)){
    total.score <<- total.score + hand
    stop.list <<- c(stop.list, hand)
    # The risk factor gets a little higher
    risk <<- risk + rnorm(1, mean=.03, sd = 0.005)
    if(risk>1){
      risk <<- 1
    }
    if(advice.taken==TRUE){
      trust <<- trust + rnorm(1, mean=experience.trust, sd = 0.005)
      if(trust>1){
        trust <<- 1
      }
    }
  }
  # If you exceed 21, no points are added to total score, and fail count increases
  if(hand>21){
    fails <<- fails + 1
    # Risk factor decreases
    risk <<- risk - rnorm(1, mean=.1, sd = 0.05)
    if(risk<0){
      risk <<- 0
    }
    if(advice.taken==TRUE){
      trust <<- trust - rnorm(1, mean=.1, sd = 0.05)
      if(trust<0){
        trust <<- 0
      }
    }
  }
  # Reset settings to a new round
  hand <<- 0
  cards.left <<- whole.deck
  stop <<- FALSE
  advice.taken <- FALSE
  # Keep track of the risk over time
  risk.list <<- c(risk.list, risk)
  trust.list <<- c(trust.list, trust)
}

experience.levels <- c()
risks <- c()
trusts <- c()
total.scores <- c()

# Play a bunch of rounds and return risk, fails, final score, and avg stop value from stop list
run.bunch <- function(n){
  # Determines whether participant is in high or low exerience condition
  experience.level <<- rbinom(1, 1, 0.5)
  # If they think the second agent has high experience, they trust the successes more and start with higher trust
  if(experience.level==1){
    experience.trust <<- 0.06
    trust <<- 0.3
  }
  # If they think the second agent has low experience, they trust the successes less and start with lower trust
  if(experience.level==0){
    experience.trust <<- 0.03
    trust <<- 0.1
  }
  for (i in 1:n){
    play.round()
  }
  #print("experience level: (1 is high, 0 is low)")
  #print(experience.level)
  #print("risk:")
  #print(risk)
  #print("number of fails: ")
  #print(fails)
  #print("total score:")
  #print(total.score)
  #print("average stopping point of success:")
  #print(mean(stop.list))
  
  experience.levels <<- c(experience.levels, experience.level)
  risks <<- c(risks, risk)
  trusts <<- c(trusts, trust)
  total.scores <<- c(total.scores, total.score)
}

# Run 300 rounds with 1 person
run.bunch(300)
experience.levels <<- c()
risks <<- c()
trusts <<- c()
total.scores <<- c()
scores.over.time <<- data.frame(risk.list, trust.list)
# See that person's risk factor over the 300 rounds with trust as a color
ggplot(scores.over.time, aes(x=1:300, y=risk.list, color=trust.list))+
  geom_line()
# Trust factor with risk as a color:
ggplot(scores.over.time, aes(x=1:300, y=trust.list, color=risk.list))+
  geom_line()


# generate data for a bunch of participants after 100 trials each (final risk, final trust, final score, category)
replicate(100, {run.bunch(50)})
# Generated data
gen.data <<- data.frame(experience.levels, risks, trusts, total.scores)

# see whether the model can predict which category the data comes from

rmse.from.models <- function(params) {
  risk <<- params[1]
  trust <<- params[2]
  new.data <- replicate(1000, {run.bunch(50)})
  
  rmse.squares <- (new.data-gen.data)^2
  rmse <- sqrt(mean(rmse.squares))
  return(rmse)
}

pars <- c(0.5,0.5)
optim.result <- optim(pars, rmse.from.models, method="Nelder-Mead")

deoptim.result <- DEoptim(rmse.from.models, c(0,0), c(1,1))
deoptim.result$par
deoptim.result$val