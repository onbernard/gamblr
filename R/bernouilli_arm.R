BernouilliArm <- R6::R6Class(
  public = list(
    p = NULL,
    reward_history = NULL,
    mean = NULL,
    nDataPoints = NULL,

    initialize = function(p){
      self$p <- p
      self$mean <- p
      self$reward_history <- c()
      self$nDataPoints <- Inf
      invisible(self)
    },

    get_reward = function(t){
      r <- rbinom(1,1,self$p)
      self$reward_history[t] <- r
      return(r)
    }
  )
)
