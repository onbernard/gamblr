BinaryArmFromDataFrame <- R6::R6Class(
  public = list(
    mean = NULL,
    visitor_rewards = NULL,
    nDataPoints = NULL,

    initialize = function(visitor_rewards){
      self$visitor_rewards <- as.matrix(visitor_rewards)
      self$mean <- mean(self$visitor_rewards)
      self$nDataPoints <- nrow(self$visitor_rewards)
      invisible(self)
    },

    get_reward = function(t){
      self$visitor_rewards[t]
    }
  )
)
