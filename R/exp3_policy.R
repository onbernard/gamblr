# Exponential-weight algorithm for exploration and exploitation
Exp3Policy = R6::R6Class(
  public = list(
    name = NULL,
    eta = NULL,
    S_hat = NULL,
    K = NULL,
    P = NULL,
    record = NULL,

    initialize = function(eta=0.05){
      self$eta <- eta
      self$name <- paste(c("exp3(",eta,")"), collapse="",  sep="")
      invisible(self)
    },

    reset = function(K){
      self$S_hat <- rep(0,K)
      self$P <- rep(0,K)
      self$K <- K
      self$record <- Recorder$new()
      invisible(self)
    },

    compute_sampling_distribution = function(){
      temp_var <- exp(self$eta * self$S_hat)

      temp_var /  sum(temp_var)
    },

    get_action = function(){
      P <- self$compute_sampling_distribution()
      self$P <- P
      sample(1:self$K, size=1, replace=TRUE, prob=P)
    },

    update_with_reward = function(r,a){
      self$S_hat <- self$S_hat + 1
      self$S_hat[a] <- self$S_hat[a] - (1-r) / self$P[a]
      invisible(self)
    },

    run = function(rewards){
      nDataPoints <- nrow(rewards)
      K <- ncol(rewards)
      self$reset(K)

      for(t in 1:nDataPoints){
        self$record$vector("S_hat",self$S_hat)
        a <- self$get_action()
        self$record$vector("P", self$P)
        r <- rewards[t,a]
        self$record$scalar("reward",r)
        self$record$scalar("action",a)
        self$update_with_reward(r,a)
      }

      self$record$publish(nDataPoints,c("action","reward","S_hat", "P"))
    }
  )
)
