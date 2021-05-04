EpsilonGreedyPolicy <- R6::R6Class(
  public = list(
    epsilon = NULL,
    mu_hat =NULL,
    N = NULL,
    K = NULL,
    record = NULL,

    initialize = function(epsilon=0.25){
      self$epsilon <- epsilon
      invisible(self)
    },

    reset = function(K){
      self$mu_hat <- rep(Inf,K)
      self$N <- rep(0,K)
      self$K <- K
      self$record <- Recorder$new()
      invisible(self)
    },

    get_action = function(){
      exploration <- rbinom(1,1,self$epsilon)
      if(exploration){
        sample(1:self$K,1)
      }
      else{
        which.max(self$mu_hat)
      }
    },

    update_with_reward = function(r, a){
      if(self$mu_hat[a] == Inf){
        self$mu_hat[a] <- r
      }
      else{
        self$mu_hat[a] <- (self$mu_hat[a] * self$N[a] + r) / (self$N[a]+1)
      }
      self$N[a] <- self$N[a] + 1
      invisible(self)
    },

    run = function(rewards){
      nDataPoints <- nrow(rewards)
      K <- ncol(rewards)
      self$reset(K)

      for(t in 1:nDataPoints){
        self$record$vector("mu_hat",self$mu_hat)
        a <- self$get_action()
        r <- rewards[t,a]
        self$record$scalar("reward",r)
        self$record$scalar("action",a)
        self$update_with_reward(r,a)
      }

      self$record$publish(nDataPoints,c("action","reward","mu_hat"))
    }
  )
)
