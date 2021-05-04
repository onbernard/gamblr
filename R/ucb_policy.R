UCBPolicy <- R6::R6Class(
  #inherit = Policy,
  public = list(
    name = NULL,
    alpha = NULL, # UCB policy parameter
    mu_hat = NULL, # vector with mean of each arms
    N = NULL, # vector with number of times each arm was played
    record = NULL,
    K = NULL, # number of arms

    initialize = function(alpha = 1){
      #super$initialize()
      self$alpha <- alpha
      self$name <- paste(c("ucb(",alpha,")"), collapse="",  sep="")
      invisible(self)
    },

    reset = function(K){
      self$mu_hat <- rep(Inf,K)
      self$N <- rep(0,K)
      self$K <- K
      self$record <- Recorder$new()
      invisible(self)
    },

    compute_UCB_for_an_arm = function(t, a){
      if(t<=1){
        Inf
      }
      else{
        self$mu_hat[a] + self$alpha * sqrt( 2*log(t) / self$N[a] )
      }
    },

    compute_UCB_for_all_arms = function(t){
      UCB <- c()
      for(a in 1:self$K){
        UCB[a] <- self$compute_UCB_for_an_arm(t, a)
      }
      return(UCB)
    },

    get_action = function(t){
      # TODO : change function to take a set of available actions as parameters
      # add a default behaviour
      UCB <- self$compute_UCB_for_all_arms(t)
      self$record$vector("ucb",UCB)
      self$record$vector("mu_hat",self$mu_hat)
      # TODO : redo sample but with replacement or maybe not, see sample
      which.max(UCB)
    },

    update_with_reward = function(r, a){
      if(self$mu_hat[a] == Inf){
        self$mu_hat[a] <- r
      }
      else{
        self$mu_hat[a] <- (self$mu_hat[a] * self$N[a] + r) / (self$N[a]+1)
      }
      self$N[a] <- self$N[a] + 1
      self$record$scalar("reward",r)
      self$record$scalar("action",a)
      invisible(self)
    },

    run = function(arms, nDataPoints){
      # TODO : change function to handle NA
      self$reset(length(arms))

      for(t in 1:nDataPoints){
        a <- self$get_action(t)
        r <- arms[[a]]$get_reward(t)
        self$update_with_reward(r,a)
      }
      self$record$publish(nDataPoints,c("action","reward","ucb","mu_hat"))
    }

  )
)
