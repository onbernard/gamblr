ThompsonSamplingPolicy <- R6::R6Class(
  #inherit = binaryPolicy || bernouilli bandit ???
  public = list(
    K = NULL,
    name = NULL,
    alpha = NULL,
    beta = NULL,
    record = NULL,

    initialize = function(){
      self$name <- "thsamp"
      invisible(self)
    },

    reset = function(K){
      self$K <- K
      self$record <- Recorder$new()
      self$alpha <- rep(1,K)
      self$beta <- rep(1,K)
      invisible(self)
    },

    get_sample_of_an_arm = function(a){
      rbeta(1,self$alpha[a],self$beta[a])
    },

    get_sample_of_all_arms = function(){
      unlist(lapply(1:self$K, self$get_sample_of_an_arm))
    },

    get_mean_of_an_arm = function(a){
      self$alpha[a] / (self$alpha[a] + self$beta[a])
    },

    get_mean_of_all_arms = function(){
      unlist(lapply(1:self$K, self$get_mean_of_an_arm))
    },

    get_action = function(){
      # TODO : change function to take a set of available actions as parameters
      # add a default behaviour
      which.max(self$get_sample_of_all_arms())
    },

    update_with_reward = function(r,a){
      self$alpha[a] <- self$alpha[a] + r
      self$beta[a] <- self$beta[a] + 1 - r
      self$record$scalar("reward",r)
      self$record$scalar("action",a)
      invisible(self)
    },

    run = function(arms, nDataPoints){
      # TODO : change function to handle NA
      self$reset(length(arms))

      for(t in 1:nDataPoints){
        self$record$vector("mu_hat", self$get_mean_of_all_arms())
        a <- self$get_action()
        r <- arms[[a]]$get_reward(t)
        self$update_with_reward(r,a)
      }
      self$record$publish(nDataPoints,c("action","reward","mu_hat"))
    }
  )
)
