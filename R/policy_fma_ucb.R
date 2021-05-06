#' R6 Class representing the Finitely Many Arm Upper Confidence Bound policy
#'
#' @description An \code{\link{UCBPolicy}} object is instantiated using
#'   exploration parameter alpha. The method \code{\link{run}} processes a
#'   reward matrix corresponding to a stochastic with finitely many arms bandit.
#'
#' @details The Upper Confidence Bound Algorithm is used in stochastic bandits
#'   with finitely many arms problems. Like other bandit algorithms, arms are
#'   chosen according to their indices computed at each round. Here this index
#'   is the upper confidence bound parameterized by the constant alpha.
#'
#'   When processing a bandit problem,iterations parameters mu_hat, N and K are
#'   first reset then updated at each iteration corresponding to the reward
#'   observed.
#'
#'   This class uses a \code{\link{Recorder}} object to export results.
#' @export
UCBPolicy <- R6::R6Class(
  public = list(

    #' @field name Short string describing the instance.
    name = NULL,

    #' @field record R6 object of class Recorder used to export simulation
    #'   results.
    record = NULL,

    #' @field alpha Numeric scalar containing UCB parameter.
    alpha = NULL,

    #' @field mu_hat Numeric vector containing reward expectation of each arm.
    mu_hat = NULL,

    #' @field N Numeric vector containing number of times each arm was pulled.
    N = NULL,

    #' @field K Integer containing the number of arms.
    K = NULL,


    #' @description
    #' Create a new UCBPolicy object.
    #' @param alpha Exploration parameter.
    #' @return A new `UCBPolicy` object.
    initialize = function(alpha=1){
      self$alpha <- alpha
      self$name <- paste(c("ucb(a=",alpha,")"), collapse="",  sep="")
      invisible(self)
    },

    #' @description
    #' Set or reset object attributes before starting a simulation.
    #' @param K Number of arms.
    reset = function(K){
      self$mu_hat <- rep(Inf,K)
      self$N <- rep(0,K)
      self$K <- K
      self$record <- Recorder$new()
      invisible(self)
    },

    #' @description Computes the upper confidence bound of arm a at time t.
    #' @param t Time. Integer.
    #' @param a Arm. Integer.
    #' @return The upper confidence bound of arm a at time t.
    compute_UCB_for_an_arm = function(t, a){
      if(self$N[a]==0){
        Inf
      }
      else{
        self$mu_hat[a] + self$alpha * sqrt( (2*log(t) / self$N[a]) )
      }
    },

    #' @description Computes the upper confidence bound of all arms at time t.
    #' @param t Time. Integer.
    #' @return Vector containing the upper confidence bound of all arms.
    compute_UCB_for_all_arms = function(t){
      UCB <- c()
      for(a in 1:self$K){
        UCB[a] <- self$compute_UCB_for_an_arm(t, a)
      }
      return(UCB)
    },

    #' @description Updates mu_hat and N given reward r of arm a.
    #' @param r Reward.
    #' @param a Arm.
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

    #' @description Process a bandit problem represented by the reward matrix.
    #' @param rewards Reward matrix. Time on rows, arms on columns.
    #' @param verbose Logical. if FALSE, only action and reward history will be
    #'   returned. Default is TRUE.
    #' @return A tibble describing at each iteration, the UCB value and reward
    #'   expectation of each arm, the action taken and the reward received. If
    #'   verbose is FALSE, only the action and reward.
    run = function(rewards, verbose=TRUE){
      # TODO : change function to handle NA
      K <- ncol(rewards)
      nDataPoints <- nrow(rewards)
      self$reset(K)

      for(t in 1:nDataPoints){
        ucb <- self$compute_UCB_for_all_arms(t)
        a <- which.max(ucb)
        r <- rewards[t,a]
        if(verbose){
          self$record$vector("mu_hat",self$mu_hat)
          self$record$vector("ucb", ucb)
        }
        self$update_with_reward(r,a)

        self$record$scalar("action", a)
        self$record$scalar("reward", r)
        if(verbose){

        }
      }
      if(verbose){
        pub <- self$record$publish(nDataPoints,
                                   c("action","reward","ucb", "mu_hat"))
      }
      else{
        pub <- self$record$publish(nDataPoints,c("action","reward"))
      }
      pub$action <- as.factor(pub$action)
      return(pub)
    }

  )
)
