#' R6 Class representing the Finitely Many Arm Epsilon Greedy policy
#'
#' @description An \code{\link{EpsilonGreedyPolicy}} object implements the
#'   behaviour of an agent with a Epsilon Greedy Policy. It is instantiated the
#'   exploration parameter epsilon. The method \code{\link{run}} processes a
#'   reward matrix corresponding to a stochastic with finitely many arms bandit.
#'
#' @details The Epsilon Greedy Algorithm is used in stochastic bandits with
#'   finitely many arms problems.
#'
#'   When processing a reward matrix, at each iteration the agent flip a coin
#'   with success probability epsilon. If it succeeds he chooses at random an
#'   arm to pull. If not he chooses the arm with the highest observed reward
#'   expectation. This expectation if updated after observing the result of the
#'   draw.
#'
#'   This class uses a \code{\link{Recorder}} object to export results.
#' @export
EpsilonGreedyPolicy <- R6::R6Class(
  public = list(

    #' @field name Short string describing the instance.
    name = NULL,

    #' @field record R6 object of class Recorder used to export simulation
    #'   results.
    record = NULL,

    #' @field epsilon Exploration parameter
    epsilon = NULL,

    #' @field mu_hat Numeric vector containing reward expectation of each arm.
    mu_hat = NULL,

    #' @field N Numeric vector containing number of times each arm was pulled.
    N = NULL,

    #' @field K Integer containing the number of arms.
    K = NULL,


    #' @description Create a new EpsilonGreedyPolicy object.
    #' @param epsilon Exploration parameter.
    #' @return A new `EpsilonGreedyPolicy` object.
    initialize = function(epsilon=0.25){
      self$epsilon <- epsilon
      self$name <- paste(c("epsg(e=",epsilon,")"), collapse="",  sep="")
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

    #' @description Chooses which arm to pull by flipping a coin and either
    #'   explore or exploit the current best arm.
    #' @return The index of the chosen arm.
    get_action = function(){
      explore <- rbinom(1,1,self$epsilon)
      self$record$scalar("explore", explore)
      if(explore){
        sample(1:self$K,1)
      }
      else{
        which.max(self$mu_hat)
      }
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
    #' @param verbose Logical. if FALSE, only the coin flip value, the action
    #'   and reward history will be returned. Default is TRUE.
    #' @return A tibble describing at each iteration, the reward expectation of
    #'   each arm, the coin flip value, the action taken and the reward
    #'   received. If verbose is FALSE, only the coin flip, the action and the
    #'   reward.
    run = function(rewards, verbose=TRUE){
      horizon <- nrow(rewards)
      K <- ncol(rewards)
      self$reset(K)

      for(t in 1:horizon){
        if(verbose){
          self$record$vector("mu_hat",self$mu_hat)
        }
        a <- self$get_action()
        r <- rewards[t,a]
        self$record$scalar("reward",r)
        self$record$scalar("action",a)
        self$update_with_reward(r,a)
      }
      if(verbose){
        pub <- self$record$publish(horizon,c(
          "explore",
          "action",
          "reward",
          "mu_hat"))
      }
      else{
        pub <- self$record$publish(horizon,c(
          "explore",
          "action",
          "reward"))
      }
      pub$action <- as.factor(pub$action)
      return(pub)
    }
  )
)
