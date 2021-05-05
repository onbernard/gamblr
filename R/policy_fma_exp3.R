#' R6 Class representing the EXP3 policy.
#'
#' @description An \code{\link{Exp3Policy}} object implements the behaviour of
#'   an agent with a Exponential-weight policy for Exploration and Exploitation
#'   (EXP3). It is instantiated with the learning parameter eta. The method
#'   \code{\link{run}} processes a reward matrix corresponding to a binary
#'   stochastic with finitely many arms bandit.
#'
#' @details The EXP3 algorithm is used in adversarial binary stochastic bandits
#'   with finitely many arms problems.
#'
#'   When processing a reward matrix, a reward distribution is attributed to
#'   each arm and then sampled randomly at each round. The maximum of those samples
#'   determines which arm is played.
#'
#'   This class uses a \code{\link{Recorder}} object to export results.
#' @export
Exp3Policy = R6::R6Class(
  public = list(

    #' @field name Short string describing the instance.
    name = NULL,

    #' @field record R6 object of class Recorder used to export simulation
    #'   results.
    record = NULL,

    #' @field K Integer containing the number of arms.
    K = NULL,

    #' @field eta Learning rate.
    eta = NULL,

    #' @field S_hat Numeric vector holding distribution parameter of each arm.
    S_hat = NULL,

    #' @field P Numeric vector containing the sample of each arm.
    P = NULL,


    #' @description
    #' Create a new Exp3Policy object.
    #' @param eta Learning rate.
    #' @return A new `Exp3Policy` object.
    initialize = function(eta=0.05){
      self$eta <- eta
      self$name <- paste(c("exp3(",eta,")"), collapse="",  sep="")
      invisible(self)
    },

    #' @description
    #' Set or reset object attributes before starting a simulation.
    #' @param K Number of arms.
    reset = function(K){
      self$S_hat <- rep(0,K)
      self$P <- rep(0,K)
      self$K <- K
      self$record <- Recorder$new()
      invisible(self)
    },

    #' @description Samples all arms reward distributions.
    #' @return A numeric vector containing each arm sample.
    compute_sampling_distribution = function(){
      temp_var <- exp(self$eta * self$S_hat)

      temp_var /  sum(temp_var)
    },

    #' @description Chooses which arm to pull by sampling the arms reward
    #'   distribution then taking the max.
    #' @return The index of the chosen arm.
    get_action = function(){
      P <- self$compute_sampling_distribution()
      self$P <- P
      sample(1:self$K, size=1, replace=TRUE, prob=P)
    },

    #' @description Updates S_hat given reward r of arm a.
    #' @param r Reward.
    #' @param a Arm.
    update_with_reward = function(r,a){
      self$S_hat <- self$S_hat + 1
      self$S_hat[a] <- self$S_hat[a] - (1-r) / self$P[a]
      invisible(self)
    },

    #' @description Process a bandit problem represented by the reward matrix.
    #' @param rewards Reward matrix. Time on rows, arms on columns.
    #' @param verbose Logical. if FALSE, only action and reward history will be
    #'   returned. Default is TRUE.
    #' @return A tibble describing at each iteration, the distribution parameter
    #'   of each arm, the sample of each arm, the action taken and the reward
    #'   received. If verbose is FALSE, only the action and reward.
    run = function(rewards, verbose=TRUE){
      horizon <- nrow(rewards)
      K <- ncol(rewards)
      self$reset(K)

      for(t in 1:horizon){
        if(verbose){
          self$record$vector("S_hat",self$S_hat)
        }
        a <- self$get_action()
        if(verbose){
          self$record$vector("P", self$P)
        }
        r <- rewards[t,a]
        self$record$scalar("reward",r)
        self$record$scalar("action",a)
        self$update_with_reward(r,a)
      }
      if(verbose){
        pub <- self$record$publish(horizon,c("action","reward","S_hat", "P"))
      }
      else{
        pub <- self$record$publish(horizon,c("action","reward"))
      }
      pub$action <- as_factor(pub$action)
      return(pub)
    }
  )
)
