#' R6 Class representing the Finitely Many Arm Thompson Sampling policy
#'
#' @description A \code{\link{ThompsonSamplingPolicy}} object implements the
#'   behaviour of an agent with a Thompson Sampling Policy. It is instantiated
#'   without arguments. The method \code{\link{run}} processes a reward matrix
#'   corresponding to a binary stochastic with finitely many arms bandit.
#'
#' @details The Thompson Sampling Algorithm is used in binary stochastic bandits
#'   with finitely many arms problems.
#'
#'   When processing a reward matrix, a reward distribution is attributed to
#'   each arm and then sampled randomly at each round. Alpha and beta attributes
#'   stores those distributions parameters (Not to be confused with the UCB
#'   parameter alpha !). They are reset at the beginning of the simulation then
#'   updated depending on the observed reward.
#'
#'   The beauty of Thompson Sampling is that prior and posterior distribution
#'   are both beta distributions when rewards are binary, simplifying the
#'   computations.
#'
#'   This class uses a \code{\link{Recorder}} object to export results.
#' @export
ThompsonSamplingPolicy <- R6::R6Class(
  public = list(

    #' @field name Short string describing the instance.
    name = NULL,

    #' @field record R6 object of class Recorder used to export simulation
    #'   results.
    record = NULL,

    #' @field K Integer containing the number of arms.
    K = NULL,

    #' @field beta Numeric vector containing the beta parameter of each arm's
    #'   distribution.
    beta = NULL,

    #' @field alpha Numeric vector containing the alpha parameter of each arm's
    #' distribution.
    alpha = NULL,


    #' @description
    #' Creates a new ThompsonSamplingPolicy object.
    #' @return A new `ThompsonSamplingObject` object.
    initialize = function(){
      self$name <- "thsamp"
      invisible(self)
    },

    #' @description
    #' Set or reset object attributes before starting a simulation.
    #' @param K Number of arms.
    reset = function(K){
      self$K <- K
      self$record <- Recorder$new()
      self$alpha <- rep(1,K)
      self$beta <- rep(1,K)
      invisible(self)
    },

    #' @description Samples an arm reward distributions.
    #' @param a the arm index.
    #' @return A number between 0 and 1.
    get_sample_of_an_arm = function(a){
      rbeta(1,self$alpha[a],self$beta[a])
    },

    #' @description Samples all arm reward distributions.
    #' @return A numeric vector containing each arm sample.
    get_sample_of_all_arms = function(){
      unlist(lapply(1:self$K, self$get_sample_of_an_arm))
    },

    #' @description Computes an arm reward expectation according to its
    #'   distribution.
    #' @param a the arm index.
    #' @return The arm reward expectation.
    get_mean_of_an_arm = function(a){
      self$alpha[a] / (self$alpha[a] + self$beta[a])
    },

    #' @description Computes all arm reward expectation according to their
    #'   distributions.
    #' @return A numeric vector containing each arm reward expectation.
    get_mean_of_all_arms = function(){
      unlist(lapply(1:self$K, self$get_mean_of_an_arm))
    },

    #' @description Chooses which arm to pull by sampling the arms reward
    #'   distribution then taking the max.
    #' @return The index of the chosen arm.
    get_action = function(){
      which.max(self$get_sample_of_all_arms())
    },

    #' @description Updates alpha and beta given reward r of arm a.
    #' @param r Reward.
    #' @param a Arm.
    update_with_reward = function(r,a){
      self$alpha[a] <- self$alpha[a] + r
      self$beta[a] <- self$beta[a] + 1 - r
      self$record$scalar("reward",r)
      self$record$scalar("action",a)
      invisible(self)
    },

    #' @description Process a bandit problem represented by the reward matrix.
    #' @param rewards Reward matrix. Time on rows, arms on columns.
    #' @param verbose Logical. if FALSE, only action and reward history will be
    #'   returned. Default is TRUE.
    #' @return A tibble describing at each iteration, the means of each arm,
    #'   the action taken and the reward received. If verbose is FALSE, only the
    #'   action and reward.
    run = function(rewards, verbose=TRUE){
      K <- ncol(rewards)
      horizon <- nrow(rewards)
      self$reset(K)

      for(t in 1:horizon){
        if(verbose){
          self$record$vector("mu_hat", self$get_mean_of_all_arms())
        }

        a <- self$get_action()
        r <- rewards[t,a]
        self$update_with_reward(r,a)
      }
      if(verbose){
        pub <- self$record$publish(horizon,c("action","reward","mu_hat"))
      }
      else{
        pub <- self$record$publish(horizon,c("action","reward"))
      }
      pub$action <- as.fa(pub$action)
      return(pub)
    }
  )
)
