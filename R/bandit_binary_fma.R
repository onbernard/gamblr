#' R6 Class handling a set of binary stochastic arms
#'
#' @description A \code{\link{BinaryFMABandit}} object holds a set of of binary
#'   arms represented by their mean and generated reward matrix. It provides
#'   methods to run policies against these arms.
#'
#' @details A BinaryFMABandit object is instantiated using the horizon and the
#'   means of each arms.  Additional arms can be provided afterward using
#'   \code{\link{add_bernouilli_arm}}.
#'
#'   Policies can be simulated against the arms using
#'   \code{\link{run_ucb_policy}} or \code{\link{run_ts_policy}}.
#'
#'   Plot of cumulative regret can be generated using \code{\link{plot_regret}}
#' @examples
#' bandit <- BinaryFMABandit$new(horizon=1000L, arms_means=c(0.2,0.5))
#' ucbres <- bandit$run_ucb_policy(alpha=c(1,10),verbose=TRUE)
#' tsres <- bandit$run_ts_policy(verbose=TRUE)
#' bandit$plot_regret(ucb_res[[1]]$result)
#' @export
BinaryFMABandit <- R6::R6Class(
  inherit = FMABandit,
  public = list(


    #' @description Create a new BinaryFMABandit object.
    #' @param horizon Number of time steps. Determines height of reward matrix.
    #' @param arms_means Vector specifying the arms means.
    #' @return A new `BinaryFMABandit` object.
    initialize = function(horizon, arms_means){
      super$initialize(horizon)
      stopifnot(is.numeric(arms_means))

      self$arms_means <- arms_means
      self$horizon <- horizon

      self$rewards <- c()
      for(i in 1:length(arms_means)){
        self$rewards <- cbind(self$rewards,
                              rbinom(horizon, 1, arms_means[i]))
      }
      self$K <- length(self$arms_means)
      invisible(self)
    },


    #' @description Adds an arm using its reward vector.
    #' @param R Rewards vector.
    add_arm = function(R){
      stopifnot(all(unlist(lapply(R, function(x)(x==0 || x==1)))))

      R <- as.matrix(R)
      self$rewards <- cbind(self$rewards, R)
      self$arms_means<- c(self$arms_means, mean(R))
      self$K <- self$K + 1
      invisible(self)
    },


    #' @description Adds a binary arm according to a bernouilli distribution.
    #' @param p Mean of bernouilli distribution.
    add_bernouilli_arm = function(p){
      self$rewards <- cbind(self$rewards, rbinom(self$horizon, 1, p))
      self$arms_means<- c(self$arms_means, p)
      self$K <- self$K + 1
      invisible(self)
    },


    #' @description Re-generate reward matrix according to the existing means.
    reset_rewards = function(){
      self$rewards <- c()
      for(i in 1:self$K){
        self$rewards <- cbind(self$rewards,
                              rbinom(self$horizon, 1, self$arms_means[i]))
      }
    },


    #' @description Runs a set of Exp3 policies against the arms.
    #' @param eta Numeric vector specifying exp3 policies parameters.
    #' @param verbose Boolean specifying the verbose mode of simulation.
    #' @return A list of simulation result. An item of this list consist of a
    #'   policy name, the result of the simulation of that policy and the system
    #'   time the simulation took.
    run_exp3_policy = function(eta=0.05, verbose=TRUE){
      list_of_results <- list()
      for(i in 1:length(eta)){
        policy <- Exp3Policy$new(eta[i])$reset(self$K)
        name <- policy$name
        time <- system.time({
          result <- policy$run(self$rewards, verbose)
        })
        list_of_results[[i]] <- c(list_of_results,
                                  list("name"=name,
                                       "result"=result,
                                       "time"=time))
      }
      return(list_of_results)
    },

    #' @description Runs a Thompson Sampling policy against the arms.
    #' @param verbose Boolean specifying the verbose mode of simulation.
    #' @return A list containing the policy name, the result of the simulation
    #'   and the system time the simulation took.
    run_ts_policy = function(verbose=TRUE){
      policy <- ThompsonSamplingPolicy$new()$reset(self$K)
      name <- policy$name
      time <- system.time({
        result <- policy$run(self$rewards, verbose)
      })

      return(list("name"=name, "result"=result, "time"=time))
    }

  )
)
