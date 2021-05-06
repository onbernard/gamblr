#' R6 Class handling a set of stochastic arms
#'
#' @description A \code{\link{FMABandit}} object holds a set of of arms
#'   represented by their mean and generated reward matrix. It provides methods
#'   to run policies against these arms.
#'
#' @details A FMABandit object is instantiated using the horizon that the
#'   simulations will span. Arms are supplied via a corresponding reward vector
#'   through the \code{\link{add_arm}} method.
#'
#'   Policies can be simulated against the arms using
#'   \code{\link{run_ucb_policy}} or \code{\link{run_eps_greedy_policy}}.
#'
#'   Plot of cumulative regret can be generated using \code{\link{plot_regret}}
#' @export
FMABandit <- R6::R6Class(
  classname = "FMABandit",
  public = list(

    #' @field arms_means Numeric vector containing each arm's mean.
    arms_means = NULL,

    #' @field horizon Integer specifying the number of time steps to run.
    horizon = NULL,

    #' @field rewards Matrix containing arms rewards. Time step on rows, arms on
    #'columns.
    rewards = NULL,

    #' @field K Number of arms.
    K = NULL,


    #' @description Create a new BinaryBandit object.
    #' @param horizon Number of time steps. Determines height of reward matrix.
    #' @return A new `FMABandit` object.
    initialize = function(horizon=1000L){
      self$K <- 0
      self$horizon <- horizon
      invisible(self)
    },

    #' @description Adds an arm using its reward vector.
    #' @param R Rewards vector.
    add_arm = function(R){
      stopifnot(length(R)==self$horizon)
      R <- as.matrix(R)
      self$rewards <- cbind(self$rewards, R)
      self$arms_means<- c(self$arms_means, mean(R))
      self$K <- self$K + 1
      invisible(self)
    },

    #' @description Runs a set of UCB policies against the arms.
    #' @param alpha Numeric vector specifying UCB policies parameters.
    #' @param verbose Boolean specifying the verbose mode of simulation.
    #' @return A list of simulation result. An item of this list consist of a
    #'   policy name, the result of the simulation of that policy and the system
    #'   time the simulation took.
    run_ucb_policy = function(alpha, verbose=TRUE){
      list_of_results <- list()
      for(i in 1:length(alpha)){
        policy <- UCBPolicy$new(alpha[i])$reset(self$K)
        name <- policy$name
        time <- system.time({
          result <- policy$run(self$rewards, verbose)
        })
        list_of_results[[i]] <- c(list_of_results,
                                  list("name"=name, "result"=result, "time"=time))
      }
      return(list_of_results)
    },

    #' @description Runs a set of Epsilon Greedy policies against the arms.
    #' @param epsilon Numeric vector specifying Epsilon Greedy policies
    #'   parameters.
    #' @param verbose Boolean specifying the verbose mode of simulation.
    #' @return A list of simulation result. An item of this list consist of a
    #'   policy name, the result of the simulation of that policy and the system
    #'   time the simulation took.
    run_eps_greedy_policy = function(epsilon, verbose=TRUE){
      list_of_results <- list()
      for(i in 1:length(alpha)){
        policy <- EpsilonGreedyPolicy$new(epsilon[i])$reset(self$K)
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

    #' @description Plots the cumulative regret of a result.
    #' @param result A simulation as returned by a policy, i.e. the result field
    #'   of a run_ucb_policy output item.
    #' @return A plot with as its x axis the time steps and as its y axis the
    #'   cumulative regret.
    plot_regret = function(result){

      best_mean <- max(self$arms_means)

      cumulative_reward <- cumsum(result$reward)

      cumulative_regret <- best_mean * 1:self$horizon - cumulative_reward
      df <- tibble(t=1:self$horizon)
      df <- bind_cols(df, "cumulative_regret"=cumulative_regret)
      ggplot(data=df, aes(x=t, y=cumulative_regret)) +
        geom_step() +
        xlab("time step") +
        ylab("cumulative regret")
    }
  )
)
