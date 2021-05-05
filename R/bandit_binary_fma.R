#' R6 Class handling a set of binary stochastic arms and compatible policies
#'
#' @description A BinaryBandit object holds a set of of binary arms represented
#'   by their mean and generated reward matrix. It provides methods to run
#'   policies against these arms.
#'
#' @details A BinaryBandit Object is instantiated using the horizon and the
#'   means of each arms.  Additional arms can be provided afterward using
#'   \code{\link{add_bernouilli_arm}}.
#'
#'   Policies can be simulated against the arms using
#'   \code{\link{run_ucb_policy}} or \code{\link{run_ts_policy}}.
#'
#'   Plot of cumulative regret can be generated using \code{\link{plot_regret}}
#' @examples
#' bandit <- BinaryBandit$new(horizon=1000L, arms_means=c(0.2,0.5))
#' ucbres <- bandit$run_ucb_policy(alpha=c(1,10),verbose=TRUE)
#' tsres <- bandit$run_ts_policy(verbose=TRUE)
#' bandit$plot_regret(ucb_res[[1]]$result)
#' @export
BinaryBandit <- R6::R6Class(
  # inherit bandit_fma
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
    #' @param arms_means Vector specifying the arms means.
    #' @return A new `Binary Bandit` object.
    initialize = function(horizon, arms_means){
      stopifnot(is_integer(horizon),
                is.numeric(arms_means))

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

    #' @description Adds an arm.
    #' @param p Reward expectation of the arm.
    add_bernouilli_arm = function(p){
      self$rewards <- cbind(self$rewards, rbinom(self$horizon, 1, p))
      self$arms_means<- c(self$arms_means, p)
      self$K <- self$K + 1
      invisible(self)
    },

    #' @description Re-generates the reward matrix using existing arms means.
    reset_rewards = function(){
      self$rewards <- c()
      for(i in 1:self$K){
        self$rewards <- cbind(self$rewards,
                              rbinom(self$horizon, 1, self$arms_means[i]))
      }
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
