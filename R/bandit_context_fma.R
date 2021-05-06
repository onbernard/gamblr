#' R6 Class representing a contextual bandit with finitely many actions.
#'
#' @description A \code{\link{ContextFMABandit}} object
#'
#' @details A FMABandit object is instantiated
#'
#'   Policies can be simulated against the arms using
#'
#'   Plot of cumulative regret can be generated using \code{\link{plot_regret}}
#' @export
ContextFMABandit <- R6::R6Class(
  public = list(

    K  = NULL, # Number of action per round, rows of dt
    dim = NULL, # dimension of context, columns of dt
    horizon = NULL,  # number of time steps, depth of dt

    dt = NULL,
    rewards = NULL,

    initialize = function(dt, rewards){
      self$K <- dim(dt)[1]
      self$dim <- dim(dt)[2]
      self$horizon <- dim(dt)[3]
      self$dt <- as.array(dt)
      self$rewards <- as.matrix(rewards)
      invisible(self)
    },

    run_linucb_policy = function(lambda, delta){
      policy <- LINUCBPolicy$new(dim, lambda, delta)$reset()
      policy$run(self$dt, self$reward)
    }
  )
)
