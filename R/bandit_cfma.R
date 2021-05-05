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

    dim = NULL,
    K  = NULL, # dimensions[1]  arms on rows
    dim = NULL, # dimensions[2] # dimensions on columns
    horizon = NULL,  # dimensions[3] # dataPoints on depth


    policies = NULL,

    initialize = function(horizon, dt, rewards){
      self$K <- 0
      self$arms <- list()
      self$policies <- list()
      invisible(self)
    },



    add_arm_from_dataframe = function(df){
      new_arm <- BinaryArmFromDataFrame$new(df)
      self$arms <- c(self$arms,new_arm)
      self$K <- self$K + 1
      self$means <- c(self$means,new_arm$mean)
      invisible(self)
    },

    run_simulation = function(nDataPoints){
      useless_variable <- lapply(c(self$policies,self$policies),
                                 function(p)(p$reset(self$K)))

      list_of_results <- vector(mode = "list", length=length(self$policies))
      for(p in 1:length(self$policies)){
        list_of_results[[p]] <- self$policies[[p]]$run(self$arms,nDataPoints)
        names(list_of_results)[p] <-
          paste(c(self$policies[[p]]$name,
                  ":", p),collapse="",sep="")
      }
      return(list_of_results)
    }
  )
)
