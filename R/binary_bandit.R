BinaryBandit <- R6::R6Class(
  public = list(
    K = NULL,

    # TODO : regret
    arms = NULL,
    best_arm = NULL,
    means = NULL,
    best_mean = NULL,


    policies = NULL,

    initialize = function(){
      # TODO : run cheks
      # 1 : list arms cannot be empty
      # 2 : arms have to be binary
      # 3 : arms have to be of equal dataPoints ? Brainstorm error handling
      self$K <- 0
      self$arms <- list()
      self$policies <- list()
      invisible(self)
    },

    add_thompson_sampling_policy = function(){
      self$policies <- c(self$policies,
                            ThompsonSamplingPolicy$new())
      invisible(self)
    },

    add_ucb_policy = function(alpha){
      self$policies <- c(self$policies,
                             UCBPolicy$new(alpha))
      invisible(self)
    },

    add_bernouilli_arm = function(p){
      new_arm <- BernouilliArm$new(p)
      self$arms <- c(self$arms,new_arm)
      self$K <- self$K + 1
      self$means <- c(self$means,new_arm$mean)
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
