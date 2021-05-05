# This shit will spy on policies
# or maybe call it evaluation or whatever
# record(sel)
Recorder <- R6::R6Class(
  public = list(
    history = NULL,

    initialize = function(){
      self$history <- list("t"=c())
      invisible(self)
    },

    #record$action_and_reward()
    scalar = function(name, value){
      self$history[[eval(name)]] <- c(self$history[[eval(name)]],value)
      invisible(self)
    },

    vector = function(name, value){
      self$history[[eval(name)]] <- rbind(self$history[[eval(name)]],value)
      invisible(self)
    },

    get = function(name){
      self$history[[eval(name)]]
    },

    publish = function(t,columns){
      self$history$t <- 1:t
      pub <- bind_cols(self$history[eval(c("t",columns))])
      pub$t <- as_factor(pub$t)
      return(pub)
    }
  )
)
