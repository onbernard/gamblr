# WIP
KernelUCBPolicy <- R6::R6Class(
  public = list(
    name = NULL,
    record = NULL,
    update_val = NULL,
    delta = NULL,
    lambda = NULL,
    alpha = NULL,
    dim = NULL,


    initialize = function(dim, update_val, delta, lamda, alpha){
      self$update_val <- update_val
      self$delta <- delta
      self$lambda <- lambda
      self$alpha <- alpha
      self$dim <- dim
      self$name <- paste(
        c("kernelUCB(upd=", update_val,
          ",l=", lambda,
          ",d=", delta,
          ",a=", alpha,")"),
        collapse="",  sep="")

      invisible(self)
    },

    reset = function(){
      self$record <- Recorder$new()
      invisible(self)
    }

  )
)
