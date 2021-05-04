CTreeUCBPolicy <- R6::R6Class(
  public = list(
    name = NULL,
    record = NULL,
    learn_size = NULL,
    arm_for_learn = NULL,
    alpha = NULL,
    explanatory_variable = NULL,
    ctree_control_val = NULL,
    # Some parameters that are dependent on dt and rewards


    initialize = function(){
      self$name <- paste(
        c("CTreeUCB"),
        collapse="",  sep="")
      invisible(self)
    },

    reset = function(){
      self$record <- Recorder$new()
      invisible(self)
    },

    run = function(dt, rewards){
      # TODO : change function to handle NA
      self$reset()

      dimensions <- dim(dt)
      K <- dimensions[1] # arms on rows
      dim <- dimensions[2] # dimensions on columns
      nDataPoints <- dimensions[3] # dataPoints on depth

      for(t in 1:nDataPoints){
        which <- self$get_action(dt[,,t],t)
        action <- dt[which,,t]
        r <- rewards[t,which]
        self$record$scalar("reward", r)
        self$update_with_reward(r,action)
      }
      self$record$publish(nDataPoints,c("ucb","which","action", "reward"))
    }
  )
)
