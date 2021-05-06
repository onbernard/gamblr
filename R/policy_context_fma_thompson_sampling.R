LinearThompsonSamplingPolicy = R6::R6Class(
  public = list(
    name = NULL,
    lambda = NULL,
    delta = NULL,
    iter = NULL,
    b = NULL,
    V = NULL,
    V_inv = NULL,
    th_hat = NULL,
    dim = NULL,
    record = NULL,


    initialize = function(dim, iter=10, lambda, delta){
      self$lambda = lambda
      self$delta = delta
      self$iter = iter
      self$name <- paste(
        c("linTS(it=",iter, ",l=", lambda, ",d=", delta, ")"),
        collapse="",  sep="")

      self$dim <- dim
      set.seed(1234)
      invisible(self)
    },

    reset = function(){
      self$b <- matrix(0,self$dim)
      self$V <- self$lambda * diag(self$dim)
      self$V_inv <- (1/self$lambda) * diag(self$dim)
      self$th_hat <- matrix(0,self$dim)
      self$record <- Recorder$new()
    },

    compute_beta = function(t){
      sqrt(self$lambda) +
        sqrt(
          2 * log(1/self$delta) +
            self$dim * log(1+(t-1)/(self$lambda * self$dim))
        )
    },

    get_action = function(C, t){
      # C pour contexte = K x dim x (nDataPoints)
      #  K = nrow(C) ?
      C <- as.matrix(C) # otherwise if there is only one dimension it's flat
      P <- c()
      beta <- self$compute_beta(t)
      for(a in 1:nrow(C)){
        th_tild <- apply(
          MASS::mvrnorm(self$iter, self$th_hat, beta * self$V_inv),2,max)
        P[a] <- C[a,] %*% th_tild
      }

      which <- which.max(P)
      self$record$vector("expreward", P)
      self$record$scalar("which", which)
      self$record$vector("action", C[which,])
      which
    },

    update_with_reward = function(r, a){
      self$V <- self$V + a %*% a
      self$V_inv <- solve(self$V)
      self$b <- self$b + r * a
      self$th_hat <- self$V_inv %*% self$b
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
      self$record$publish(nDataPoints,c("expreward","which","action", "reward"))
    }
  )
)
