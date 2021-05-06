# Kullback-Leibler Upper Confidence Bound (KL-UCB) bandit strategy.
# LIVRE : KLUCB2
KLUCBPolicy <- R6::R6Class(
  public = list(
    name = NULL,
    record = NULL,
    precision = NULL,
    c = NULL,
    N = NULL,
    mu_hat = NULL,

    initialize = function(precision=1e-6, c=0){
      self$name <- paste(
        c("klucb(prec=",precision, ",c=", c, ")"),
        collapse="",  sep="")
      self$precision <- precision
      self$c <- c
      invisible(self)
    },

    reset = function(K){
      self$N <- rep(0, K)
      self$mu_hat <- rep(Inf, K)
      self$record <- Recorder$new()
      invisible(self)
    },

    # dichotomique, séparer ?
    condition_for_KLUCB = function(kl, upperbound, max_iter){
      t <- sum(self$N)
      d <- log(t) + self$c * (log(log(t))) / self$N
      upperbound <- upperbound
      value <- self$mu_hat
      i <- 0
      kl_vec <- Vectorize(kl)

      while(i < max_iter && upperbound-value > self$precision){
        m <- (value + upperbound)/2

        upperbound <- ifelse(kl_vec(self$mu_hat, m) > d, m, upperbound)
        value <- ifelse(kl_vec(self$mu_hat, m) <= d, m, value)

        count_iteration = count_iteration + 1
      }

      return ((value + upperbound) / 2)
    },

    kl_bernouilli = function(p, q){
      epsilon <- 1e-16
      p <- min(max(p, epsilon), 1 - epsilon)
      q <- min(max(q, epsilon), 1 - epsilon)

      p*log(p/q) + (1-p)*log((1-p)/(1-q))
    },

    kl_gaussian = function(x, d, sig2=1){
      x + sqrt(2 * sig2 * d)
    },

    kl_gaussian2 = function(mu1, sig1, mu2, sig2){
      log(sig2/sig1) + (sig1^2 + (mu1-mu2)^2) / (2*sig2^2) - 0.5
    },

    # x = S[1,]
    # d=(log(t) + c*(log(log(t)))) / S[2,]
    # precision = precision
    # S = S
    # max_iteration = 50
    # visitor_reward = visitor_reward[j,]I_È -TT FYBUÈSGIYURSKJ TGFHYHJ
    compute_KL_UCB = function(max_iter){
      t <- sum(self$N)
      d <- (log(t) + c*(log(log(t)))) / self$N

      upperbound = min(1, kl_ucb_gaussian(self$mu_hat, d))

      condition_for_KLUCB(kl=kl_bernoulli, upperbound, max_iter)
    },

    get_action = function(){
      KLUCB <- self$compute_KL_UCB(50)
      self$record$vector("klucb", KLUCB)
      self$record$scalar("mu_hat", self$mu_hat)
      return(which.max(KLUCB))
    },

    update_with_reward = function(r, a){
      if(self$mu_hat[a] == Inf){
        self$mu_hat[a] <- r
      }
      else{
        self$mu_hat[a] <- (self$mu_hat[a] * self$N[a] + r) / (self$N[a]+1)
      }
      self$N[a] <- self$N[a] + 1
      self$record$scalar("reward",r)
      self$record$scalar("action",a)
      invisible(self)
    },

    run = function(arms, nDataPoints){
      # TODO : change function to handle NA
      self$reset(length(arms))

      for(t in 1:nDataPoints){
        a <- self$get_action(t)
        r <- arms[[a]]$get_reward(t)
        self$update_with_reward(r,a)
      }
      self$record$publish(nDataPoints,c("action","reward","klucb","mu_hat"))
    }
  )
)
