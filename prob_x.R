prob_trade <- function(trade, v){
    trade_split <- split.xts(trade, f= "days")
    trade_split <- lapply(trade_split,prob_x,v)
    trade <- do.call(rbind,trade_split)
    return(trade)
}


prob_x <- function(trade, v){
  alpha <- v[1]
  delta <- v[2]
  mu <- v[3]
  tau <- v[4]
  eps <- v[5]
  
  prob_h <- alpha * delta
  prob_l <- alpha * (1 - delta)
  prob_n <-  1 - alpha
  
  prob_v_h <- prob_h
  prob_v_l <- prob_l
  prob_v_n <- prob_n
  
  hist_x_h <- 1
  hist_x_l <- 1
  hist_x_n <- 1
  
  beta <- .5
  sigma <- .5
  
      
  g_h <- function(s_t, tau) 1 + tau * (2 * s_t - 1)
  g_l <- function(s_t, tau) 1 - tau * (2 * s_t - 1)
  
  
  prob_buy_h <- function(beta, mu, tau, eps) mu * (tau * (1 - beta ^ 2) + (1 - tau) * (1 - beta)) + (1 - mu) * eps /2
  prob_buy_l <- function(beta, mu, tau, eps) mu * (- tau * (1 - beta ^ 2) + (1 + tau) * (1 - beta)) + (1 - mu) * eps / 2
  prob_buy_n <- function(beta, mu, tau, eps) 1 - prob_buy_h(beta, mu, tau, eps) - prob_buy_l(beta, mu, tau, eps)
  prob_sell_h <- function(sigma, mu, tau, eps) mu * (tau * (sigma ^ 2) + (1 - tau) * (sigma)) + (1 - mu) * eps / 2
  prob_sell_l <- function(sigma, mu, tau, eps) mu * (- tau * (sigma ^ 2) + (1 + tau) * (sigma)) + (1 - mu) * eps / 2
  prob_sell_n <- function(sigma, mu, tau, eps)  1 - prob_sell_h(sigma, mu, tau, eps) - prob_sell_l(sigma, mu, tau, eps)
  prob_no_h <- eps / 2
  prob_no_l <- eps / 2
  prob_no_n <- 1 - eps
  
  
  for (t in 1:dim(trade)[1]) {
  
  prob_v_beta_h <- function(beta) {
    g_h(beta, tau) * prob_v_h /
      (g_h(beta, tau) * prob_v_h +
         g_l(beta, tau) * prob_v_l)
  }
  prob_v_beta_l <- function(beta) {
    g_l(beta, tau) * prob_v_l /
      (g_h(beta, tau) * prob_v_h +
         g_l(beta, tau) * prob_v_l)
  }
  prob_v_sigma_h <- function(sigma) {
    g_h(sigma, tau) * prob_v_h /
      (g_h(sigma, tau) * prob_v_h +
         g_l(sigma, tau) * prob_v_l)
  }
  prob_v_sigma_l <- function(sigma) {
    g_l(sigma, tau) * prob_v_l /
      (g_h(sigma, tau) * prob_v_h +
         g_l(sigma, tau) * prob_v_l)
  }
  
  prob_v_buy_h <- function(beta) {
    prob_buy_h(beta, mu, tau, eps) * prob_v_h /
      (prob_buy_h(beta, mu, tau, eps) * prob_v_h +
          prob_buy_l(beta, mu, tau, eps) * prob_v_l +
          prob_buy_n(beta, mu, tau, eps) * prob_v_n)
  }
  prob_v_buy_l <- function(beta) {
    prob_buy_l(beta, mu, tau, eps) * prob_v_l /
      (prob_buy_h(beta, mu, tau, eps) * prob_v_h +
          prob_buy_l(beta, mu, tau, eps) * prob_v_l +
          prob_buy_n(beta, mu, tau, eps) * prob_v_n)
  }
  prob_v_sell_h <- function(sigma) {
    prob_sell_h(sigma, mu, tau, eps) * prob_v_h /
      (prob_sell_h(sigma, mu, tau, eps) * prob_v_h +
          prob_sell_l(sigma, mu, tau, eps) * prob_v_l +
          prob_sell_n(sigma, mu, tau, eps) * prob_v_n)
  }
  prob_v_sell_l <- function(sigma) {
    prob_sell_l(sigma, mu, tau, eps) * prob_v_l /
      (prob_sell_h(sigma, mu, tau, eps) * prob_v_h +
          prob_sell_l(sigma, mu, tau, eps) * prob_v_l +
          prob_sell_n(sigma, mu, tau, eps) * prob_v_n)
  }
  
  
  beta_solv <- function(beta) {
      abs((1 - delta) * (prob_v_beta_h(beta) - prob_v_buy_h(beta)) - delta * (prob_v_beta_l(beta) - prob_v_buy_l(beta)))
  }
  
  sigma_solv <- function(sigma) {
      abs((1 - delta) * (prob_v_sigma_h(sigma) - prob_v_sell_h(sigma)) - delta * (prob_v_sigma_l(sigma) - prob_v_sell_l(sigma)))
  }
  
  beta <- optim(par = beta, beta_solv, method = "L-BFGS-B", lower = 0, upper = 1)$par
  sigma <- optim(par = sigma, sigma_solv, method = "L-BFGS-B", lower = 0, upper = 1)$par
  
  
    if (trade[t, "x"] == 1) {
      prob_x_h <- prob_buy_h(beta, mu, tau, eps)
      prob_x_l <- prob_buy_l(beta, mu, tau, eps)
      prob_x_n <- prob_buy_n(beta, mu, tau, eps)
    } else if (trade[t, "x"] == 0) {
      prob_x_h <- prob_no_h
      prob_x_l <- prob_no_l
      prob_x_n <- prob_no_n
    } else if (trade[t, "x"] == -1) {
      prob_x_h <- prob_sell_h(sigma, mu, tau, eps)
      prob_x_l <- prob_sell_l(sigma, mu, tau, eps)
      prob_x_n <- prob_sell_n(sigma, mu, tau, eps)
    }
    
    trade[t, "prob_x"] <- prob_x_h * prob_v_h + prob_x_l * prob_v_l + prob_x_n * prob_v_n
    trade[t,c("beta","sigma")] <- c(beta, sigma) 
    trade[t,c("prob_x_h","prob_x_l","prob_x_n")] <- c(prob_x_h, prob_x_l, prob_x_n)
    
    hist_x_h <- hist_x_h * prob_x_h
    hist_x_l <- hist_x_l * prob_x_l
    hist_x_n <- hist_x_n * prob_x_n
    
    prob_v_h <  hist_x_h * prob_h / 
        (hist_x_h * prob_h +
             hist_x_l * prob_l +
             hist_x_n* prob_n)
    prob_v_l <- hist_x_l * prob_l / 
        (hist_x_h * prob_h +
             hist_x_l * prob_l +
             hist_x_n* prob_n)
    prob_v_n <-  hist_x_n * prob_n / 
        (hist_x_h * prob_h +
             hist_x_l * prob_l +
             hist_x_n* prob_n)
    
    cat("Iteration", t , "\r")
    flush.console()
    }
  return(trade)
}
