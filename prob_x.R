prob_x <- function(v,x){
  n <- length(x)
  data <- data.frame(
    x = x,
    prob_x = rep(NA, n),
    prob_x_h = rep(NA, n), prob_x_l = rep(NA, n), prob_x_n = rep(NA, n),
    prob_v_h = rep(NA, n), prob_v_l = rep(NA, n), prob_v_n = rep(NA, n),
    beta = rep(NA, n), sigma = rep(NA, n))

  alpha <- v[1]
  delta <- v[2]
  mu <- v[3]
  tau <- v[4]
  eps <- v[5]


  
  g_h <- function(s_t, tau) 1 + tau * (2 * s_t - 1)
  g_l <- function(s_t, tau) 1 - tau * (2 * s_t - 1)
  
  prob_h <- function(alpha, delta)  alpha * delta
  prob_l <- function(alpha, delta)  alpha * (1 - delta)
  prob_n <- function(alpha, delta)  1 - delta
  
  data[1, c("prob_v_h", "prob_v_l", "prob_v_n")] <-
    c(prob_h(alpha, delta),
      prob_l(alpha, delta),
      prob_n(alpha, delta))
  
  
  prob_buy_h <- function(beta, mu, tau, eps) mu * (tau * (1 - beta ^ 2) + (1 - tau) * (1 - beta)) + (1 - mu) * eps /2
  prob_buy_l <- function(beta, mu, tau, eps) mu * (-tau * (1 - beta ^ 2) + (1 + tau) * (1 - beta)) + (1 - mu) * eps / 2
  prob_buy_n <- function(beta, mu, tau, eps) 1 - prob_buy_h(beta, mu, tau, eps) - prob_buy_l(beta, mu, tau, eps)
  prob_sell_h <- function(sigma, mu, tau, eps) mu * (tau * (sigma ^ 2) + (1 - tau) * (sigma)) + (1 - mu) * eps / 2
  prob_sell_l <- function(sigma, mu, tau, eps) mu * (-tau * (sigma ^ 2) + (1 + tau) * (sigma)) + (1 - mu) * eps / 2
  prob_sell_n <- function(sigma, mu, tau, eps)  1 - prob_sell_h(sigma, mu, tau, eps) - prob_buy_l(sigma, mu, tau, eps)
  prob_no_h <- eps / 2
  prob_no_l <- eps / 2
  prob_no_n <- 1 - eps
  
  
  prob_v_beta_h <- function(beta) {
    g_h(beta, tau) * data[t, "prob_v_h"] /
      (g_h(beta, tau) * data[t, "prob_v_h"] +
         g_l(beta, tau) * data[t, "prob_v_l"])
  }
  prob_v_beta_l <- function(beta) {
    g_l(beta, tau) * data[t, "prob_v_l"] /
      (g_h(beta, tau) * data[t, "prob_v_h"] +
         g_l(beta, tau) * data[t, "prob_v_l"])
  }
  prob_v_sigma_h <- function(sigma) {
    g_h(sigma, tau) * data[t, "prob_v_h"] /
      (g_h(sigma, tau) * data[t, "prob_v_h"] +
         g_l(sigma, tau) * data[t, "prob_v_l"])
  }
  prob_v_sigma_l <- function(sigma) {
    g_l(sigma, tau) * data[t, "prob_v_l"] /
      (g_h(sigma, tau) * data[t, "prob_v_h"] +
         g_l(sigma, tau) * data[t, "prob_v_l"])
  }
  
  
  prob_v_buy_h <- function(beta) {
    prob_buy_h(beta, mu, tau, eps) * data[t, "prob_v_h"] /
      (prob_buy_h(beta, mu, tau, eps) * data[t, "prob_v_h"] +
          prob_buy_l(beta, mu, tau, eps) * data[t, "prob_v_l"] +
          prob_buy_n(beta, mu, tau, eps) * data[t, "prob_v_n"])
  }
  prob_v_buy_l <- function(beta) {
    prob_buy_l(beta, mu, tau, eps) * data[t, "prob_v_l"] /
      (prob_buy_h(beta, mu, tau, eps) * data[t, "prob_v_h"] +
          prob_buy_l(beta, mu, tau, eps) * data[t, "prob_v_l"] +
          prob_buy_n(beta, mu, tau, eps) * data[t, "prob_v_n"])
  }
  prob_v_sell_h <- function(sigma) {
    prob_sell_h(sigma, mu, tau, eps) * data[t, "prob_v_h"] /
      (prob_sell_h(sigma, mu, tau, eps) * data[t, "prob_v_h"] +
          prob_sell_l(sigma, mu, tau, eps) * data[t, "prob_v_l"] +
          prob_sell_n(sigma, mu, tau, eps) * data[t, "prob_v_n"])
  }
  prob_v_sell_l <- function(sigma) {
    prob_sell_l(sigma, mu, tau, eps) * data[t, "prob_v_l"] /
      (prob_sell_h(sigma, mu, tau, eps) * data[t, "prob_v_h"] +
          prob_sell_l(sigma, mu, tau, eps) * data[t, "prob_v_l"] +
          prob_sell_n(sigma, mu, tau, eps) * data[t, "prob_v_n"])
  }
  
  
  prob_v_beta_h <- function(beta) {
    g_h(beta, tau) * data[t, "prob_v_h"] /
      (g_h(beta, tau) * data[t, "prob_v_h"] +
         g_l(beta, tau) * data[t, "prob_v_l"])
  }
  prob_v_beta_l <- function(beta) {
    g_l(beta, tau) * data[t, "prob_v_l"] /
      (g_h(beta, tau) * data[t, "prob_v_h"] +
         g_l(beta, tau) * data[t, "prob_v_l"])
  }
  prob_v_buy_h <- function(beta) {
    prob_buy_h(beta, mu, tau, eps) * data[t, "prob_v_h"] /
      (prob_buy_h(beta, mu, tau, eps) * data[t, "prob_v_h"] +
          prob_buy_l(beta, mu, tau, eps) * data[t, "prob_v_l"] +
          prob_buy_n(beta, mu, tau, eps) * data[t, "prob_v_n"])
  }
  prob_v_buy_l <- function(beta) {
    prob_buy_l(beta, mu, tau, eps) * data[t, "prob_v_l"] /
      (prob_buy_h(beta, mu, tau, eps) * data[t, "prob_v_h"] +
          prob_buy_l(beta, mu, tau, eps) * data[t, "prob_v_l"] +
          prob_buy_n(beta, mu, tau, eps) * data[t, "prob_v_n"])
  }
  
  beta_solv <- function(beta) {
    (1 - delta) * (prob_v_beta_h(beta) - prob_v_buy_h(beta)) - delta * (prob_v_beta_l(beta) - prob_v_buy_l(beta))
  }
  
  sigma_solv <- function(sigma) {
    (1 - delta) * (prob_v_sigma_h(sigma) - prob_v_sell_h(sigma)) - delta * (prob_v_sigma_l(sigma) - prob_v_sell_l(sigma))
  }
  
  
  
  for (t in 1:n) {
    if (data[t, "x"] == 1) {
      data[t, "beta"] <- uniroot(beta_solv, c(0, 1))$root
      data[t, "prob_x_h"] <- prob_buy_h(data[t, "beta"], mu, tau, eps)
      data[t, "prob_x_l"] <- prob_buy_l(data[t, "beta"], mu, tau, eps)
      data[t, "prob_x_n"] <- prob_buy_n(data[t, "beta"], mu, tau, eps)
    } else if (data[t, "x"] == 0) {
      data[t, "prob_x_h"] <- prob_no_h
      data[t, "prob_x_l"] <- prob_no_l
      data[t, "prob_x_n"] <- prob_no_n
    } else if (data[t, "x"] == -1) {
      data[t, "sigma"] <- uniroot(sigma_solv, c(0, 1))$root
      data[t, "prob_x_h"] <- prob_sell_h(data[t, "sigma"], mu, tau, eps)
      data[t, "prob_x_l"] <- prob_sell_l(data[t, "sigma"], mu, tau, eps)
      data[t, "prob_x_n"] <- prob_sell_n(data[t, "sigma"], mu, tau, eps)
    }
    
    data[t, "prob_x"] <- data[t, "prob_x_h"] * data[t, "prob_v_h"] + data[t, "prob_x_l"] * data[t, "prob_v_l"] + data[t, "prob_x_n"] * data[t, "prob_v_n"]
    
    if (t < n && is.na(data[t+1, "prob_v_h"])) {
      data[t + 1, "prob_v_h"] <-
        cumprod(data$prob_x_h)[t] * data[t, "prob_v_h"] / 
        (cumprod(data$prob_x_h)[t] * data[t, "prob_v_h"] +
            cumprod(data$prob_x_l)[t] * data[t, "prob_v_l"] +
            cumprod(data$prob_x_n)[t] * data[t, "prob_v_n"])
      data[t + 1, "prob_v_l"] <-
        cumprod(data$prob_x_l)[t] * data[t, "prob_v_l"] / 
        (cumprod(data$prob_x_h)[t] * data[t, "prob_v_h"] +
            cumprod(data$prob_x_l)[t] * data[t, "prob_v_l"] +
            cumprod(data$prob_x_n)[t] * data[t, "prob_v_n"])
      data[t + 1, "prob_v_n"] <-
        cumprod(data$prob_x_n)[t] * data[t, "prob_v_n"] / 
        (cumprod(data$prob_x_h)[t] * data[t, "prob_v_h"] +
            cumprod(data$prob_x_l)[t] * data[t, "prob_v_l"] +
            cumprod(data$prob_x_n)[t] * data[t, "prob_v_n"])
    }
  }
  return(data)
}
