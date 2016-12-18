prob_trade <- function(trade, v){
    # Split data into days
    trade_split <- split.xts(trade, f= "days")
    # Apply function to find probability of x for everyday
    trade_split <- lapply(trade_split,prob_x,v)
    # Recombine data
    trade <- do.call(rbind,trade_split)
    return(trade)
}


prob_x <- function(trade, v){
    # Separate nuisance parameter
    alpha <- v[1]
    delta <- v[2]
    mu <- v[3]
    tau <- v[4]
    eps <- v[5]
    
    # Set probability of high, low, no-change
    prob_h <- alpha * delta
    prob_l <- alpha * (1 - delta)
    prob_n <-  1 - alpha
    
    # Set beliefs at beginning of day
    prob_v_h <- prob_h
    prob_v_l <- prob_l
    prob_v_n <- prob_n
    
    # Initialize probability of sequence of x
    hist_x_h <- 1
    hist_x_l <- 1
    hist_x_n <- 1
    
    # Initialize beta and sigma
    beta <- .5
    sigma <- .5
    
    # Set g_h and g_l functions
    g_h <- function(s_t) 1 + tau * (2 * s_t - 1)
    g_l <- function(s_t) 1 - tau * (2 * s_t - 1)
  
    # Set probability of buy
    prob_buy_h <- function(beta) mu * (tau * (1 - beta ^ 2) + (1 - tau) * (1 - beta)) + (1 - mu) * eps /2
    prob_buy_l <- function(beta) mu * (-tau * (1 - beta ^ 2) + (1 + tau) * (1 - beta)) + (1 - mu) * eps / 2
    prob_buy_n <- function(beta) eps / 2
    
    # Set probability of sell
    prob_sell_h <- function(sigma) mu * (tau * (sigma ^ 2) + (1 - tau) * (sigma)) + (1 - mu) * eps / 2
    prob_sell_l <- function(sigma) mu * (-tau * (sigma ^ 2) + (1 + tau) * (sigma)) + (1 - mu) * eps / 2
    prob_sell_n <- function(sigma) eps / 2
    
    # Set probability of no-trade
    prob_no_h <- function(beta, sigma) 1 - prob_buy_h(beta) - prob_sell_h(sigma)
    prob_no_l <- function(beta, sigma) 1 - prob_buy_l(beta) - prob_sell_l(sigma)
    prob_no_n <- function(beta, sigma) 1 - eps
  
    # Start iteration
    for (t in 1:dim(trade)[1]) {
        # Set probability functions for market maker
        prob_v_beta_h <- function(beta) {
            g_h(beta) * prob_v_h /
                (g_h(beta) * prob_v_h +
                 g_l(beta) * prob_v_l)
            }
        prob_v_beta_l <- function(beta) {
            g_l(beta) * prob_v_l /
                (g_h(beta) * prob_v_h +
                 g_l(beta) * prob_v_l)
            }
        prob_v_sigma_h <- function(sigma) {
            g_h(sigma) * prob_v_h /
                (g_h(sigma) * prob_v_h +
                 g_l(sigma) * prob_v_l)
            }
        prob_v_sigma_l <- function(sigma) {
            g_l(sigma) * prob_v_l /
                (g_h(sigma) * prob_v_h +
                 g_l(sigma) * prob_v_l)
            }
        # Set probability functions for market maker
        prob_v_buy_h <- function(beta) {
            prob_buy_h(beta) * prob_v_h /
                (prob_buy_h(beta) * prob_v_h +
                 prob_buy_l(beta) * prob_v_l +
                 prob_buy_n(beta) * prob_v_n)
            }
        prob_v_buy_l <- function(beta) {
            prob_buy_l(beta) * prob_v_l /
                (prob_buy_h(beta) * prob_v_h +
                 prob_buy_l(beta) * prob_v_l +
                 prob_buy_n(beta) * prob_v_n)
            }
        prob_v_sell_h <- function(sigma) {
            prob_sell_h(sigma) * prob_v_h /
                (prob_sell_h(sigma) * prob_v_h +
                 prob_sell_l(sigma) * prob_v_l +
                 prob_sell_n(sigma) * prob_v_n)
            }
        prob_v_sell_l <- function(sigma) {
            prob_sell_l(sigma) * prob_v_l /
                (prob_sell_h(sigma) * prob_v_h +
                 prob_sell_l(sigma) * prob_v_l +
                 prob_sell_n(sigma) * prob_v_n)
            }
        
        # Set function to minimize (i.e. solve equality for market-marker and trader)
        beta_solv <- function(beta) {
            abs((1 - delta) * (prob_v_beta_h(beta) - prob_v_buy_h(beta)) - delta * (prob_v_beta_l(beta) - prob_v_buy_l(beta)))
        }
        
        sigma_solv <- function(sigma) {
            abs((1 - delta) * (prob_v_sigma_h(sigma) - prob_v_sell_h(sigma)) - delta * (prob_v_sigma_l(sigma) - prob_v_sell_l(sigma)))
        }
        
        # Minimize function to return signal
        beta <- optim(par = beta, beta_solv, method = "L-BFGS-B", lower = 0, upper = 1)$par
        sigma <- optim(par = sigma, sigma_solv, method = "L-BFGS-B", lower = 0, upper = 1)$par
        
        # Set probability of trade x given signal
        if (trade[t, "x"] == 1) {
            prob_x_h <- prob_buy_h(beta)
            prob_x_l <- prob_buy_l(beta)
            prob_x_n <- prob_buy_n(beta)
        } else if (trade[t, "x"] == 0) {
            prob_x_h <- prob_no_h(beta, sigma)
            prob_x_l <- prob_no_l(beta, sigma)
            prob_x_n <- prob_no_n(beta, sigma)
        } else if (trade[t, "x"] == -1) {
            prob_x_h <- prob_sell_h(sigma)
            prob_x_l <- prob_sell_l(sigma)
            prob_x_n <- prob_sell_n(sigma)
        }
        
        # Fill in the dataframe with the different probabilities for a given trade
        trade[t, "prob_x"] <- prob_x_h * prob_v_h + prob_x_l * prob_v_l + prob_x_n * prob_v_n
        trade[t,c("beta","sigma")] <- c(beta, sigma)
        
        # Update probability of trade sequence given the state of market
        scale <-  mean(hist_x_h, hist_x_l, hist_x_n)
        hist_x_h <- hist_x_h * prob_x_h / scale
        hist_x_l <- hist_x_l * prob_x_l / scale
        hist_x_n <- hist_x_n * prob_x_n / scale
        
        # Update beliefs
        prob_v_h <-  hist_x_h * prob_h /
            (hist_x_h * prob_h +
             hist_x_l * prob_l +
             hist_x_n * prob_n)
        prob_v_l <- hist_x_l * prob_l / 
            (hist_x_h * prob_h +
             hist_x_l * prob_l +
             hist_x_n * prob_n)
        prob_v_n <-  hist_x_n * prob_n / 
            (hist_x_h * prob_h +
             hist_x_l * prob_l +
             hist_x_n * prob_n)
        # Print day
        cat("Iteration", t , "\r")
        flush.console()
    }
  return(trade)
}
