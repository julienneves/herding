lee_ready <- function(tqdata, delay = default_delay(tqdata)){
    
    x = getTradeDirection(tqdata)
    trade = xts(x = x, order.by = time(tqdata), unique = FALSE, tzone = attr(tqdata,'tzone'))
    
    trade_split <- split.xts(trade, f= "days")
    trade_split <- lapply(trade_split, insert_no_trade, delay)
    trade <- do.call(rbind,trade_split)
    
    trade <- merge(trade,xts(x = matrix(rep(NA,9*length(trade)),ncol = 9), order.by = time(trade)))
    colnames(trade) <- c("x", "prob_x", "beta", "sigma", "prob_x_h", "prob_x_l", "prob_x_n", "prob_v_h", "prob_v_l", "prob_v_n")
    
    return(trade)
}

insert_no_trade <- function(trade, delay = NULL){
    
    time_trade <- time(trade)
    
    time_no_trade <- test_inactivity(time_trade, delay)
    
    no_trade <- xts(x = rep(0,length(time_no_trade)), order.by = time_no_trade, unique = FALSE, tzone = attr(tqdata,'tzone'))
    
    no_trade[time_trade,] <- trade
    trade <- no_trade
    return(trade)
}

test_inactivity <- function(time_trade, delay){
    
    diff_time <- diff(time_trade)
    
    if(!all(diff_time<=delay)){
        time_trade <- c(time_trade[diff_time>delay]+delay,time_trade)
        time_trade <- time_trade[order(time_trade)]
        time_trade <- add_no_trade(time_trade, delay)
    }
    return(time_trade)
}

delay =     trade_split <- split.xts(trade, f= "days")

default_delay <- function(tqdata) {
    
    x = getTradeDirection(tqdata)
    trade = xts(x = x, order.by = time(tqdata), unique = FALSE, tzone = attr(tqdata,'tzone'))
    
    trade_split <- split.xts(trade, f= "days")
    
    time_mean <- function(trade) mean(diff(time(trade)))
    
    return(mean(sapply(trade_split, time_mean)))
}

