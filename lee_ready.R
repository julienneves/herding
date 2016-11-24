lee_ready <- function(tqdata, delay = 200){
    
    x = getTradeDirection(tqdata)
    trade = xts(x = x, order.by = time(tqdata), unique = FALSE)
    
    trade_split <- split.xts(trade, f= "days")
    trade_split <- lapply(trade_split, no_trade, delay)
    trade <- do.call(rbind,trade_split)
    
    trade <- merge(trade,xts(x = matrix(rep(NA,6*length(trade)),ncol = 6), order.by = time(trade)))
    colnames(trade) <- c("x", "prob_x","prob_x_h","prob_x_l","prob_x_n", "beta", "sigma")
    
    return(trade)
}

no_trade <- function(trade, delay){
    
    time_trade <- time(trade)
    
    time_no_trade <- add_no_trade(time_trade, delay)
    
    no_trade <- xts(x = rep(0,length(time_no_trade)), order.by = time_no_trade, unique = FALSE)
    
    no_trade[time_trade,] <- trade
    trade <- no_trade
    
    return(trade)
}

add_no_trade <- function(time_trade, delay){
    
    diff_time <- diff(time_trade)
    
    if(!all(diff_time<=delay)){
        time_trade <- c(time_trade[diff_time>delay]+delay,time_trade)
        time_trade <- time_trade[order(time_trade)]
        time_trade <- add_no_trade(time_trade, delay)
    }
    return(time_trade)
}
