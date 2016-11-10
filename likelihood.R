library(GA)
library(highfrequency)

from = "2013-01-01"; 
to = "2013-01-07"; 
datasource = "~/Projects/Herding/xts_data";

tdata = TAQLoad("ASH",from,to,trades=TRUE,quotes=FALSE,
                datasource=datasource,variables=NULL)
qdata = TAQLoad("ASH",from,to,trades=FALSE,quotes=TRUE,
                datasource=datasource,variables=NULL)

#Match the trade and quote data
tqdata = matchTradesQuotes(tdata,qdata);

#Get the inferred trade direction according to the Lee-Ready rule
x = getTradeDirection(tqdata)

trade = xts(x = cbind(x,rep(NA,length(x))) , order.by = time(tqdata), unique = FALSE)
colnames(trade) <- c("x","prob_x")


v <- c(.28,.62,.42,.45,.57)
lower <- v - rep(0.1,5)
upper <- v + rep(0.1,5)


likelihood <- function(v){
    trade <- prob_trade(trade, v)
    return(cumprod(trade$prob_x))
}

GA <- ga(type = "real-valued", fitness = likelihood, min = lower, max = upper)

