library(GA)
library(highfrequency)

source('~/Projects/Herding/code/lee_ready.R')
source('~/Projects/Herding/code/prob_x.R')

from = "1998-01-01"; 
to = "1998-01-31"; 
datasource = "~/Projects/Herding/xts_data";

tdata = TAQLoad("ASH",from,to,trades=TRUE,quotes=FALSE,
                datasource=datasource,variables=NULL)
qdata = TAQLoad("ASH",from,to,trades=FALSE,quotes=TRUE,
                datasource=datasource,variables=NULL)

#Match the trade and quote data
tqdata = matchTradesQuotes(tdata,qdata);

#Get the inferred trade direction according to the Lee-Ready rule
trade = lee_ready(tqdata, delay = 200)

v <- c(.28,.62,.42,.45,.57)
lower <- rep(0.1,5)
upper <- rep(0.9,5)


likelihood <- function(v){
    trade <- prob_trade(trade, v)
    return(sum(trade$prob_x))
}

GA <- ga(type = "real-valued", fitness = likelihood, min = lower, max = upper, monitor = plot)

