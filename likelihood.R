library(GA)
library(pso)
library(highfrequency)

setwd("~/Projects/Herding")

source('lee_ready.R')
source('prob_x.R')

# Set datasource + date
from <- "1998-01-01"; 
to <- "1998-01-31"; 
datasource <- "~/Projects/Herding/xts_data";

# Download both trades and quotes data
tdata <- TAQLoad("ASH",from,to, trades=TRUE, quotes=FALSE,
                datasource=datasource, variables=NULL)
qdata <- TAQLoad("ASH",from,to,trades=FALSE,quotes=TRUE,
                datasource=datasource, variables=NULL)

#Match the trade and quote data
tqdata <- matchTradesQuotes(tdata,qdata);

#Get the inferred trade direction according to the Lee-Ready rule
trade <- lee_ready(tqdata)

# Set sarting point and bounds for nuisance parameter
# v = (alpha, delta, mu, tau, eps)
v <- c(.28,.62,.42,.45,.57)
lower <- rep(0.1,5)
upper <- rep(0.9,5)

# Set loglikelihood function
likelihood <- function(v){
    trade <- prob_trade(trade, v)
    return(sum(trade$prob_x))
}

# Use particle swarm to find optimia
out_ga <- ga(type = "real-valued", fitness = likelihood, min = lower, max = upper, monitor = plot, run = 15, maxiter = 30)
summary(out_ga)

# Use particle swarm to find optimia
out_pso <- psoptim(v, likelihood, lower = lower, upper = upper, control = list(fnscale = -1,maxit = 30))


result <- function(trade,v){
    trade <- prob_trade(trade, v)
    
    herd_beta <- dim(trade[trade$beta<0.5])[1] / dim(trade)[1]
    herd_sigma <- dim(trade[trade$sigma>0.5])[1] / dim(trade)[1]
    
    out <- c(herd_beta,herd_sigma)
    names(out) <- c("herd buying", "herd selling")
    return(out)    
}

res <- result(trade, out_ga@solution)
res
