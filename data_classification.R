library(highfrequency)

from = "2013-01-01"; 
to = "2013-02-01"; 
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
