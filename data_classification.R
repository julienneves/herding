library(highfrequency)

from = "2013-01-01"; 
to = "2013-02-01"; 
datasource = "C:/Users/Server/Desktop/Projects/Herding/xts_data";

tdata = TAQLoad("ASH",from,to,trades=TRUE,quotes=FALSE,
                datasource=datasource,variables=NULL)
qdata = TAQLoad("ASH",from,to,trades=FALSE,quotes=TRUE,
                datasource=datasource,variables=NULL)

#Match the trade and quote data
tqdata = matchTradesQuotes(tdata,qdata);

#Get the inferred trade direction according to the Lee-Ready rule
x = getTradeDirection(tqdata)

time_diff = diff(time(tqdata$PRICE))
