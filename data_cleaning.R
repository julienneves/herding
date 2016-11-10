library(highfrequency)

from = "2013-01-01"; 
to = "2013-02-01"; 
datasource = "C:/Users/Server/Desktop/Projects/Herding/raw_data";
datadestination = "C:/Users/Server/Desktop/Projects/Herding/xts_data";
convert( from=from, to=to, datasource=datasource, 
         datadestination=datadestination, trades = F,  quotes = T, 
         ticker="ASH", dir = TRUE, extension = "csv", 
         header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
         format="%Y%m%d %H:%M:%S", onefile = TRUE )

convert( from=from, to=to, datasource=datasource, 
         datadestination=datadestination, trades = T,  quotes = F, 
         ticker="ASH", dir = TRUE, extension = "csv", 
         header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
         format="%Y%m%d %H:%M:%S", onefile = TRUE )

