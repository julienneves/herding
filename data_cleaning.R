library(highfrequency)

from = "1998-01-01"; 
to = "1998-01-31"; 
datasource = "~/raw_data";
datadestination = "~/xts_data";
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

