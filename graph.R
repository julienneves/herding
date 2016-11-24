library(ggplot2)
library(gridExtra)
library(broom)

day <- "1998-01-07"
x <- tidy(trade[day,c("beta","sigma")])
y <- tidy(cumsum(trade[day,"x"]))

g.top <- ggplot(x, aes(x=index,y=value, color = series)) + geom_line() + 
    geom_hline(aes(yintercept=0.5), color="blue", linetype="dashed", size=.5) + 
    theme_minimal()
g.bottom <- ggplot(y, aes(x=index,y=value, fill = series)) + 
    geom_area() + 
    theme_minimal()

grid.arrange(g.top, g.bottom, heights = c(.5, .5))
