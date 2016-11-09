library(GA)

#x <-  sample(c(-1,1), 10 ,replace = TRUE)
x <- rep(1,100)
v <- c(.28,.62,.42,.45,.57)
lower <- v - rep(0.01,5)
upper <- v + rep(0.01,5)

likelihood <- function(v){
    data <- prob_x(v,x)
    return(prod(data$prob_x))
}


GA <- ga(type = "real-valued", fitness = likelihood, min = lower, max = upper)

