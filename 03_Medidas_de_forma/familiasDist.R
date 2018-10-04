## Perason typo VII family
pearsonVII <- function(x, a, m) {
    gamma(m)/(a*sqrt(pi)*gamma(m-1/2)) * (1 + (x / a)^2)^(-m)
}

## a is a scale parameter
## m is a shape parameter

exessCurtosis <- function(x, lambda){
    pearsonVII(x, a = sqrt(2 + 6 / lambda), m = 5/2 + 3/lambda)
}


## Semicírculo

semicirculo <- function(x){
    2/(pi*2^2) * sqrt(2^2 - x^2)
}


semicirculo(-2)

ggplot(data.frame(x = 0), aes(x = x)) +
    stat_function(fun = dnorm) + 
    stat_function(fun = exessCurtosis, args = list(lambda = 100000000)) + 
    stat_function(fun = dt, args = list(df = 1)) +
    stat_function(fun = semicirculo, xlim = c(-2, 2)) +
    xlim(-3, 3) +
    layer()

    scale_color_manual("Kurtosis", values = c("black", "tomato", "lightblue"))

    
    # stat_function(fun = function(x, lambda) log(exessCurtosis(x, lambda)), args = list(lambda = 5)) +
    

ggplot(data.frame(x = 0), aes(x = x)) +
    layer(stat = "function",
          fun = dnorm,
          mapping = aes(color = "Mesocúrtica")) +
    scale_x_continuous(limits = c(-5, 5)) +
    scale_color_manual(name = "Kurtosis", values = c("blue"))

ggplot(data.frame(x = 0), aes(x = x)) +
    stat_function(fun = dnorm, aes(color = "dnorm")) + 
    stat_function(fun = exessCurtosis, args = list(lambda = 100000000), aes(color = "dnorm2")) + 
    stat_function(fun = dt, args = list(df = 1), aes(color = "dnorm3")) +
    stat_function(fun = semicirculo, xlim = c(-2, 2), aes(color = "dnorm4")) +
    xlim(-3, 3) +
    scale_color_manual("Kurtosis", values = c("black", "tomato", "lightblue", "seagreen3"))

ggplot(data.frame(x = 0), aes(x = x)) +
    stat_function(fun = dnorm, aes(color = "Mesocúrtica"), size = 1) + 
    stat_function(fun = dt, args = list(df = 1), aes(color = "Leptocúrtica"), size = 1) +
    stat_function(fun = semicirculo, xlim = c(-2, 2), aes(color = "Platicúrtica"), size = 1) +
    xlim(-3, 3) +
    scale_color_manual("Kurtosis", values = c("tomato", "black", "lightblue")) +
    theme_minimal() +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
    labs(x = NULL, y = NULL)






## Asimetría

ggplot(data.frame(x = 0), aes(x = x)) +
    stat_function(fun = dlnorm, xlim = c(0, 5)) + 
    stat_function(fun = function(x){dlnorm(-x)}, xlim = c(-5, 0)) + 
    # stat_function(fun = semicirculo, xlim = c(-2, 2), col = "lightblue") + 
    
    # stat_function(fun = function(x, lambda) log(exessCurtosis(x, lambda)), args = list(lambda = 5)) +
    xlim(-5, 5)


## Asimétrico a la izquierda


library(moments)

kurtosis(rnorm(1000))
kurtosis(rt(1000, 1))
