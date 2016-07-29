# set the margins
tmpmar <- par("mar")
tmpmar[3] <- 0.5
par(mar=tmpmar)

# get underlying plot
x <- 1:10
y <- jitter(x^2)
plot(x, y, pch=20)

# basic straight line of fit
fit <- glm(y~x)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

# exponential
f <- function(x,a,b) {a * exp(b * x)}
fit <- nls(y ~ f(x,a,b), start = c(a=1, b=1)) 
co <- coef(fit)
curve(f(x, a=co[1], b=co[2]), add = TRUE, col="green", lwd=2) 

# logarithmic
f <- function(x,a,b) {a * log(x) + b}
fit <- nls(y ~ f(x,a,b), start = c(a=1, b=1)) 
co <- coef(fit)
curve(f(x, a=co[1], b=co[2]), add = TRUE, col="orange", lwd=2) 

# polynomial
f <- function(x,a,b,d) {(a*x^2) + (b*x) + d}
fit <- nls(y ~ f(x,a,b,d), start = c(a=1, b=1, d=1)) 
co <- coef(fit)
curve(f(x, a=co[1], b=co[2], d=co[3]), add = TRUE, col="pink", lwd=2)


# baby func f = a/(1+exp(-(x-x0)/b))
f <- function(x,a,b,x0) {a/(1+exp(-(x-x0)/b))}
fit <- nls(y ~ f(x,a,b,x0), start = c(a=1, b=1, x0=1)) 
co <- coef(fit)
curve(f(x, a=co[1], b=co[2], x0=co[3]),  add = TRUE, col="pink", lwd=2)
