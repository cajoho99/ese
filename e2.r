library(rethinking)

# Part 1

data(Howell1)
d <- Howell1
d_over_18 <- d[d$age >= 18, ]
x_bar <- mean(d_over_18$weight)

m2.1 <- quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b * (weight - x_bar),
        a ~ dnorm(178, 20),
        b ~ dlnorm(0, 1),
        sigma ~ dunif(0, 50)
    ),
    data = d_over_18
)
post <- extract.samples(m2.1)
weight.seq <- c(46.95, 43.72, 64.78, 32.59, 54.63)

f <- function(weight) {
    y <- rnorm(1e5, post$a + post$b * (weight - x_bar), post$sigma)
    return(c(mean(y), PI(y, prob = 0.89)))
}
res <- sapply(weight.seq, f)

rtab <- cbind(weight.seq, t(res))
colnames(rtab) <- c("weight", "height", "5%", "94%")
rtab

# Part 2
# a
d_under_18 <- d[d$age < 18, ]
x_bar <- mean(d_under_18$weight)

m2.2 <- quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b * (weight - x_bar),
        a ~ dnorm(110, 20),
        b ~ dlnorm(0, 1),
        sigma ~ dunif(0, 50)
    ),
    data = d_under_18
)
precis(m2.2)
# b
post <- extract.samples(m2.2)
mu.link <- function(weight) post$a + post$b * (weight - x_bar)
weight.seq <- seq(from = 0, to = 45, by = 1)
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.CI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(m2.2, data = list(weight = weight.seq))
height.PI <- apply(sim.height, 2, PI, prob = 0.89)


plot(height ~ weight, data = d_under_18, col = rangi2)
lines(weight.seq, mu.mean)
shade(mu.CI, weight.seq)
shade(height.PI, weight.seq)
# c
# The model is whack as children weight/height relationship is not very linear.
# maybe parabolic or logarithmic.