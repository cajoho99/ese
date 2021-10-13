library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

xbar <- mean(d2$weight)

m4.3 <- quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b * (weight - xbar),
        a ~ dnorm(178, 20),
        b ~ dlnorm(0, 1),
        sigma ~ dunif(0, 50)
    ),
    data = d2
)

post <- extract.samples(m4.3)
weight.seq <- seq(from = 25, to = 70, by = 1)
mu <- link(m4.3, data = data.frame(weight = weight.seq))
str(mu)
plot(height ~ weight, d2, type = "n")

for (i in 1:100) {
    points(
        weight.seq,
        mu[i, ],
        pch = 16,
        col = col.alpha(rangi2, 0.1)
    )
}

mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)

plot(height ~ weight, data = d2, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)

sim.height <- sim(m4.3, data = list(weight = weight.seq))
str(sim.height)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)
plot(height ~ weight, d2, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

d$weight_s <- (d$weight - mean(d$weight)) / sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- queap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b1 * weight_s + b2 * weight_s2,
        a ~ dnorm(178, 20),
        b1 ~ dlnorm(0, 1),
        b2 ~ dnorm(0, 1),
        sigma ~ dunif(0, 50)
    ),
    data = d
)
precis(m4.5)

weight.seq <- seq(from = -2.2, to = 2, length.out = 30)
pred_dat <- list(weight_s = weight.seq, weight_s2 = weight.seq^2)
mu <- link(m4.5, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(m4.5, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

plot(height ~ weight_s, d, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

d$weight_s3 <- d$weight_s^3
m4.6 <- quap(
    alist(
        height ~ dnorm(mu, sigma),
    )
)