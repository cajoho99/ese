library(rethinking)
data(milk)
d <- milk
levels(d$clade)

d$clade_id <- as.integer(d$clade)
d$K <- standardize(d$kcal.per.g)
m5.9 <- quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a[clade_id],
        a[clade_id] ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ),
    data = d
)
labels <- paste("a[", 1:4, "]:", levels(d$clade), sep = "")
plot(precis(m5.9, depth = 2, pars = "a"),
    labels = labels,
    xlab = "expected kcal (std)"
)