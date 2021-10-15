library(rethinking)
library(readr)
path <- paste(getwd(), "/assigment/data.csv", sep = "")
print(path)
d <- read_delim(path,
    delim = ";",
    col_types = cols(
        .default = "?",
        category = "factor",
        technique = "factor"
    )
)

d$category_id <- as.numeric(d$category)
d$technique_id <- as.numeric(d$technique)

ot_le <- d[which(d$technique_id == 1 & d$category_id == 1), ]
ot_me <- d[which(d$technique_id == 1 & d$category_id == 2), ]
nt_le <- d[which(d$technique_id == 2 & d$category_id == 1), ]
nt_me <- d[which(d$technique_id == 2 & d$category_id == 2), ]

d_ot_le <- density(ot_le$tp)
d_ot_me <- density(ot_me$tp)
d_nt_le <- density(nt_le$tp)
d_nt_me <- density(nt_me$tp)

df <- data.frame(
    technique = c("OT", "OT", "NT", "NT"),
    experience = c("LE", "ME", "LE", "ME"),
    mean = c(mean(ot_le$tp), mean(ot_me$tp), mean(nt_le$tp), mean(nt_me$tp)),
    sd = c(sd(ot_le$tp), sd(ot_me$tp), sd(nt_le$tp), sd(nt_me$tp))
)
print(head(df))

plot(d_ot_le,
    main = "Density graphs",
    ylab = "", type = "l", col = "cyan",
    ylim = c(0, 0.3), xlim = c(0, 10)
)
lines(d_ot_me, col = "magenta")
lines(d_nt_le, col = "red")
lines(d_nt_me, col = "green")
legend(
    "topleft",
    c("OT-LE", "OT-ME", "NT-LE", "NT-ME"),
    fill = c("cyan", "magenta", "red", "green")
)

# Negative binomial approach
# först vanlig poisson
dat <- list(
    T = d$technique_id,
    E = d$category_id,
    S = d$subject,
    F = d$tp
)
str(dat)

m1.1 <- ulam(
    alist(
        F ~ dpois(lambda),
        log(lambda) <- a[T] + b[E],
        a[T] ~ dnorm(0, 1.5),
        b[E] ~ dnorm(0, 1.5)
    ),
    data = dat, chains = 4
)


# Det finns inget övre limit av möjliga faults så därför är det inte orimligt med
# en Poisson