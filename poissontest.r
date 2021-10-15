library(rethinking)
data(Kline)
d <- Kline

d$P <- scale(log(d$population))
d$contact_id <- ifelse(d$contact == "high", 2, 1)