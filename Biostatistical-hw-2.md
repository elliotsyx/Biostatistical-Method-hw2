BM Hw2
================
Yixiao Sun
2023-10-06

\#Problem1

``` r
n1 <- 56
p <- 0.73
prob_at_least_40 <- sum(dbinom(40:56, n1, p))
mean <- n1 * p
std <- sqrt(n1 * p * (1-p))
z_score_40 <- (40 - mean) / std
prob_at_least_40 <- 1 - pnorm(z_score_40)
expected_value <- n1 * p
standard_deviation <- sqrt(n1 * p * (1-p))
```

\#Problem2

``` r
lambda <- 6
prob_fewer_than_3 <- ppois(2, lambda)
prob_exactly_3 <- dpois(3, lambda)
prob_more_than_3 <- 1 - ppois(3, lambda)
```

\#Problem3

``` r
mu <- 128.0
sigma <- 10.2
x1 <- 137.0

z1 <- (x1 - mu) / sigma
prob_above_137 <- 1 - pnorm(z1)

n2 <- 50
SE <- sigma / sqrt(n2)
x2 <- 125.0

z2 <- (x2 - mu) / SE
prob_below_125 <- pnorm(z2)

n3 <- 40
SE2 <- sigma / sqrt(n3)

percentile_90 <- qnorm(0.90, mu, SE2)
```

\#Problem4

``` r
n4 <- 40
sample_mean <- 80
sample_sd <- 10
alpha <- 0.05

t_critical <- qt(1 - alpha/2, df = n4-1)
margin_of_error <- t_critical * (sample_sd / sqrt(n4))

confidence_interval <- c(sample_mean - margin_of_error, sample_mean + margin_of_error)

mu_0 <- 70

t_statistic_2 <- (sample_mean - mu_0) / (sample_sd / sqrt(n4))
p_value <- 2*(1-(pt(abs(t_statistic_2), df = 39)))
```
