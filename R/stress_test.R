
library(lme4)
library(tibble)

# Example taken from https://m-clark.github.io/posts/2019-10-20-big-mixed-models/
# Simulate some data

set.seed(12358)
N <- 1e6 # total sample size
n_groups <- 1000 # number of groups
g <- rep(1:n_groups, e = N / n_groups) # the group identifier

x <- rnorm(N) # an observation level continuous variable
b <- rbinom(n_groups, size = 1, prob = .5) # a cluster level categorical variable
b <- b[g]

sd_g <- .5 # standard deviation for the random effect
sigma <- 1 # standard deviation for the observation

re0 <- rnorm(n_groups, sd = sd_g) # random effects
re <- re0[g]

lp <- 0 + .5 * x + .25 * b + re # linear predictor

y <- rnorm(N, mean = lp, sd = sigma) # create a continuous target variable
y_bin <- rbinom(N, size = 1, prob = plogis(lp)) # create a binary target variable

d <- tibble(x, b, y, y_bin, g = factor(g))

# Run the models
# Linear mixed effect
system.time({
  mixed_big <- lmer(y ~ x + b + (1 | g))
})

# Generalised linear mixed effect (more intensive)
system.time({
  mixed_big_glmm <- glmer(y_bin ~ x + b + (1 | g), family = binomial)
})
