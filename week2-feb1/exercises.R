#2M1.  ####

grid_size <- 1000

# define grid

p_grid <- seq(from = 0 ,
              to = 1 ,
              length.out = grid_size)

# define prior

prior <- rep(1 , grid_size)

# compute likelihood at each value in grid

likelihood <-
  dbinom(6 , size = 9 , prob = p_grid) #relative number of ways to get six W's, holding p at 0.5 and n at nine

likelihood1 <- dbinom(5, 7, prob = p_grid)

# compute product of likelihood and prior

unstd.posterior <- likelihood * prior
unstd.posterior1 <- likelihood1 * prior

# standardize the posterior, so it sums to 1

posterior <- unstd.posterior / sum(unstd.posterior)
posterior1 <- unstd.posterior1 / sum(unstd.posterior1)


plot(p_grid ,
     posterior1 ,
     type = "b" ,
     
     xlab = "probability of water" ,
     ylab = "posterior probability")

mtext(paste(grid_size, "points"))


# 2M2 ####


# define prior

prior2 <- data.frame(prior_val = 0, grid_point = p_grid) |>
  dplyr::mutate(prior_val = ifelse(grid_point < .5, 0, 1)) |>
  dplyr::select(prior_val) |>
  as.vector() |>
  unlist()

# define likelihood

likelihood2 <- dbinom(0, 3, prob = p_grid)

# compute product of likelihood and prior

unstd.posterior2 <- likelihood2 * prior2

# standardize the posterior, so it sums to 1

posterior2 <- unstd.posterior2 / sum(unstd.posterior2)


plot(p_grid ,
     posterior2 ,
     type = "b" ,
     
     xlab = "probability of water" ,
     ylab = "posterior probability")

mtext(paste(grid_size, "points"))


# So the prior of 0 sets a very strong constraint!

# 2M3 looks intriguing to work through. Maybe later.

# 3E2 ####

samples <-
  sample(p_grid ,
         prob = posterior ,
         size = 1e4 ,
         replace = TRUE)

mean(samples > .8)

# 3E5 ####
# 20% of the posterior probability lies above which value of p?

p_ecdf <- ecdf(samples)

1 - quantile(p_ecdf, .8)

# 3E6 ####
# Which values of p contain the narrowest interval equal to 66% of the posterior probability?

rethinking::HPDI(samples, prob = .66)


# 3M1 ####
# 3M1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the posterior distribution, using grid approximation. Use the same flat prior as before.


likelihood3 <- dbinom(8, 15, prob = p_grid)

# compute product of likelihood and prior

unstd.posterior3 <- likelihood3 * prior

# standardize the posterior, so it sums to 1

posterior3 <- unstd.posterior3 / sum(unstd.posterior3)

plot(posterior3)

# 3M2 ####
# 3M2. Draw 10,000 samples from the grid approximation from above. Then use the samples to calculate the 90% HPDI for p.

samples2 <- 
  sample(p_grid ,
         prob = posterior3 ,
         size = 1e5 ,
         replace = TRUE)

rethinking::HPDI(samples2,
                 prob = .9)


# 3H1 for fun ####

birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0, 0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0, 1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,00,0,1,0,1,1,0, 1,0,1,1,1,0,1,1,1,1)

birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,00, 1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1, 0,0,0,1,1,1,0,0,0,0)

prior <- rep(1 , grid_size)

# compute likelihood at each value in grid

likelihood4 <-
  dbinom(sum(birth1, birth2) , size = sum(length(birth1), length(birth2)) , prob = p_grid) #relative number of ways to get six W's, holding p at 0.5 and n at nine

# compute product of likelihood and prior

unstd.posterior4 <- likelihood4 * prior

# standardize the posterior, so it sums to 1

posterior4 <- unstd.posterior4/ sum(unstd.posterior4)


plot(p_grid ,
     posterior4 ,
     type = "b" ,
     
     xlab = "probability of water" ,
     ylab = "posterior probability")

mtext(paste(grid_size, "points"))

# 3H2 #### 
# draw 10,000 random parameter values from the posterior distribution you calculated above. Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals.

samples4 <- sample(p_grid, size = 10000, prob = posterior4, replace = T)

rethinking::HPDI(samples4, prob = .5)

# 3H3 #### 

replicates <- rbinom(10000, 200, prob = samples4)
