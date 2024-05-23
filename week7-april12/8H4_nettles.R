library(rethinking)
library(brms)
library(dplyr)
library(ggplot2)
library(tidybayes)


data(nettle)

nettle <- nettle |>
  mutate(lang_per_cap = num.lang / k.pop) |>
  mutate(log_lang_per_cap = log(lang_per_cap),
         log_area = log(area)) |>
  mutate(across(where(is.double),
                scale))

# a. Evaluate the hypothesis that language diversity, as measured by log(lang.per.cap), 
# is positively associated with the average length of the growing season, 
# mean.growing.season. 
# Consider log(area) in your regression(s) as a covariate (not an interaction). 
# Interpret your results. 

ggplot(nettle, aes(log_area, log_lang_per_cap)) +
  geom_point() +
  scale_color_viridis_c()

hist(nettle$log_lang_per_cap)
hist(nettle$mean.growing.season)
hist(nettle$sd.growing.season)
hist(nettle$log_area)

# question: appropriate priors?
# I might default to normal, but others may be better suited.
# Note from online: Go ahead and standardize everything to make priors simpler
nettlea <- brm(log_lang_per_cap ~ mean.growing.season + log_area,
               data = nettle,
               family = gaussian(),
               prior = c(prior(normal(0, 0.5), class = b),
                         prior(normal(0, 0.2), class = Intercept),
                         prior(exponential(1), class = sigma)))

plot(nettlea)


nettle_newdat <- data.frame(
  mean.growing.season = seq(-2, 2, by = .1),
  log_area = seq(-2, 2, by = .1),
  sd.growing.season = seq(-2, 2, by = .1)
) |>
  tidyr::expand(mean.growing.season,
                log_area,
                sd.growing.season)

nettle_newdat_mean_log <- nettle_newdat |>
  select(-sd.growing.season) |>
  distinct()

nettlea_preds <- nettle_newdat_mean_log %>% 
  add_epred_draws(nettlea, ndraws = 100) %>% 
  tidybayes::mean_qi(.width = 0.97)

ggplot(nettlea_preds, aes(mean.growing.season, .epred, color = log_area)) +
  geom_point()

# (b) Now evaluate the hypothesis that language diversity is 
# negatively associated with the standard deviation of length of 
# growing season, sd.growing.season. 
# This hypothesis follows from uncertainty in harvest favoring 
# social insurance through larger social networks and therefore 
# fewer languages. Again, consider log(area) as a covariate 
# (not an interaction). Interpret your results. 

nettleb <- brm(log_lang_per_cap ~ sd.growing.season + log_area,
               data = nettle,
               family = gaussian(),
               prior = c(prior(normal(0, 0.5), class = b),
                         prior(normal(0, 0.2), class = Intercept),
                         prior(exponential(1), class = sigma)))

plot(nettleb)


nettle_newdat_sd_log <- nettle_newdat |>
  select(-mean.growing.season) |>
  distinct()

nettleb_preds <- nettle_newdat_sd_log %>% 
  add_epred_draws(nettleb, ndraws = 100) %>% 
  tidybayes::mean_qi(.width = 0.97)

ggplot(nettleb_preds, aes(sd.growing.season, .epred, color = log_area)) +
  geom_point()

#' (c) Finally, evaluate the hypothesis that mean.growing.season and 
#' sd.growing.season interact to synergistically reduce language 
#' diversity. 
#' The idea is that, in nations with longer average growing seasons,
#'  high variance makes storage and redistribution even more important 
#'  than it would be otherwise. 
#'  That way, people can cooperate to preserve and protect windfalls 
#'  to be used during the droughts.
#'  

nettlec <- brm(log_lang_per_cap ~ mean.growing.season*sd.growing.season + log_area,
               data = nettle,
               family = gaussian(),
               prior = c(prior(normal(0, 0.5), class = b),
                         prior(normal(0, 0.2), class = Intercept),
                         prior(exponential(1), class = sigma)))

plot(nettlec)

nettlec_preds <- nettle_newdat %>% 
  add_epred_draws(nettlec, ndraws = 100) %>% 
  tidybayes::mean_qi(.width = 0.97)


ggplot(
  nettlec_preds |> filter(log_area == -1.5),
  aes(
    mean.growing.season,
    .epred,
    color = sd.growing.season,
    group = sd.growing.season
  )
) +
  geom_line()


ggplot(
  nettlec_preds |> filter(log_area == 0),
  aes(
    mean.growing.season,
    .epred,
    color = sd.growing.season,
    group = sd.growing.season
  )
) +
  geom_line() +
  scale_color_viridis_c() +
  geom_point(data = nettle, inherit.aes = F, aes(mean.growing.season, log_lang_per_cap, color = sd.growing.season))

ggplot(nettlec_tidy, aes(log_area, pred_mu, color = sd.growing.season)) +
  geom_point() 
