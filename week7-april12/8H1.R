library(rethinking)
library(brms)
library(dplyr)
library(ggplot2)
library(tidybayes)

# 8H1.  Return to the data(tulips) example in the chapter. Now include the bed variable as a predictor in the interaction model. Don’t interact bed with the other predictors; just include it as a main effect. Note that bed is categorical. So to use it properly, you will need to either construct dummy variables or rather an index variable, as explained in Chapter 5.

data(tulips)

str(tulips)

tulips <- tulips |>
  mutate(light = 1-shade) |>
  mutate(blooms = scale(blooms),
         water = scale(water, scale = F),
         shade = scale(shade, scale = F),
         light = scale(light, scale = F))

str(tulips)

ggplot(tulips, aes(water, blooms, color = light)) +
  geom_jitter() +
  theme_bw() +
  facet_wrap(vars(bed))
# 
# tulips_bed <- brm(blooms ~ 0 + water*light + bed,
#                 data = tulips,
#                 prior = c(prior(normal(0, 0.25), class = b),
#                           prior(exponential(1), class = sigma)))
# 
# tulips_nobed <- brm(blooms ~ 0 + water*light,
#                     data = tulips,
#                     prior = c(prior(normal(0, 0.25), class = b),
#                               prior(exponential(1), class = sigma)))
# 
# save(tulips_bed, tulips_nobed, file = "week7-april12/tulips_models.RData")

load("week7-april12/tulips_models.RData")

tulips_bed_newdat <- expand.grid(light = c(-1, 0, 1),
                                 water= c(-1, 0, 1)) 

tulips_bed_newdat <- bind_rows(tulips_bed_newdat,
                               tulips_bed_newdat,
                               tulips_bed_newdat) |>
  mutate(bed = sort(rep(unique(tulips$bed), 9))) |>
  mutate(light = scale(light, scale = F),
         water = scale(water, scale = F))

tulips_bed_pp <- add_epred_draws(tulips_bed_newdat, object = tulips_bed)

ggplot(tulips_bed_pp, aes(water, .epred, group = .draw)) +
  geom_line(alpha = .2) +
  facet_grid(rows = vars(bed), cols = vars(light)) +
  theme_bw()

summary(tulips_bed)


# 8H2

waic(tulips_bed, tulips_nobed, compare =T)

loo(tulips_bed, tulips_nobed, compare =T)

