library(rethinking)
library(brms)
library(dplyr)
library(ggplot2)
library(tidybayes)

data(foxes)
# Revisit the urban fox data, data(foxes), from the previous chapter’s practice problems. 
# Use WAIC or PSIS based model comparison on five different models, each using weight as the outcome, and containing these sets of predictor variables:
#   
#   (1)  avgfood + groupsize + area
# 
# (2)  avgfood + groupsize
# 
# (3)  groupsize + area
# 
# (4)  avgfood
# 
# (5)  area
# PSIS: https://paul-buerkner.github.io/brms/reference/loo.brmsfit.html
# WAIC: https://paul-buerkner.github.io/brms/reference/waic.brmsfit.html


foxes_standardized <- foxes |>
  select(-group) |>
  mutate(across(everything(), scale))

m1 <- brm(weight ~ avgfood + groupsize + area,
          data = foxes_standardized,
          family = gaussian(),
          prior = c(prior(normal(0, 0.5), class = b),
                            prior(normal(0, 0.2), class = Intercept),
                            prior(exponential(1), class = sigma)))

m2 <- brm(weight ~ avgfood + groupsize,
          data = foxes_standardized,
          family = gaussian(),
          prior = c(prior(normal(0, 0.5), class = b),
                    prior(normal(0, 0.2), class = Intercept),
                    prior(exponential(1), class = sigma)))

m3 <- brm(weight ~ groupsize + area,
          data = foxes_standardized,
          family = gaussian(),
          prior = c(prior(normal(0, 0.5), class = b),
                    prior(normal(0, 0.2), class = Intercept),
                    prior(exponential(1), class = sigma)))

m4 <- brm(weight ~ avgfood,
          data = foxes_standardized,
          family = gaussian(),
          prior = c(prior(normal(0, 0.5), class = b),
                    prior(normal(0, 0.2), class = Intercept),
                    prior(exponential(1), class = sigma)))


m5 <- brm(weight ~ area,
          data = foxes_standardized,
          family = gaussian(),
          prior = c(prior(normal(0, 0.5), class = b),
                    prior(normal(0, 0.2), class = Intercept),
                    prior(exponential(1), class = sigma)))

save(m1, m2, m3, m4, m5, file = "week7-april12/fox_models.RData")

fox_waic <- waic(m1, m2, m3, m4, m5, compare = T)

fox_waics <- lapply(fox_waic$loos, (\(x) as.data.frame(x$estimates))) |>
  bind_rows(.id = "model") 

fox_waics <- fox_waics |>
  mutate(var = row.names(fox_waics)) |>
  filter(!grepl("elpd", var)) |>
  mutate(var = substr(var, 1, nchar(var) - 4)) |>
  mutate(var = ifelse(var == "waic.", "waic", var)) |>
  left_join(
    data.frame(model = c("m1",
                         "m2",
                         "m3",
                         "m4",
                         "m5"),
               formula = c("avgfood + groupsize + area",
                           "avgfood + groupsize",
                           "groupsize + area",
                           "avgfood",
                           "area"))
  ) |>
  left_join(mutate(as.data.frame(fox_waic$diffs), model = row.names(fox_waic$diffs)))

ggplot(filter(fox_waics, var == "waic"), aes(Estimate, model)) +
  geom_point() +
  geom_errorbarh(aes(xmin = Estimate - SE, xmax = Estimate + SE)) +
  geom_errorbarh(aes(xmin = Estimate - se_diff, xmax = Estimate + se_diff), color = "red") +
  geom_label(aes(label = formula), nudge_y = .25)

fox_psis <- loo(m1, m2, m3, m4, m5, compare = T)


fox_psises <- lapply(fox_psis$loos, (\(x) as.data.frame(x$estimates))) |>
  bind_rows(.id = "model") 

fox_psises <- fox_psises |>
  mutate(var = row.names(fox_psises)) |>
  filter(!grepl("elpd", var)) |>
  mutate(var = substr(var, 1, nchar(var) - 4)) |>
  mutate(var = ifelse(var == "looic.", "looic", var)) |>
  left_join(
    data.frame(model = c("m1",
                         "m2",
                         "m3",
                         "m4",
                         "m5"),
               formula = c("avgfood + groupsize + area",
                           "avgfood + groupsize",
                           "groupsize + area",
                           "avgfood",
                           "area"))
  )

ggplot(filter(fox_psises, var == "looic"), aes(Estimate, model)) +
  geom_point() +
  geom_errorbarh(aes(xmin = Estimate - SE, xmax = Estimate + SE)) +
  geom_label(aes(label = formula), nudge_y = .25)

