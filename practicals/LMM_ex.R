## -----------------------------------------------------------------------------
#| warning: false
#| message: false
#| code-summary: "Load libraries"

library(INLA)
library(patchwork)
library(inlabru)
library(tidyverse)
# load some libraries to generate nice plots
library(scico)


## -----------------------------------------------------------------------------
#| echo: true

bru_options_set(control.compute = list(dic = T, waic = T))




## -----------------------------------------------------------------------------
#| code-fold: show
#| code-summary: "Get body_mass data"

data("penguins")
glimpse(penguins)




## -----------------------------------------------------------------------------
penguins$body_mass = penguins$body_mass/1000


## -----------------------------------------------------------------------------
penguins = penguins %>%  drop_na()












## -----------------------------------------------------------------------------
#| eval: true

fit1 = bru(cmp, lik)



## -----------------------------------------------------------------------------
#| code-summary: "Model summaries"
#| collapse: true
summary(fit1)




## -----------------------------------------------------------------------------
new_data = data.frame(flipper_len  = 170:240)

pred = predict(fit1, new_data, ~ effects,
               n.samples = 1000)


## -----------------------------------------------------------------------------
#| code-fold: true
#| fig-cap: Data and 95% credible intervals
#| message: false
#| warning: false
#| fig-align: center
#| fig-width: 4
#| fig-height: 4

pred %>% ggplot() +
  geom_line(aes(flipper_len,mean)) +
  geom_ribbon(aes(flipper_len, ymin = q0.025, ymax = q0.975), alpha = 0.5) +
  xlab("Flipper length") + ylab("body_mass") +
  geom_point(data = penguins, aes(flipper_len, body_mass))



## -----------------------------------------------------------------------------
new_data = data.frame(flipper_len  = 170:240)

pred1 = predict(fit1, new_data,
               formula = ~ { eta = effects
                             sigma = sqrt(1/Precision_for_the_Gaussian_observations)
                             list(mean = eta,
                                  q1 = qnorm(0.025, mean = eta, sd = sigma),
                                  q2 =  qnorm(0.975, mean = eta, sd = sigma))  
                            },
               n.samples = 1000)



## -----------------------------------------------------------------------------
#| code-fold: true
#| fig-cap: Data and 95% credible intervals
#| message: false
#| warning: false
#| fig-align: center
#| fig-width: 4
#| fig-height: 4

ggplot() +
  geom_line(data = pred, aes(flipper_len,mean)) +
  geom_ribbon(data = pred,aes(flipper_len, ymin = q0.025, ymax = q0.975), alpha = 0.5) +
  geom_line(data = pred1$mean, aes(flipper_len,mean), color = "red") +
  geom_line(data = pred1$q1,aes(flipper_len, mean),
               color = "red") +
   geom_line(data = pred1$q2,aes(flipper_len, mean),
               color = "red") +
  xlab("flipper length") + ylab("body_mass") +
  geom_point(data = penguins, aes(flipper_len, body_mass))






## -----------------------------------------------------------------------------
#| code-fold: true
penguins %>% 
  ggplot() + geom_point(aes(flipper_len, body_mass, color= sex)) +
  facet_wrap(.~sex)


## -----------------------------------------------------------------------------
#| eval: true

cmp = ~ -1 + effects( ~ sex*flipper_len, model = "fixed")
formula = body_mass ~ .
lik = bru_obs(formula = formula,
              data = penguins
              )
fit2 = bru(cmp, lik)



## -----------------------------------------------------------------------------
fit2$summary.random




## -----------------------------------------------------------------------------
#| code-fold: show

# Here we use the _latent "trick" to recover the parameters
# here we do not need any new data to predict for, we can use an
# empty data frame

params = predict(fit2, data.frame() ,
   ~ data.frame( intercept_female = effects_latent[1],
                 intercept_male = effects_latent[1] + effects_latent[2],
                 slope_female = effects_latent[3] ,
                 slope_male = effects_latent[3] + effects_latent[4]))



## -----------------------------------------------------------------------------
cmp = ~ -1 + sex_intercept(sex, model = "iid", initial = log(0.001), fixed = T) +
  sex_slope(sex, flipper_len,  model = "iid", fixed = T, initial = log(0.001))

lik = bru_obs(formula = body_mass ~ .,
              data = penguins)

fit2b = bru(cmp, lik)


## -----------------------------------------------------------------------------
fit2b$summary.random$sex_intercept
fit2b$summary.random$sex_slope


## -----------------------------------------------------------------------------
#| echo: true
#| code-fold: true
penguins %>%
  ggplot() + 
  geom_point(aes(flipper_len, body_mass, color = species)) + 
  facet_wrap(.~sex)



## -----------------------------------------------------------------------------
#| echo: true
#| eval: false
# cmp = ~ -1 + effects( ~ sex*flipper_len, model = "fixed") +
#   species(species, ... )
# formula = ...
# lik = bru_obs(formula = formula,
#               data = penguins
#               )
# fit3 = bru(cmp, lik)




## -----------------------------------------------------------------------------
fit3$summary.random$effects[,c(1,3,5)]
fit2$summary.random$effects[,c(1,3,5)]



## -----------------------------------------------------------------------------
#| echo: true
#| code-fold: true

# Predict
pred = predict(fit3, penguins, formula = ~ effects + species) 

pred %>%
  ggplot(aes(x=flipper_len,y=mean,color=factor(sex)))+
  geom_line()+
  geom_ribbon(aes(flipper_len,ymin = q0.025, ymax= q0.975,fill=factor(sex)), alpha = 0.5) + 
  geom_point(data=penguins,aes(x=flipper_len,y=body_mass,colour=factor(sex)))+
  facet_wrap(~species,scales="free_x")



## -----------------------------------------------------------------------------
#| echo: true
#| eval: false

# 
# cmp = ~ -1 + effects( ~ sex*flipper_len, model = "fixed") +
#   species1(species, ... ) +
#   species2(species, ... )
# 
# formula = ...
# 
# lik = bru_obs(formula = formula,
#               data = penguins
#               )
# 
# fit4 = bru(cmp, lik)




## -----------------------------------------------------------------------------
deltaIC(fit1, fit2, fit3, fit4, criterion = c("DIC","WAIC"))


