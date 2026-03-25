## -------------------------------------------------------------------------------------------------------------
#| warning: false
#| message: false
#| code-summary: "Load libraries"

library(dplyr)
library(INLA)
library(ggplot2)
library(patchwork)
library(inlabru)     
# load some libraries to generate nice plots
library(scico)




## -------------------------------------------------------------------------------------------------------------
#| code-summary: "Simulate data from a LMM"
#| 
set.seed(12)
beta = c(1.5,1)
sd_error = 1
tau_group = 1

n = 100
n.groups = 5
x = rnorm(n)
v = rnorm(n.groups, sd = tau_group^{-1/2})
y = beta[1] + beta[2] * x + rnorm(n, sd = sd_error) +
  rep(v, each = 20)

df = data.frame(y = y, x = x, j = rep(1:5, each = 20))  


## ----plot_data_lmm--------------------------------------------------------------------------------------------
#| code-fold: true
#| fig-cap: Data for the linear mixed model example with 5 groups
#| fig-align: center
#| fig-width: 4
#| fig-height: 4

ggplot(df) +
  geom_point(aes(x = x, colour = factor(j), y = y)) +
  theme_classic() +
  scale_colour_discrete("Group")



## ----define_components_lmm------------------------------------------------------------------------------------
# Define model components
cmp =  ~ -1 + beta_0(1) + beta_1(x, model = "linear") +
  u(j, model = "iid")


## ----define_likelihood_lmm------------------------------------------------------------------------------------
# Construct likelihood
lik =  bru_obs(formula = y ~.,
            family = "gaussian",
            data = df)


## -------------------------------------------------------------------------------------------------------------
#| collapse: true
#| code-summary: "Fit a LMM in inlabru"
fit = bru(cmp, lik)
summary(fit)


## -------------------------------------------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "LMM fitted values"
#| fig-align: center
#| fig-width: 4
#| fig-height: 4

# New data
xpred = seq(range(x)[1], range(x)[2], length.out = 100)
j = 1:n.groups
pred_data = expand.grid(x = xpred, j = j)
pred = predict(fit, pred_data, formula = ~ beta_0 + beta_1 + u) 


pred %>%
  ggplot(aes(x=x,y=mean,color=factor(j)))+
  geom_line()+
  geom_ribbon(aes(x,ymin = q0.025, ymax= q0.975,fill=factor(j)), alpha = 0.5) + 
  geom_point(data=df,aes(x=x,y=y,colour=factor(j)))+
  facet_wrap(~j)


