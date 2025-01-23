library(brms)
library(cmdstanr)
library(dplyr)
rm(list=ls())
load("data/empirical_data/data_analysis/df_all.rdata")

myprior = prior(normal(0, 0.2),  class = b)
df$reward_oneback=factor(df$reward_oneback)
# main -------------------------------------------------------
m_unch_sgs <-
  brm(
    formula=stay_unch_shape~0+Intercept+reward_oneback*sgs_score+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    prior=myprior,
    backend = "cmdstanr"
  )
save(m_unch_sgs,file="data/regression/unch_sgs.Rdata")

m_unch_ocir <-
  brm(
    formula=stay_unch_shape~0+Intercept+reward_oneback*oci_score+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )

m_oil <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(m_oil,file="data/regression/oil.Rdata")

m_oil_session <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback*session+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(m_oil_session,file="data/regression/oil_session.Rdata")

m_oil_aq <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback*aq_score+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F,aq_score>0),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(m_oil_aq,file="data/regression/oil_aq.Rdata")










