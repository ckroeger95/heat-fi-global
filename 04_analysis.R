
##############################################################################@

# Author: Carolin Kroeger
# Github: ckroeger95

# Date: March 2023

# 04_analysis: Generate the main results reported in the paper

##############################################################################@
# Set up  --------------------------------------------------------------------
##############################################################################@

##############################################################################@
 #### 1 | Libraries & data ---------------------------------------------------
##############################################################################@

# Load libraries

library(sf)
library(ggplot2)
library(countrycode)
library(tidyverse)
library(lme4)
library(lmerTest) 
library(stringdist)
library(RColorBrewer)
library(stargazer)
library(mediation)
library(stringi)
library(CBPS)
library(viridis)
library(margins)
library(lmtest)
library(cobalt)
library(broom.mixed)
library(cowplot)
library(multiwayvcov)
library(ggpubr)
library(ggeffects)
library(jtools)
library(raster)

##############################################################################@
#### 2 | Load data --------------------------------------------
##############################################################################@

# These are the results of code in this file that takes a long time to run 
# I have indicated with a note where code takes long time to run

# Load map at country level (layer 0)
countries <- 
  read_sf(dsn='data/map', layer='gadm36_0') %>% 
  mutate(ISO_3 = countrycode(NAME_0, origin='country.name', destination='iso3c'), 
         ISO_3 = ifelse(NAME_0=='Kosovo', 'XKO', ISO_3),
         ISO_3 = ifelse(NAME_0=='Northern Cyprus', 'CYP2', ISO_3)) %>% 
  filter(NAME_0 != 'Antarctica')

data <- 
  read_csv('time-savers/data_final.csv') %>% 
  mutate(children = as.factor(children), 
         partner = as.factor(partner), 
         year = as.factor(year), 
         gender = as.factor(gender), 
         area = as.factor(area))

states <- 
  read_sf(dsn='data/map', layer='gadm36_1') %>% 
  mutate(ISO_3 = countrycode(NAME_0, origin='country.name', destination='iso3c'), 
         ISO_3 = ifelse(NAME_0=='Kosovo', 'XKO', ISO_3),
         ISO_3 = ifelse(NAME_0=='Northern Cyprus', 'CYP2', ISO_3)) %>% 
  filter(NAME_0 != 'Antarctica')

# Remove NAs from data set
CBPS_data <- 
  data %>% 
  drop_na(hot_week_10P, 
          area, children,gender,age,partner, 
          utci.avg.state,log_precip_week, hotdays_year, 
          year) %>% 
  mutate(area = as.factor(area), 
         gender = as.factor(gender), 
         year = as.factor(year),
         children = as.factor(children), 
         partner = as.factor(partner), 
         hot_week_10P = as.factor(hot_week_10P)  )

# CBPS 
load('time-savers/cbps_base.rda')

# Results mediation analysis 
results_mediation <- read_csv('time-savers/results_mediation_separatedata.csv', show_col_types = F)

# Average marginal effects 
margins_ve <- read_csv('time-savers/margins_ve.csv', show_col_types = F)
margins_gni <- read_csv(file='time-savers/margins_gni.csv', show_col_types = F)
margins_ae <- read_csv(file='time-savers/margins_ae.csv', show_col_types = F)
margins_wl <- read_csv('time-savers/margins_wl.csv', show_col_types = F)

# Sensitivity analysis for covariates
df_drop <- read_csv('time-savers/df_drop.csv')


##############################################################################@
# Results ------------------------------------------------------------------
##############################################################################@

# This section has three parts
  # 1) Base models
  # 2) Mediation analysis 
  # 3) Moderation analysis

# Rationale for each section 
  # 1) The goal is to find out whether and in what time time period heat affects
       # food insecurity (1). 
  # 2) I then explore the extent to which the effect of a hot week on food 
       # insecurity is mediated by income, health, and employment as workers 
       # struggle to work and earn an income when it gets hot.
  # 3) I then conduct a moderator analysis to test which regions are more affected 
       # given socio-economic variables, such as GNI per capita. 

##############################################################################@
#### 1 | Main models ----------------------------------------------------------
##############################################################################@

# No controls
##############################################################################@

model_noc <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    (1 | country.mix/region.mix), 
  data=data) 
summary(model_noc) 
df.residual(model_noc)

# Controls
##############################################################################@

model_base <- lme4::lmer(
  fi_binary ~ hot_week_10P + # heat indicator
      area + children + age + partner + gender + # individual covariates 
      log_precip_week + utci.avg.state + hotdays_year + # heat controls 
      year + # year fixed effects 
      (1 | country.mix/region.mix), # intercept is random (flexible) across states nested within countries
      data=data) 
summary(model_base) 
df.residual(model_base)

# CBPS weights
##############################################################################@

model_cbps <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + 
    year +
    (1 | country.mix/region.mix), 
  data=CBPS_data %>% mutate(hot_week_10P = as.numeric(hot_week_10P)), # makes no difference to results and facilitates export of model
  weights=cbps_base$weights ) 
summary(model_cbps)
df.residual(model_cbps)


#confint(model_cbps, parm = 'hot_week_10P', level =0.95, method ='Wald')

# Effects by day
##############################################################################@

# Day effects
model_base_days <- lme4::lmer(
  fi_binary ~ as.factor(hotdays_week_10P) + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + 
    year +  
    (1 | country.mix/region.mix), 
  data=data, control=lmerControl(optimizer = "bobyqa"))
summary(model_base_days)
df.residual(model_base_days)

# Mild, moderate, severe models
##############################################################################@

model_moderate <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    area + children + age + partner + gender + log_precip_week + utci.avg.state + hotdays_year + 
    year + (1 | country.mix/region.mix), 
  data=data) 

model_moderate_cbps <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    area + children + age + partner + gender + log_precip_week + utci.avg.state + hotdays_year + 
    year + (1 | country.mix/region.mix), 
  data=CBPS_data %>% mutate(hot_week_10P = as.numeric(hot_week_10P)), weights=cbps_base$weights) 

model_severe <- lme4::lmer(
  severe_fi ~ hot_week_10P + 
    area + children + age + partner + gender + log_precip_week + utci.avg.state + hotdays_year + 
    year + (1 | country.mix/region.mix), 
  data=data)
df.residual(model_severe)

model_severe_cbps <- lme4::lmer(
  severe_fi ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + 
    year + (1 | country.mix/region.mix), 
  data=CBPS_data %>% mutate(hot_week_10P = as.numeric(hot_week_10P)), weights=cbps_base$weights) 

model_mild <- lme4::lmer(
  mild_fi ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + 
    year + (1 | country.mix/region.mix), 
  data=data) 

model_mild_cbps <- lme4::lmer(
  mild_fi ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + 
    year + (1 | country.mix/region.mix), 
  data=CBPS_data %>% mutate(hot_week_10P = as.numeric(hot_week_10P)), weights=cbps_base$weights, control=lmerControl(optimizer = "bobyqa")) 


##############################################################################@
#### 2 | Moderation --------------------------------------
##############################################################################@

# Models can be fitted within a minute, but margins take longer to calculate and 
# have been loaded through time savers already 

# Agricultural employment 
##############################################################################@

model_ae <- lmerTest::lmer(
  fi_binary ~ hot_week_10P*ag.employ + log_gni + #urbanicity + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), 
  data=data, 
  control=lmerControl(optimizer = "bobyqa") ) 
summary(model_ae)
df.residual(model_ae)

# GNI 
##############################################################################@

model_gni <- lme4::lmer(
  fi_binary ~ hot_week_10P*log_gni + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data=data) 
summary(model_gni)

# Vulnerable employment 
##############################################################################@

model_ve <- lmerTest::lmer(
  fi_binary ~ hot_week_10P*vulnerable.employment + log_gni + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data=data) 
summary(model_ve)
df.residual(model_ve)

# Margins
##############################################################################@

# Time saver
# The margins commands take longer to run and results were pre-loaded in the set up

# What are we doing here? 
# What margins does is to calculates the average marginal effects of a 
# hot week on food insecurity at different levels of each moderator. 
# The 'threshold' margins calculate at which point the effect becomes positive
# and significant 

##### Agricultural employment

margins_ae <- 
  model_ae %>% 
  margins(variable='hot_week_10P',at=list(ag.employ=c(10, 30, 50, 70))) %>% summary()
write_csv(margins_ae, file='time-savers/margins_ae.csv')

margins_ae_threshold <- 
  model_ae %>% 
  margins(variable='hot_week_10P', at=list(ag.employ=c(21,22,23,24))) %>% summary()
write_csv(margins_ae_threshold, file='time-savers/margins_ae_threshold.csv')

##### GNI

margins_gni <- model_gni %>% 
  margins(variable='hot_week_10P', at=list(log_gni=c(7, 8, 9, 10))) %>% summary() 
write_csv(margins_gni, file='time-savers/margins_gni.csv')

margins_gni_threshold <- model_gni %>% 
  margins(variable='hot_week_10P', at=list(log_gni=c(9.01, 9.02))) %>% summary() 
write_csv(margins_gni_threshold, file='time-savers/margins_gni_threshold.csv')

##### Vulnerable employment

margins_ve <- model_ve %>% 
  margins(variable='hot_week_10P', at=list(vulnerable.employment=c(10, 30, 50, 70))) %>% summary()
write_csv(margins_ve, file='time-savers/margins_ve.csv')

margins_ve_threshold <- model_ve %>% 
  margins(variable='hot_week_10P', at=list(vulnerable.employment=c(31,32,33))) %>% summary()
write_csv(margins_ve_threshold, file='time-savers/margins_ve_threshold.csv')


##############################################################################@
#### 3 | Mediation  -------------------------------------------
##############################################################################@

# Time saver
# Takes time to run, so results of this section were already loaded in the set up

# Employment
##############################################################################@
  
m_emp <- lme4::lmer(
  employment_7days ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix), 
  data = data %>% drop_na(fi_binary, employment_7days, hot_week_10P)) 
summary(m_emp)

m_c_e <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    employment_7days +
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix), data = data %>% drop_na(fi_binary, employment_7days, hot_week_10P) ) 
summary(m_c_e)
df.residual(m_c_e)

med.emp <- 
  mediation::mediate(
    m_emp, m_c_e, 
    treat = 'hot_week_10P', mediator = 'employment_7days', 
    robustSE = FALSE, boot=F, sims=100, dropobs=T) 
# boot must be F for lmer models used 
# dropobs = T so that common rows only are used 

summary(med.emp)


# Income
##############################################################################@

m_income <- lme4::lmer(
  log_income_2 ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix), data = data %>% drop_na(fi_binary, log_income_2, hot_week_10P) ) 

m_c_i <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    log_income_2 + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix), data = data %>% drop_na(fi_binary, log_income_2, hot_week_10P) ) 
df.residual(m_c_i)

med.income <- 
  mediation::mediate(m_income, m_c_i, 
          treat = 'hot_week_10P', 
          mediator = 'log_income_2', 
          robustSE = FALSE, boot=F, sims=100, dropobs=T) 
summary(med.income)

# Health
##############################################################################@

m_health <- lme4::lmer(
  health_problems ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix), data = data %>% drop_na(fi_binary, health_problems, hot_week_10P) ) 

m_c_h <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    health_problems + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix), data = data %>% drop_na(fi_binary, health_problems, hot_week_10P) ) 
df.residual(m_c_h)

med.health <- 
  mediation::mediate(m_health, m_c_h, 
          treat = 'hot_week_10P', 
          mediator = 'health_problems', 
          robustSE = FALSE, boot=F, sims=100, dropobs=T) 
summary(med.health)

# Difficulties HH income
##############################################################################@

m_HH <- lme4::lmer(
  feeling_HH_income ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix),data = data %>% drop_na(fi_binary, feeling_HH_income, hot_week_10P) ) 

m_c_HH <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    feeling_HH_income + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix), data = data %>% drop_na(fi_binary, feeling_HH_income, hot_week_10P) ) 
df.residual(m_c_HH)

med.feeling <- 
  mediation::mediate(m_HH, m_c_HH, 
          treat = 'hot_week_10P', 
          mediator = 'feeling_HH_income', 
          robustSE = FALSE, boot=F, sims=100, dropobs=T)
summary(med.feeling)


# Job
##############################################################################@

m_job <- lme4::lmer(
  local_job_market ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix), data = data %>% drop_na(fi_binary, local_job_market, hot_week_10P) ) 

m_c_j <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    local_job_market + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix), data = data %>% drop_na(fi_binary, local_job_market, hot_week_10P) ) 
df.residual(m_c_j)

med.job <- 
  mediation::mediate(m_job, m_c_j, 
          treat = 'hot_week_10P', 
          mediator = 'local_job_market', 
          robustSE = F, boot=F, sims=100, dropobs=T) 
summary(med.job)

med.job %>% tidy()

# Store results in a data frame 
##############################################################################@

results_med.feeling <-  
  as.data.frame(  rbind (
    ACME =  cbind(med.feeling$d.avg,med.feeling$d.avg.ci[[1]], med.feeling$d.avg.ci[[2]],med.feeling$d.avg.p), 
    ADE =   cbind(med.feeling$z.avg,med.feeling$z.avg.ci[[1]], med.feeling$z.avg.ci[[2]],med.feeling$z.avg.p), 
    total = cbind(med.feeling$tau.coef, med.feeling$tau.ci[[1]], med.feeling$tau.ci[[2]], med.feeling$tau.p), 
    prop =  cbind(med.feeling$n.avg, NA, NA, NA),
    obs =   cbind(med.feeling$nobs, NA, NA, NA),
    sim =   cbind(med.feeling$sims, NA, NA, NA)  )  ) %>% 
  mutate(model = 'Difficulties getting by on income', 
         name = c('Average causal mediated effect', 'Direct effect', 'Total effect', 'Proportion mediated', 'Observations', 'Simulations')) 
colnames(results_med.feeling) <- c('estimate', 'lower', 'upper', 'pvalue', 'model', 'name')

results_med.emp <-  
  as.data.frame(  rbind (
    ACME =  cbind(med.emp$d.avg,med.emp$d.avg.ci[[1]], med.emp$d.avg.ci[[2]],med.emp$d.avg.p), 
    ADE =   cbind(med.emp$z.avg,med.emp$z.avg.ci[[1]], med.emp$z.avg.ci[[2]],med.emp$z.avg.p), 
    total = cbind(med.emp$tau.coef, med.emp$tau.ci[[1]], med.emp$tau.ci[[2]], med.emp$tau.p), 
    prop =  cbind(med.emp$n.avg, NA, NA, NA),
    obs =   cbind(med.emp$nobs, NA, NA, NA),
    sim =   cbind(med.emp$sims, NA, NA, NA)  )  ) %>% 
  mutate(model = 'Employment', 
         name = c('Average causal mediated effect', 'Direct effect', 'Total effect', 'Proportion mediated', 'Observations', 'Simulations')) 
colnames(results_med.emp) <- c('estimate', 'lower', 'upper', 'pvalue', 'model', 'name')

results_med.income <-  
  as.data.frame(  rbind (
    ACME = cbind(med.income$d.avg,med.income$d.avg.ci[[1]], med.income$d.avg.ci[[2]],med.income$d.avg.p), 
    ADE = cbind(med.income$z.avg,med.income$z.avg.ci[[1]], med.income$z.avg.ci[[2]],med.income$z.avg.p), 
    total = cbind(med.income$tau.coef, med.income$tau.ci[[1]], med.income$tau.ci[[2]], med.income$tau.p), 
    prop = cbind(med.income$n.avg, NA, NA, NA),
    obs = cbind(med.income$nobs, NA, NA, NA),
    sim = cbind(med.income$sims, NA, NA, NA)  )  ) %>% 
  mutate(model = 'Annual income', 
         name = c('Average causal mediated effect', 'Direct effect', 'Total effect', 'Proportion mediated', 'Observations', 'Simulations')) 
colnames(results_med.income) <- c('estimate', 'lower', 'upper', 'pvalue', 'model', 'name')

results_med.health <-  
  as.data.frame(  rbind (
    ACME = cbind(med.health$d.avg,med.health$d.avg.ci[[1]], med.health$d.avg.ci[[2]],med.health$d.avg.p), 
    ADE = cbind(med.health$z.avg,med.health$z.avg.ci[[1]], med.health$z.avg.ci[[2]],med.health$z.avg.p), 
    total = cbind(med.health$tau.coef, med.health$tau.ci[[1]], med.health$tau.ci[[2]], med.health$tau.p), 
    prop = cbind(med.health$n.avg, NA, NA, NA),
    obs = cbind(med.health$nobs, NA, NA, NA),
    sim = cbind(med.health$sims, NA, NA, NA)  )  ) %>% 
  mutate(model = 'Health', 
         name = c('Average causal mediated effect', 'Direct effect', 'Total effect', 'Proportion mediated', 'Observations', 'Simulations')) 
colnames(results_med.health) <- c('estimate', 'lower', 'upper', 'pvalue', 'model', 'name')

results_med.job <-  
  as.data.frame(  rbind (
    ACME = cbind(med.job$d.avg,med.job$d.avg.ci[[1]], med.job$d.avg.ci[[2]],med.job$d.avg.p), 
    ADE = cbind(med.job$z.avg,med.job$z.avg.ci[[1]], med.job$z.avg.ci[[2]],med.job$z.avg.p), 
    total = cbind(med.job$tau.coef, med.job$tau.ci[[1]], med.job$tau.ci[[2]], med.job$tau.p), 
    prop = cbind(med.job$n.avg, NA, NA, NA),
    obs = cbind(med.job$nobs, NA, NA, NA),
    sim = cbind(med.job$sims, NA, NA, NA)  )  ) %>% 
  mutate(model = 'Job market', 
         name = c('Average causal mediated effect', 'Direct effect', 'Total effect', 'Proportion mediated', 'Observations', 'Simulations')) 
colnames(results_med.job) <- c('estimate', 'lower', 'upper', 'pvalue', 'model', 'name')

results_mediation <- 
  rbind(results_med.income, 
        results_med.health, results_med.job, results_med.emp, results_med.feeling)

# Save results in time saver 
write_csv(results_mediation, file='time-savers/results_mediation_separatedata.csv')

##############################################################################@
# Exhibits -------------------------------------------
##############################################################################@

#### Goal Section 3
# This section calculates the plots to describe the data, visualise the main 
# results for regression models, the mediation analysis, and the moderator 
# analysis 

##############################################################################@
#### 1 | Exhibit 1 Descriptive heat maps -------------------------------------------
##############################################################################@

# Plot A) Food insecurity map across the world 
##############################################################################@

map_data <- 
  merge(
  countries, 
  data %>% group_by(ISO_3) %>% 
  summarize(share_fi = 100*mean(fi_binary, na.rm=T),
            share_hot_weeks = 100*mean(hot_week_10P, na.rm=T)), 
  by='ISO_3', all.x=T)

exhibit1_plotA <- 
  map_data %>% ggplot() + 
  geom_sf(aes(fill = share_fi), size = 0.00) +
  labs(x='Moderate-severe food insecurity (% of population)', y = '') +
  ggtitle('') +
  scale_fill_viridis(option='viridis', name='') +
  theme_classic() +  
  theme(legend.position='bottom', legend.box = 'horizontal', legend.key.size = unit(0.5, 'cm'),
        text = element_text(size = 14, family='Times New Roman'), 
        axis.title.x = element_text(size=14), 
        legend.text = element_text(size=14), 
        axis.line = element_line(color='white'),
        axis.text=element_blank(), 
        axis.ticks=element_blank())

# Plot B) Country-level food insecurity by heat
##############################################################################@

exhibit1_plotB <- 
  data %>% 
  group_by(percentile) %>% 
  summarize(fi_percentile = mean(fi_index, na.rm=T)) %>%
  ggplot(aes(x = percentile, y = fi_percentile)) +
    geom_point(aes(col = percentile)) + 
    geom_smooth(method = 'loess', col = 'black') + 
    scale_color_viridis(option = 'inferno', direction = -1, name = '') +
    labs(x='Percentile of year-round UTCI', y='Average food insecurity') +
    theme_classic() + 
  theme(legend.position = 'bottom', legend.box = 'horizontal', legend.key.size = unit(0.5, 'cm'),
        text=element_text(size = 14, family = 'Times New Roman'), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14), 
        legend.text = element_text(size=14))

plots <- align_plots(exhibit1_plotA, exhibit1_plotB, 
                     align = 'hv', axis = 'b')

exhibit1 <- plot_grid(plots[[1]], exhibit1_plotB,
                     labels = c('A', 'B'), align = 'hv', axis = 't',
                     rel_widths = c(1, 0.65), scale=0.9, rel_heights = c(1,1))

ggsave(exhibit1, file = 'output/exhibit1_descriptive.pdf', 
       width=15, height=5, device = cairo_pdf, dpi =1000)


##############################################################################@
#### 2 | Exhibit 2 CBPS and model severity ---------------------------------------
##############################################################################@

# Model severity
##############################################################################@

# Appendix figure 4b 

plot_severity <- 
  rbind(
    model_severe_cbps %>% tidy(effects = 'fixed', conf.int=T) %>% filter(term == 'hot_week_10P') %>% mutate(model = 'Severe FI', balance = 'CBPS'), 
    model_severe %>% tidy(effects = 'fixed', conf.int=T) %>% filter(term == 'hot_week_10P') %>% mutate(model = 'Severe FI', balance = 'None'), 
    model_moderate_cbps %>% tidy(effects = 'fixed', conf.int=T) %>% filter(term == 'hot_week_10P') %>% mutate(model = 'Moderate-to-severe FI', balance = 'CBPS'), 
    model_moderate %>% tidy(effects = 'fixed', conf.int=T) %>% filter(term == 'hot_week_10P') %>% mutate(model = 'Moderate-to-severe FI', balance = 'None'), 
    model_mild_cbps %>% tidy(effects = 'fixed', conf.int=T) %>% filter(term == 'hot_week_10P') %>% mutate(model = 'Mild-to-severe FI', balance = 'CBPS'),
    model_mild %>% tidy(effects = 'fixed', conf.int=T) %>% filter(term == 'hot_week_10P') %>% mutate(model = 'Mild-to-severe FI', balance = 'None') ) %>% 
  mutate(balance = factor(balance, c('None', 'CBPS'))) %>% 
  ggplot(aes(x = balance, y = 100*estimate)) + 
  geom_point(aes(col = balance), position = position_dodge(.1)) + 
  geom_errorbar(aes(ymin = 100*conf.low, ymax = 100*conf.high, col=balance), width = 0.01, position = position_dodge(.1)) +
  geom_text(aes(label = round(100*estimate, digits=2)), hjust = -0.5, vjust = 0, size = 6, position = position_dodge(.1), family = 'Times New Roman') +
  scale_color_manual(values=c('#31688e', '#932b80'), name='', labels = c('Unadjusted', 'Adjusted', NA)) +
  facet_grid(~model) +
  theme_classic() + 
  geom_hline(yintercept = 0.0, col = 'grey', linetype = 'dashed') +
  scale_y_continuous(limits = c(-0.001, 1.3)) + # labels = scales::percent,
  labs(x = 'Hot week', y = 'AME (%-point change)') +
  theme(text = element_text(size = 14, family = 'Times New Roman'), 
        axis.title.y = element_text(vjust=1, size =14), 
        axis.text = element_text(size=14), 
        legend.text = element_text(size=14), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
        legend.position = 'bottom')

# Model Balance 
##############################################################################@

plot_balance <- 
  cbps_base %>% 
    cobalt::love.plot(
      var.order = 'unadjusted', stars = 'std', thresholds = c(m = .1),
      drop.distance = TRUE, abs = TRUE, line = TRUE, 
      var.names = c(area_Urban = 'Area', 
                    age = 'Age', 
                    gender_Male = 'Gender', 
                    children = 'Children under 15', 
                    partner = 'Having a partner', 
                    log_precip_week = 'Precipitation', 
                    utci.avg.state = 'Average year-round heat', 
                    hotdays_year = 'Hot days in last 365 days',
                    year_2014 = '2014', year_2015 = '2015', 
                    year_2016 = '2016', year_2017 = '2017'),
      colors = c('#31688e', '#932b80'),
      shapes = c('triangle filled', 'circle filled')) + 
  ggtitle('') +
  labs(x='Absolute mean difference (*standardised)') +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        legend.position = 'bottom', legend.title = element_blank(), 
        legend.text = element_text(size=14), 
        text = element_text(size=14, family = 'Times New Roman'),
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size=14, vjust=0),
        axis.text.x = element_text(size=14, vjust=1) ) 

cbps_results <- plot_grid(plot_balance, plot_severity, 
                          align='hv', labels=c('A', 'B'), 
                          rel_widths = c(0.75,1))

ggsave(cbps_results, file='output/exhibit2_plotcbps.pdf', 
       device = cairo_pdf, width = 14, height = 6, dpi = 1000)

##############################################################################@
#### 3 | Exhibit 3 Regression table ---------------------------------------
##############################################################################@

# Regressions - Base 
##############################################################################@

# Base 
num_region <- as.numeric(sapply(ranef(model_base),nrow)[1])
num_country <- as.numeric(sapply(ranef(model_base),nrow)[2])
sd_region <- round(as.numeric(attributes(VarCorr(model_base)$'region.mix')$stddev), 4)
sd_country <- round(as.numeric(attributes(VarCorr(model_base)$'country.mix')$stddev), 4)

# noc
num_region_noc <- as.numeric(sapply(ranef(model_noc),nrow)[1])
num_country_noc <- as.numeric(sapply(ranef(model_noc),nrow)[2])
sd_region_noc <- round(as.numeric(attributes(VarCorr(model_noc)$'region.mix')$stddev), 4)
sd_country_noc <- round(as.numeric(attributes(VarCorr(model_noc)$'country.mix')$stddev), 4)

# cbps
num_region_cbps <- as.numeric(sapply(ranef(model_cbps),nrow)[1])
num_country_cbps <- as.numeric(sapply(ranef(model_cbps),nrow)[2])
sd_region_cbps <- round(as.numeric(attributes(VarCorr(model_cbps)$'region.mix')$stddev), 4)
sd_country_cbps <- round(as.numeric(attributes(VarCorr(model_cbps)$'country.mix')$stddev), 4)

# Simple main text table 
stargazer(
  model_noc, model_base, model_cbps, 
  df=T, single.row=F, report='vc*s', title='', digits = 5, model.numbers=T, 
  type='html', 
  out='output/exhibit3_models_main.htm', 
  dep.var.labels='Moderate-severe food insecurity',
  order=
    c('Constant',
      'hot_week_10P',
      'areaUrban', 'age', 'partner', 'genderMale', 'children',
      'log_precip_week', 'hotdays_year', 'utci.avg.state',
      'year2015', 'year2016', 'year2017'),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'Area: Urban', 'Age', 'Partner: Yes', 'Gender: Male',  'Children: Yes',
      'Precipitation (ln)',  'Hot days in the last 365 days', 'Year-round UTCI in subregion',
      '2015', '2016', '2017'), 
  add.lines = 
    list(c('Number of subregions', num_region_noc, num_region, num_region_cbps),
         c('Number of countries', num_country_noc, num_country, num_country_cbps),
         c('Standard deviation of subregions', sd_region_noc, sd_region, sd_region_cbps),
         c('Standard deviation of countries', sd_country_noc, sd_country, sd_country_cbps)))


# reporting full details in a supplementary table 
stargazer(
  model_noc, model_base, model_cbps, ci=T, report = ('vcsp'), 
  df=T, single.row=F, title='', digits = 4, model.numbers=T, 
  type='html', 
  out='output/exhibit3_models_main_supplementary.htm', 
  dep.var.labels='Moderate-severe food insecurity',
  omit = 
    c('log_precip_week', 'year'),
  order=
    c('Constant',
      'hot_week_10P',
      'areaUrban', 'age', 'partner', 'genderMale', 'children',
      #'log_precip_week', 
      'hotdays_year', 'utci.avg.state',
      'year2015', 'year2016', 'year2017'),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'Area: Urban', 'Age', 'Partner: Yes', 'Gender: Male',  'Children: Yes',
      #'Precipitation (ln)',  
      'Hot days in the last 365 days', 'Year-round UTCI in subregion',
      '2015', '2016', '2017'), 
  add.lines = 
    list(#c('Number of subregions', num_region_noc, num_region, num_region_cbps),
         c('Number of countries', num_country_noc, num_country, num_country_cbps),
        # c('Standard deviation of subregions', sd_region_noc, sd_region, sd_region_cbps),
         c('Standard deviation of countries', sd_country_noc, sd_country, sd_country_cbps)))


##############################################################################@
#### 4 | Exhibit 4 Heat days ---------------------------------------
##############################################################################@

# Number of days 
##############################################################################@

plot_days <- 
  model_base_days %>%
  tidy(effects = 'fixed', conf.int=T) %>% 
  filter(startsWith(term, 'as.factor')) %>% 
  mutate(
    term_final = as.numeric( stringi::stri_replace_all_fixed( term, 'as.factor(hotdays_week_10P)', '')) ) %>% 
  ggplot() + 
  geom_hline(yintercept=0.0, col='grey', linetype='dashed') +
  geom_point(aes(x=term_final, y=100*estimate), size=2) + 
  geom_errorbar(aes(x=term_final, ymin=100*conf.low, ymax=100*conf.high), width=0.01) + 
  geom_text(aes(x=term_final, y=100*estimate, label=100*(round(estimate, digits=4))),hjust=-0.25, vjust=0, size=5, family='Times New Roman') +
  labs(x='Number of hot days in last 7 days', y='AME (%-point change)') +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), limits = c(0.5, 7.5)) + 
  scale_y_continuous(limits = c(-1,3.0)) +
  theme_classic() +
  theme(text = element_text(size = 14, family='Times New Roman'), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14)) 

ggsave(plot_days, file = 'output/exhibit4_days.pdf', 
       device = cairo_pdf, width=8, height=4, dpi = 1000)


##############################################################################@
#### 5 | Exhibit 5 Mediation -------------------------------------------------------------
##############################################################################@

# Prepare data frame for plot
##############################################################################@

ame <-
  results_mediation %>% 
  filter(name == 'Average causal mediated effect' | 
         name =='Direct effect' | 
         name =='Total effect' )

proportion_subset <- 
  results_mediation %>% 
  filter(name == 'Proportion mediated' | name== 'Observations') %>% 
  pivot_wider(names_from='name', values_from='estimate') %>% 
  dplyr::select(`Proportion mediated`, Observations, model) %>% 
  rename('proportion' = `Proportion mediated`, 
         'observations' = Observations )

ame_subset <-  merge(ame, proportion_subset, by='model')

ame_subset <- 
  ame_subset %>% 
  mutate(
    proportion = as.numeric(proportion),
    proportion = round(proportion, digits=4)*100,
    proportion = replace(proportion, name == 'Direct effect', NA), 
    proportion = replace(proportion, name == 'Total effect', NA), 
    pvalue = replace(pvalue, pvalue == 0.00, '< 0.001'), 
    observations = replace(observations, name == 'Total effect',  NA), 
    observations = replace(observations, name == 'Direct effect',  NA),
    model =  factor(model, levels = c('Health', 'Difficulties getting by on income', 'Annual income', 'Job market', 'Employment'))) %>% 
  arrange(model)

## Forest plot 
##############################################################################@

library(metafor)

png(file = 'output/exhibit5_mediation.png', 
    width=12, height=8, units = 'in', family='Times New Roman', res=600)

par(bg = 'white', family='Times New Roman', font=1, cex = 1.1)

# Start forest plot
forest(
  
  # Plot 
  x=ame_subset$estimate*100, 
  ci.lb=ame_subset$lower*100, 
  ci.ub=ame_subset$upper*100, 
  slab=ame_subset$name, 
  xlab = 'Effect size (%-points)', 
  psize=0.5, 
  boxsize=0.5, 
  annotate=TRUE, 
  showweight=FALSE,
  refline=0, # xintercept = 0 for plot
  
  # Bounds
  xlim =c(-5,2), 
  alim=c(-0.3,1), 
  ylim=c(27, 1),
  
  # Extra columns 
  ilab = cbind(ame_subset$pvalue, ame_subset$proportion, ame_subset$observations), 
  ilab.xpos=c(-2.5,-1.5, -0.75), 
  
  digits=c(2,1),
  
  rows = c(23:21, 18:16, 13:11, 8:6, 3:1) # three sets of rows with three lines each 
 ) 

# Add text above each row block 
op <- par(cex = 1.1, font = 3)
text(-5, c(24, 19, 14, 9, 4), pos = 4, 
     c('Health problems', 'Difficulties getting by on present income', 
       'Annual income', 'Local job market', 'Employment'))

# Add text labels 
par(cex=1.1, font = 2)
text(-2.5, 26, 'p-value')
text(c(-1.5), 26.5, c('Proportion \n\ mediated (%)'))
text(c(-0.75), 26, c('n'))
text(-5, 26, 'Mediator', pos = 4)
text(2, 26, '%-point change', pos = 2)

dev.off()

##############################################################################@
#### 6 | Exhibit 6 Moderators ---------------------------------------------------
##############################################################################@

# The margins have been calculated and stored in time-savers and read in during the set up

# Agricultural employment
##############################################################################@

plot_margins_ae <- 
  merge(data %>% 
          dplyr::select(ag.employ) %>% 
          mutate(ag.employ = round(ag.employ, digits=4)) %>% 
          group_by(ag.employ) %>% 
          mutate(n=n()), 
        margins_ae, all=T) %>% 
  ggplot(aes(x=ag.employ)) +
  geom_area(stat ='bin', alpha=0.20, binwidth=3, fill='#35B779FF') +
  geom_point(aes(y=AME*2000000), stat = 'unique') +
  geom_errorbar(aes(ymin=lower*2000000, ymax=upper*2000000), size=0.3, width=0.3, stat = 'unique') +
  geom_text(aes(y=AME*2000000, label=(round(AME*100, digits=2))),
            hjust=-0.25, vjust=0, size=5, family='Times New Roman', stat = 'unique') +
  scale_x_continuous(name='% of total employment', breaks=c(10, 30, 50, 70, 90)) + 
  scale_y_continuous(sec.axis = sec_axis(~./20000, name='AME (%-point change)'), 
                     breaks=c(0,25000,50000,75000,100000), 
                     labels = c('0','25', '50', '75', '100') ) +
  labs(y='Observations (in 1,000)') +
  theme_bw() +
  theme(panel.grid=element_blank(), 
        axis.ticks.y.right = element_blank(), 
        axis.text.y.right = element_blank(), 
        axis.title.x = element_text(vjust = -0.5), 
        plot.title = element_text(size = 14, hjust = 0.5, face='bold'), 
        text = element_text(size = 14, family='Times New Roman'),
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        panel.border = element_blank())


# Visualisation map 
heatmap.ag.employ <- 
  merge(countries, 
        data %>% group_by(ISO_3) %>% reframe(mean_ag.employ = mean(ag.employ, na.rm=T)), 
        by='ISO_3', all.x=T) %>% 
  ggplot() + 
  geom_sf(aes(fill = mean_ag.employ<=22), size=0.01, alpha=1) +
  scale_fill_manual(values=c('#932b80', '#31688e'), 
                    name='', labels = c('Significant increase', 'Other', 'NA')) +
  theme_classic() +  
  theme(legend.position='bottom', legend.box = 'horizontal', legend.key.size = unit(0.25, 'cm'),
        axis.line = element_blank(), axis.title =element_blank(), axis.text =element_blank(), axis.ticks =element_blank(),
        plot.title = element_text(size = 14, hjust=0.5, face='bold'), 
        legend.title = element_text(size=14), legend.text = element_text(size=14), 
        text = element_text(size = 14, family='Times New Roman')) +
  labs(x='',y='') 


# GNI
##############################################################################@

# Plot margins 
plot_margins_gni <-  
  merge(data %>% 
          dplyr::select(log_gni) %>% 
          mutate(log_gni = round(log_gni, digits=4)) %>% 
          group_by(log_gni) %>% 
          mutate(n=n()), 
        margins_gni, all=T) %>% 
  ggplot(aes(x=log_gni)) +
  geom_area(stat ='bin', alpha=0.20, binwidth=0.5, fill='#35B779FF') +
  geom_point(aes(y=AME*2000000), stat = 'unique') +
  geom_errorbar(aes(ymin=lower*2000000, ymax=upper*2000000), size=0.3, width=0.00, stat = 'unique') +
  geom_text(aes(x=log_gni, y=AME*2000000, label=(round(AME*100, digits=2))),hjust=-0.25, vjust=0, size=5, family='Times New Roman', stat = 'unique') +
  scale_x_continuous(name='GNI per capita (ln)', breaks=c(6,7,8,9,10,11,12)) + 
  scale_y_continuous(sec.axis = sec_axis(~./20000, name='AME (%-point change)'), breaks=c(0, 25000, 50000, 75000), labels=c(0, 25, 50, 75)) +
  labs(y='Observations (in 1,000)') +
  theme_bw() +
  theme(panel.grid=element_blank(), panel.border = element_blank(), 
        axis.ticks.y.right = element_blank(), 
        axis.text.y.right = element_blank(), 
        axis.title.x = element_text(vjust = -0.5), 
        plot.title = element_text(size = 14, hjust = 0.5, face='bold'), 
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=14), 
        text = element_text(size = 14, family='Times New Roman'))

# visualisation map 
heatmap.gni <-  
  merge(countries, 
        data %>% group_by(ISO_3) %>% reframe(mean_log_gni = mean(log_gni, na.rm=T)), 
        by='ISO_3', all.x=T) %>% 
  ggplot() + 
  geom_sf(aes(fill = mean_log_gni>=9.01), size=0.01, alpha=1) +
  scale_fill_manual(values=c('#932b80', '#31688e'), 
                    name='', labels = c('Significant increase', 'Other', 'NA')) +
  theme_classic() +  
  theme(legend.position='bottom', legend.box = 'horizontal', legend.key.size = unit(0.25, 'cm'),
        text = element_text(size = 14, family='Times New Roman'), 
        axis.line = element_blank(), axis.title =element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5, face='bold'), 
        legend.text = element_text(size=14), 
        plot.caption = element_text(size = 14, hjust=0)) +
  labs(x='',y='') 


# Vulnerable employment 
##############################################################################@

plot_margins_ve <-  
  merge(data %>% 
          dplyr::select(vulnerable.employment) %>% 
          mutate(vulnerable.employment = round(vulnerable.employment, digits=0)) %>% 
          group_by(vulnerable.employment) %>% 
          mutate(n=n()), 
        margins_ve, all=T) %>% 
  ggplot(aes(x=vulnerable.employment)) +
  geom_area(stat ='bin', alpha=0.20, binwidth=3, fill='#35B779FF') +
  geom_point(aes(y=AME*1250000), stat = 'unique') +
  geom_errorbar(aes(ymin=lower*1250000, ymax=upper*1250000), size=0.3, width=0.01, stat = 'unique') +
  geom_text(aes(x=vulnerable.employment, y=AME*1250000, label=(round(AME*100, digits=2))),hjust=-0.25, vjust=0, size=5, family='Times New Roman', stat = 'unique') +
  scale_x_continuous(name='% of total employment', limits=c(0, 100), breaks=c(10,30,50,70)) + 
  scale_y_continuous(sec.axis = sec_axis(~./12500, name='AME (%-point change)'),  breaks = c(0, 20000, 40000), labels = c(0, 20, 40) ) +
  labs(y='Observations (in 1,000)') +
  theme_bw() +
  theme(panel.grid=element_blank(), 
        panel.border = element_blank(), 
        axis.ticks.y.right = element_blank(), 
        axis.text.y.right = element_blank(), 
        axis.title.x = element_text(vjust = -0.5), 
        plot.title = element_text(size = 14, hjust = 0.5, face='bold'), 
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=14), 
        text = element_text(size = 14, family='Times New Roman'))

# visualisation map 
heatmap.ve <-   
  merge(countries, 
        data %>% group_by(ISO_3) %>% reframe(mean_ve = mean(vulnerable.employment, na.rm=T)), 
        by='ISO_3', all.x=T) %>% 
  ggplot() + 
  geom_sf(aes(fill = mean_ve<32), size=0.01, alpha=1) +
  scale_fill_manual(values=c('#932b80', '#31688e'), 
                    name='', labels = c('Significant increase', 'Other', 'NA')) +
  theme_classic() +  
  theme(legend.position='bottom', legend.box = 'horizontal', legend.key.size = unit(0.25, 'cm'),
        text = element_text(size = 14, family='Times New Roman'), 
        axis.line = element_blank(),
        legend.text = element_text(size=14),
        axis.title =element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5, face='bold'), 
        plot.caption = element_text(size = 14, hjust=0)) +
  labs(x='',y='') 


plot_grid(plot_margins_ve, heatmap.ve, rel_widths = c(0.7, 1), scale=0.9)

# Compile into exhibit
##############################################################################@

exhibit3_plots <- align_plots(plot_margins_gni, heatmap.gni, 
                              plot_margins_ae, heatmap.ag.employ,
                              plot_margins_ve, heatmap.ve, 
                              align = 'hv', axis = 'tb')

exhibit3 <- plot_grid(exhibit3_plots[[1]], heatmap.gni,
                      exhibit3_plots[[3]], heatmap.ag.employ, 
                      exhibit3_plots[[5]], heatmap.ve, 
                      nrow=3, ncol=2, 
                      labels = c('A-1', 'A-2', 'B-1', 'B-2', 'C-1', 'C-2'), 
                      align = 'hv', axis='tb',
                      rel_widths = c(0.7, 1), scale=0.9)

ggsave(exhibit3, file = 'output/exhibit6_moderation.pdf', 
       width = 14, height = 12, dpi=1000, 
       device = cairo_pdf)

##############################################################################@
# Appendix -------------------------------------------
##############################################################################@

##############################################################################@
# 1 | Descriptive ----------------------------------------------------
##############################################################################@

##############################################################################@
#### 1 | List of variables ----------------------------------------------------
##############################################################################@

# edited directly in overleaf 

##############################################################################@
#### 2 | Descriptive statistics -----------------------------------------------
##############################################################################@

descriptive_df <- 
  data %>% 
  dplyr::select(
    hot_week_10P, severe_fi, fi_binary, mild_fi, 
    area, children, age, partner, gender,
    log_precip_week, utci.avg.state,
    year, log_gni, vulnerable.employment, ag.employ, formality) %>% 
  
  mutate(
    hot_week_10P = replace(hot_week_10P, hot_week_10P == 0, 'No'),
    hot_week_10P = replace(hot_week_10P, hot_week_10P == 1, 'Yes'), 
    
    severe_fi = replace(severe_fi, severe_fi == 0, 'No'),
    severe_fi = replace(severe_fi, severe_fi == 1, 'Yes'), 
    
    mild_fi = replace(mild_fi, mild_fi == 0, 'No'),
    mild_fi = replace(mild_fi, mild_fi == 1, 'Yes'), 
    
    fi_binary = replace(fi_binary, fi_binary == 0, 'No'),
    fi_binary = replace(fi_binary, fi_binary == 1, 'Yes'),
    
    children = as.numeric(children), 
    children = replace(children, children == '1', 'No'),
    children = replace(children, children == '2', 'Yes'), 
    
    partner = as.numeric(partner), 
    partner = replace(partner, partner == 1, 'No'),
    partner = replace(partner, partner == 2, 'Yes')) %>% 
  
  mutate(hot_week_10P = as.factor(hot_week_10P), 
         severe_fi = as.factor(severe_fi), 
         mild_fi = as.factor(mild_fi), 
         children = as.factor(children), 
         partner = as.factor(partner), 
         fi_binary = as.factor(fi_binary), 
         year = as.factor(year)
  ) %>% 
  
  rename(`Hot week` = `hot_week_10P`,
         `Moderate-to-severe food insecurity` = `fi_binary`,
         `Severe food insecurity` = `severe_fi`, 
         `Mild-to-severe food security` = `mild_fi`, 
         `Area` = `area`, 
         `Age` = `age`, 
         `Gender: Male` = `gender`, 
         `Children under 15 in household` = `children`, 
         `Having a partner` = `partner`,
         `Precipitation in week (ln)` = `log_precip_week`, 
         `Average year-round UTCI` = `utci.avg.state`, 
         `GNI per capita (ln)` = `log_gni`,
         `Vulnerable employment` = vulnerable.employment, 
         `Agricultural employment` = `ag.employ`, 
         `Formality` = `formality`, 
         `Year` = year)

library(reporttools)
descriptive_df %>% 
  dplyr::select_if(is.numeric) %>% 
  as.data.frame() %>% 
  tableContinuous(
    stats = c('n', 'min', 'q1', 'median', 'mean', 'q3', 'max', 's', 'na'), 
    co.tit = c('N', 'Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max', 'SD', 'NA'), 
    cap = 'Descriptive Statistics: Continuous variables', 
    caption.placement = getOption("xtable.caption.placement", "top"), 
    file = 'output/1_2_num_descriptive.tex', 
    longtable = F)
  
descriptive_df %>% 
  dplyr::select_if(is.factor) %>% 
  as.data.frame() %>% 
  tableNominal(cumsum=F, longtable=F, 
               cap = 'Descriptive statistics for categorical variables', 
               caption.placement = getOption("xtable.caption.placement", "top"), 
               file='output/1_2_cat_descriptive.tex')


##############################################################################@
#### 3 | Distribution of key variables -------------------------------------------------
##############################################################################@

# Histograms for hot days and fi index 
##############################################################################@

hist_hot <- 
  data %>% 
  group_by(hotdays_week_10P) %>% 
  reframe(n = n()) %>% 
  ggplot() + 
  geom_col(aes(x=hotdays_week_10P, y=n/1000), width=0.5) + 
  labs(x='Days in the last week', y='Count in (1,000)', 
       title = 'Days in last week in the hottest 10% of year') +
  theme_classic() + 
  theme(text = element_text(size = 18, family='Times New Roman'), 
        axis.text = element_text(size = 18, family='Times New Roman'),
        plot.title = element_text(face='bold'))

hist_fi <- 
  data %>% 
  group_by(fi_index) %>% 
  reframe(n = n()) %>% 
  ggplot() + 
  geom_col(aes(x=fi_index, y=n/1000), width=0.5) + 
  labs(x='FIES scale', y='Count in (1,000)', title = 'Food insecurity index') +
  theme_classic() + 
  theme(text = element_text(size = 18, family='Times New Roman'),
        axis.text = element_text(size = 18, family='Times New Roman'),
        plot.title = element_text(face='bold'))

histograms <- ggarrange(hist_hot, hist_fi, labels=c('A', 'B'))

ggsave(histograms, file='output/1_3_distribution_1.pdf',  
       width=14, height=5, device = cairo_pdf)

# Share of fi and hot weeks in each country 
##############################################################################@

fi_country <-  
  data %>% 
  group_by(ISO_3) %>% 
  summarise(fi_share = 100*mean(fi_binary, na.rm=T)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(ISO_3, fi_share), y=fi_share)) + 
  geom_hline(yintercept=10, col='red') + 
  geom_hline(yintercept=90, col='red') +
  labs(x='Country', y='Share of observations', title='Moderate-severe food insecurity') +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, family='Times New Roman'),
        plot.title = element_text(size = 10, face='bold'), 
        text = element_text(size = 10, family = 'Times New Roman') )
        
heat_country <-  
  data %>% 
  group_by(ISO_3) %>% 
  summarise(hot_week_share = 100*mean(hot_week_10P, na.rm = T)) %>% 
  ggplot() +
  geom_col(aes(x = reorder(ISO_3, hot_week_share), y = hot_week_share)) + 
  geom_hline(yintercept = 10, col = 'red') + 
  geom_hline(yintercept = 90, col = 'red') +
  labs(x = 'Country', y = 'Share of observations', title='Hot weeks') +
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 10, family='Times New Roman'),
        plot.title = element_text(size = 10, face='bold'), 
        text = element_text(size = 10, family = 'Times New Roman') )

distribution_country <-  
  ggarrange(fi_country, heat_country, labels=c('A', 'B'), nrow=2)

ggsave(distribution_country, file = 'output/1_3_distribution_2.pdf', 
       width = 8, height = 5, device = cairo_pdf)
      

##############################################################################@
#### 4 | Standard deviation year / area / day------------------------------------
##############################################################################@

# Standard deviation throughout the year 
plot_sd_year <- 
  data %>% 
  ggplot() + 
  geom_histogram(aes(x = sd_utci_year, y=..count../1000)) + 
  theme_classic() + 
  theme(text = element_text(size = 18, family = 'Times New Roman'), 
        plot.title = element_text(face = 'bold')) + 
  labs(x='Standard deviation of UTCI throughout the year in an area', 
       y = 'Count (in 1,000)', 
       title = 'Throughout the year') 

# SD within a day in a given area 
plot_sd_day <- 
  data %>% 
  ggplot() + 
  geom_histogram(aes(x = sd_utci_exact, y=..count../1000)) + 
  theme_classic() + 
  theme(text = element_text(size = 18, family='Times New Roman'), 
        plot.title = element_text(face = 'bold')) + 
  labs(x ='Standard deviation of UTCI throughout the day in an area (2013)', 
       y='Count (in 1,000)', 
       title = 'Throughout the day')

# SD within an area on a day in a given area 
plot_sd_area <- 
  data %>% 
  ggplot() + 
  geom_histogram(aes(x = sd_area_mean, y=..count../1000)) + 
  theme_classic() + 
  theme(text = element_text(size = 18, family = 'Times New Roman'), 
        plot.title = element_text(face = 'bold')) + 
  labs(x='Standard deviation of UTCI within an area on a day', 
       y='Count (in 1,000)', 
       title = 'Within an area')

plots_sd <- ggarrange(plot_sd_year, plot_sd_area, plot_sd_day, 
                      labels = c('A', 'B', 'C'), nrow=3)

ggsave(plots_sd, file = 'output/1_4_plots_sd.pdf', 
       width = 12, height = 10, device = cairo_pdf)


##############################################################################@
#### 5 | Country profiles interpretation  ----------------------------------------------
##############################################################################@

library(vtable)

data %>% 
  dplyr::select(country.mix, hot_week_10P, threshold_10P, mean_utci_mix, ag.employ, 
                vulnerable.employment, formality, wage.labour, log_gni) %>% 
  rename(Country = country.mix) %>% 
  st(group='Country', group.long=T, 
     labels=c('Hot week', 'Threshold at 90th percentile','Daily UTCI', 
              'Agricultural employment', 'Vulnerable employment', 'Formality',
              'Wage labour', 'GNI per capita (ln)'), 
     summ = c('notNA(x)','mean(x)', 'median(x)', 'sd(x)',
              'min(x)','pctile(x)[25]','pctile(x)[75]','max(x)'),
     title = 'Country profiles', 
     out = 'latex', #htmlreturn
     file = 'output/1_5_country_profile.tex') 

##############################################################################@
# 2 | Results ----------------------------------------------------
##############################################################################@


##############################################################################@
#### 1 | Mediation results  ----------------------------------------------------
##############################################################################@

# Fit models with three levels  
##############################################################################@

m3_emp <- lme4::lmer(
  employment_7days ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix/region.mix), data = data %>% drop_na(fi_binary, employment_7days, hot_week_10P)) 

m3_job <- lme4::lmer(
  local_job_market ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix/region.mix), data = data %>% drop_na(fi_binary, local_job_market, hot_week_10P) ) 

m3_income <- lme4::lmer(
  log_income_2 ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix/region.mix), data = data %>% drop_na(fi_binary, log_income_2, hot_week_10P) ) 

m3_health <- lme4::lmer(
  health_problems ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix/region.mix), data = data %>% drop_na(fi_binary, health_problems, hot_week_10P) ) 

m3_HH <- lme4::lmer(
  feeling_HH_income ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + year + utci.avg.state + hotdays_year + 
    (1 | country.mix/region.mix), data = data %>% drop_na(fi_binary, feeling_HH_income, hot_week_10P) ) 

# Export table 
stargazer(
  m3_emp, m3_job, m3_income, m3_health, m3_HH, 
  type='latex', df=T, single.row=T, report='vc*s', model.numbers=T, 
  title='Models testing an association between a hot week and different mediators',
  out='output/2_1_models_mediators.tex', 
  dep.var.labels=c('Unemployment', 'Job', 'Income', 'Health', 'Difficulties'),
  order=
    c('Constant',
      'hot_week_10P',
      'areaUrban', 'age', 'partner', 'genderMale', 'children',
      'log_precip_week', 'hotdays_year', 'utci.avg.state',
      'year2015', 'year2016', 'year2017'),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'Area: Urban', 'Age', 'Gender: Male',  'Partner: Yes', 'Children: Yes',
      'Precipitation (ln)', 'Hot days in last 365 days', 'Year-round UTCI',
      '2015', '2016', '2017') )

# Export table results of formal analysis
##############################################################################@

results_mediation %>% 
  mutate(
    model = model, 
    name = name, 
    estimate = as.character(round(estimate, digits=4)),
    lowerCI = as.character(round(lower, digits=4)),
    upperCI = as.character(round(upper, digits=4)) ) %>% 
  relocate(model, name) %>% 
  mutate(
    pvalue = replace(pvalue, pvalue == 0, '<0.01') ) %>% 
  filter(model == 'Annual income') %>% 
  rename('variable' = name) %>% 
  dplyr::select(-model, -lower, -upper) %>% 
  stargazer(summary = F, title = 'Annual income', digits=4, 
          type = 'latex', out = 'output/2_1_mediation_AnnualIncome.tex')

results_mediation %>% 
  mutate(
    model = model, 
    name = name, 
    estimate = as.character(round(estimate, digits=4)),
    lowerCI = as.character(round(lower, digits=4)),
    upperCI = as.character(round(upper, digits=4)) ) %>% 
  relocate(model, name) %>% 
  mutate(
    pvalue = replace(pvalue, pvalue == 0, '<0.01') ) %>% 
  filter(model == 'Health') %>% 
  rename('variable' = name) %>% 
  dplyr::select(-model, -lower, -upper) %>% 
  stargazer(summary = F,  digits=4, 
            title = 'Health problems',
            type = 'latex', out = 'output/2_1_mediation_Health.tex')

results_mediation %>% 
  mutate(
    model = model, 
    name = name, 
    estimate = as.character(round(estimate, digits=4) ),
    lowerCI = as.character(round(lower, digits=4)),
    upperCI = as.character(round(upper, digits=4)) ) %>% 
  relocate(model, name) %>% 
  mutate(
    pvalue = replace(pvalue, pvalue == 0, '<0.01') ) %>% 
  filter(model == 'Job market') %>% 
  rename('variable' = name) %>% 
  dplyr::select(-model, -lower, -upper) %>% 
  stargazer(summary = F,  digits=4, 
            title = 'Local job market', 
            type = 'latex', out = 'output/2_1_mediation_JobMarket.tex')

results_mediation %>% 
  mutate(
    model = model, 
    name = name, 
    estimate = as.character(round(estimate, digits=4)),
    lowerCI = as.character(round(lower, digits=4)),
    upperCI = as.character(round(upper, digits=4)) ) %>% 
  relocate(model, name) %>% 
  mutate(
    pvalue = replace(pvalue, pvalue == 0, '<0.01') ) %>% 
  filter(model == 'Employment') %>% 
  rename('variable' = name) %>% 
  dplyr::select(-model, -lower, -upper) %>% 
  stargazer(summary = F,  digits=4, 
            title = 'Employment', 
            type = 'latex', out = 'output/2_1_mediation_Employment.tex')

results_mediation %>% 
  mutate(
    model = model, 
    name = name, 
    estimate = as.character(round(estimate, digits=4)),
    lowerCI = as.character(round(lower, digits=4)),
    upperCI = as.character(round(upper, digits=4)) ) %>% 
  relocate(model, name) %>% 
  mutate(
    pvalue = replace(pvalue, pvalue == 0, '<0.01') ) %>% 
  filter(model == 'Difficulties getting by on income') %>% 
  rename('variable' = name) %>% 
  dplyr::select(-model, -lower, -upper) %>% 
  stargazer(summary = F,  digits=4, 
            title = 'Difficulties getting by on income', 
            type = 'latex', out = 'output/2_1_mediation_DifficultiesIncome.tex')


##############################################################################@
#### 2 | Moderation results  -------------------------------
##############################################################################@

# Regressions - Moderator 
##############################################################################@

# Agricultural employment
num_region.gni <- as.numeric(sapply(ranef(model_gni),nrow)[1])
num_country.gni <- as.numeric(sapply(ranef(model_gni),nrow)[2])
sd_region.gni <- round(as.numeric(attributes(VarCorr(model_gni)$'region.mix')$stddev), 4)
sd_country.gni <- round(as.numeric(attributes(VarCorr(model_gni)$'country.mix')$stddev), 4)

# Agricultural employment
num_region.ag.employ <- as.numeric(sapply(ranef(model_ae),nrow)[1])
num_country.ag.employ <- as.numeric(sapply(ranef(model_ae),nrow)[2])
sd_region.ag.employ <- round(as.numeric(attributes(VarCorr(model_ae)$'region.mix')$stddev), 4)
sd_country.ag.employ <- round(as.numeric(attributes(VarCorr(model_ae)$'country.mix')$stddev), 4)

# Vulnerable employment 
num_region.ve <- as.numeric(sapply(ranef(model_ve),nrow)[1])
num_country.ve <- as.numeric(sapply(ranef(model_ve),nrow)[2])
sd_region.ve <- round(as.numeric(attributes(VarCorr(model_ve)$'region.mix')$stddev), 4)
sd_country.ve <- round(as.numeric(attributes(VarCorr(model_ve)$'country.mix')$stddev), 4)

num_region.formality <- as.numeric(sapply(ranef(model_formality),nrow)[1])
num_country.formality <- as.numeric(sapply(ranef(model_formality),nrow)[2])
sd_region.formality <- round(as.numeric(attributes(VarCorr(model_formality)$'region.mix')$stddev), 4)
sd_country.formality <- round(as.numeric(attributes(VarCorr(model_formality)$'country.mix')$stddev), 4)

stargazer(
  model_gni, model_ae, model_ve, 
  type='latex', df=T, single.row=T, report='vc*s', 
  title='Moderation analysis',
  out='output/2_2_models_moderators.tex', 
  model.numbers=T, dep.var.labels='Moderate-severe food insecurity',
  order =
    c('Constant',
      'hot_week_10P',
      'log_gni', 
      'ag.employ',
      'vulnerable.employment', 
      'formality',
      'areaUrban', 'age', 'partner', 'genderMale', 'children',
      'log_precip_week', 'hotdays_year','utci.avg.state',
      'year2015', 'year2016', 'year2017'),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'Hot week x GNI',
      'Hot week x Agricultural emp.',
      'Hot week x Vulnerable em.',
      'GNI',
      'Agricultural employment',
      'Vulnerable employment', 
      'Area: Urban', 'Age', 'Gender: Male', 'Partner: Yes', 'Children: Yes',
      'Precipitation (ln)', 'Hot days in the last 365 days', 'Year-round UTCI',
      '2015', '2016', '2017'), 
  add.lines = list(c('Number of subregions', num_region.gni, num_region.ag.employ, num_region.ve),
                   c('Number of countries', num_country.gni, num_country.ag.employ, num_country.ve),
                   c('Standard deviation of subregions', sd_region.gni, sd_region.ag.employ, sd_region.ve),
                   c('Standard deviation of countries', sd_country.gni, sd_country.ag.employ, sd_country.ve)))

##############################################################################@
# 3 | Experiment ----------------------------------------------------
##############################################################################@

##############################################################################@
#### 1 | CBPS ----------------------------------------------
##############################################################################@

# Create balance 
## Use time saver, this runs for ~1h on Mac M1, 8GB
cbps_base <- CBPS(hot_week_10P ~ 
                  area + children + age + gender + partner + 
                  utci.avg.state + log_precip_week + hotdays_year + year,
                  data = CBPS_data)

# Save for future use 
save(cbps_base, file='time-savers/cbps_base.rda')

# Extract more details 
num_region.cbps.mild <- as.numeric(sapply(ranef(model_mild_cbps),nrow)[1])
num_country.cbps.mild <- as.numeric(sapply(ranef(model_mild_cbps),nrow)[2])
sd_region.cbps.mild <- round(as.numeric(attributes(VarCorr(model_mild_cbps)$'region.mix')$stddev), 4)
sd_country.cbps.mild <- round(as.numeric(attributes(VarCorr(model_mild_cbps)$'country.mix')$stddev), 4)

num_region.cbps.moderate <- as.numeric(sapply(ranef(model_moderate_cbps),nrow)[1])
num_country.cbps.moderate <- as.numeric(sapply(ranef(model_moderate_cbps),nrow)[2])
sd_region.cbps.moderate <- round(as.numeric(attributes(VarCorr(model_moderate_cbps)$'region.mix')$stddev), 4)
sd_country.cbps.moderate <- round(as.numeric(attributes(VarCorr(model_moderate_cbps)$'country.mix')$stddev), 4)

num_region.cbps.severe <- as.numeric(sapply(ranef(model_severe_cbps),nrow)[1])
num_country.cbps.severe <- as.numeric(sapply(ranef(model_severe_cbps),nrow)[2])
sd_region.cbps.severe <- round(as.numeric(attributes(VarCorr(model_severe_cbps)$'region.mix')$stddev), 4)
sd_country.cbps.severe <- round(as.numeric(attributes(VarCorr(model_severe_cbps)$'country.mix')$stddev), 4)

stargazer(
  model_mild_cbps, model_moderate_cbps, model_severe_cbps, 
  type = 'latex', df = T, single.row = T, report = 'vc*s', model.numbers = T, 
  title = 'Regression models using covariate balance propensity scores',
  out = 'output/3_1_models_cbps.tex', 
  dep.var.labels = c('Mild to severe', 'Moderate-severe', 'Severe'),
  order =
    c('Constant',
      'hot_week_10P',
      'areaUrban','age','partner','genderMale','children',
      'log_precip_week', 'utci.avg.state', 'hotdays_year',
      'year2015', 'year2016', 'year2017'),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'Area: Urban', 'Age', 'Gender: Male', 'Partner: Yes', 'Children: Yes',
      'Precipitation (ln)', 'Year-round UTCI', 'Hot days in past year', 
      '2015', '2016', '2017'), 
  add.lines = list(c('Number of subregions', num_region.cbps.mild, num_region.cbps.moderate, num_region.cbps.severe),
                   c('Number of countries', num_country.cbps.mild, num_country.cbps.moderate, num_country.cbps.severe),
                   c('Standard deviation of subregions', sd_region.cbps.mild, sd_region.cbps.moderate, sd_region.cbps.severe),
                   c('Standard deviation of countries', sd_country.cbps.mild, sd_country.cbps.moderate, sd_country.cbps.severe)))


##############################################################################@
#### 2 | Selection bias  -------------------------------
##############################################################################@

# number of interviews per day
model_total.interviews <- lme4::lmer(
  total.interviews ~ hot_week_10P +
          area + children + age + gender + partner + 
          log_precip_week + utci.avg.state + hotdays_year + year + 
          (1 | country.mix/region.mix), data=data)
summary(model_total.interviews)

# check share of poor interview
model_share.li <- lme4::lmer(
  share.li.interviews ~ hot_week_10P +
          area + children + age + gender + partner + 
          log_precip_week + utci.avg.state + hotdays_year + year +
          (1 | country.mix/region.mix), data=data)
summary(model_share.li)
confint(model_share.li,  parm='hot_week_10P', level=0.95)
df.residual(model_share.li)

# include share of poor interview as a covariate
model_selection <- lme4::lmer(
  fi_binary ~ hot_week_10P +
    share.li.interviews + 
    area + children + age + gender + partner + 
    log_precip_week + utci.avg.state + hotdays_year + year +
    (1 | country.mix/region.mix), data=data)
summary(model_selection)
confint(model_selection,  parm='hot_week_10P', level=0.95)
df.residual(model_selection)

stargazer(
  model_total.interviews, model_share.li, model_selection, 
  type = 'latex',
  out = 'output/3_2_models_selection_bias.tex', 
  single.row = T, report = 'vc*s', title = 'Models exploring the selection bias', model.numbers = T, 
  dep.var.labels = c('Number of interviews', 'Share of low income interviews', 'Moderate-severe food insecurity'),
  order =
    c('Constant',
      'hot_week_10P',
      'share.li.interviews', 
      'areaUrban','age','partner','genderMale', 'children',
      'log_precip_week', 'hotdays_year', 'utci.avg.state', 
      'year2015', 'year2016', 'year2017'),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'Low income interviews share',
      'Area: Urban', 'Age','Gender: Male', 'Partner: Yes', 'Children: Yes',
      'Precipitation (ln)', 'Hot days in last 365 days','Year-round UTCI', 
      '2015', '2016', '2017') )


##############################################################################@
#### 3 | Geographic match  ----------------------------------------------------
##############################################################################@

states <- 
  read_sf(dsn='data/map', layer='gadm36_1') %>% 
  mutate(ISO_3 = countrycode(NAME_0, origin='country.name', destination='iso3c'), 
         ISO_3 = ifelse(NAME_0=='Kosovo', 'XKO', ISO_3),
         ISO_3 = ifelse(NAME_0=='Northern Cyprus', 'CYP2', ISO_3)) %>% 
  filter(NAME_0 != 'Antarctica')

# combination of latlon points
lonlat.UK <- 
  brick('data/utci/example.nc') %>% 
  as.data.frame(xy=TRUE) %>% 
  rename(lon = x, lat = y) %>% 
  dplyr::select(lon, lat) %>% 
  filter(lon > -14 & lon <2 &
           lat > 49 & lat < 62) %>% 
  st_as_sf(dim='XY', coords=c('lon', 'lat'), crs='+proj=longlat +datum=WGS84 +no_defs') %>% 
  st_join(states %>% filter(NAME_0 == 'United Kingdom'), left=F)

# boxplot of area sizes (ln)
plot_area <- 
  data %>%
  mutate(area_sqkm = log(area_sqkm)) %>%
  ggplot() + 
  geom_boxplot(aes(x=area_sqkm)) + 
  labs(x='Area in square kilomaters (ln)') +
  theme_classic() + 
  theme(text = element_text(size = 14, family = 'Times New Roman'), 
        axis.text.y = element_text(size=14), 
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank()) +
  coord_flip()

Summary<-boxplot(log(data$area_sqkm))$stats
rownames(Summary)<-c("Min","First Quartile","Median","Third Quartile","Maximum")
Summary

Q3 = Summary['Third Quartile',] %>% as.data.frame() %>% pull()
Q1 = Summary['First Quartile',] %>% as.data.frame() %>% pull()
IQR = Q3-Q1

upper_whisker = min(max(log(data$area_sqkm), Q3 + 1.5 * IQR) ) 
lower_whisker = max(min(log(data$area_sqkm), Q3 + 1.5 * IQR) ) 

# Plot of longitude and latitude ERA-5 observations over United Kingdom as an example
plot_geography1 <- 
  ggplot() +
  geom_sf(data = states %>% filter(NAME_0 == 'United Kingdom'), fill='white', col='black') +
  geom_sf(data = lonlat.UK, size = 0.0005, col = 'darkblue') +
  theme_classic() + 
  theme(text = element_text(size = 14, family = 'Times New Roman'))

plot_geography <- plot_grid(plot_geography1, plot_area,
                            labels = c('A', 'B'), align = 'hv', axis = 't',
                            ncol=2)

ggsave(plot_geography, file = 'output/3_3_plot_geography.pdf', 
       device = cairo_pdf, width = 6, height=3)

# Only smaller areas # use the median of 12434 sqkm: this is a little more than half the size of Wales
model_small_area <- lmerTest::lmer(
  fi_binary ~ hot_week_10P +
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data = data %>% filter(area_sqkm < 12434)) 
summary(model_small_area)
data %>% group_by(continent) %>% reframe(mean_size = mean(area_sqkm, na.rm=T))

# Only areas with low UTCI variation 
model_small_sd <- lmerTest::lmer(
  fi_binary ~ hot_week_10P +
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data = data %>% filter(sd_area_mean < 2)) 
summary(model_small_sd)
df.residual(model_small_sd)
confint(model_small_sd, parm = 'hot_week_10P', level = 0.95)

# Controlling for size of area and variation of UTCI observations within areas
model_small_control <- lmerTest::lmer(
  fi_binary ~ hot_week_10P +sd_area_mean + log(area_sqkm) + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data = data) 
summary(model_small_control)
df.residual(model_small_control)
confint(model_small_control, parm = 'hot_week_10P', level = 0.95)

# Export regression tables  
stargazer(
  model_small_sd, model_small_control, 
  type = 'latex',
  out = 'output/3_3_model_small.tex', 
  model.numbers = T, single.row = T, report = 'vc*s',
  title = 'Models using more precise geographic UTCI matches',
  dep.var.labels = 'Moderate-severe food insecurity',
  order = 
    c('Constant',
      'hot_week_10P',
      'sd_area_mean',
      'area_sqkm',
      'areaUrban', 'age', 'partner','genderMale', 'children',
      'log_precip_week', 'hotdays_year', 'utci.avg.state', 
      'year2015', 'year2016', 'year2017'),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'SD area', 
      'Size area (ln)',
      'Area: Urban', 'Age','Gender: Male','Partner: Yes','Children: Yes',
      'Precipitation (ln)', 'Hot days in the last 365 days', 'Year-round UTCI in subregion', 
      '2015', '2016', '2017') )


##############################################################################@
# 4 | Specifications  -------------------------------
##############################################################################@


##############################################################################@
#### 1 | Non-linearity  -------------------------------
##############################################################################@

# GLMER binomial 
m_binomial <- lme4::glmer(
  fi_binary ~ hot_week_10P +
    area + children + age + gender + partner +
    utci.avg.state + log_precip_week + hotdays_year + year + 
    (1 | region.mix/country.mix), data=data, family = binomial(link='logit'))
summary(m_binomial)

# GLMER poisson 
m_poisson <- lme4::glmer(
  fi_index ~ hot_week_10P +
    area + children + age + gender + partner +
    utci.avg.state + log_precip_week + hotdays_year + year + 
    (1 | region.mix/country.mix), data=data, family = poisson)
summary(m_poisson)

# GLMER poisson 
m_poisson_per <- lme4::glmer(
  fi_index ~ percentile +
    area + children + age + gender + partner +
    utci.avg.state + log_precip_week + hotdays_year + year + 
    (1 | region.mix/country.mix), data=data, family = poisson)
summary(m_poisson_per)

# GLMER poisson 
m_poisson_abs <- lme4::glmer(
  fi_index ~ mean_utci_mix + log_gni + 
    area + children + age + gender + partner +
    utci.avg.state + log_precip_week + hotdays_year + year + 
    (1 | region.mix/country.mix), data=data, family = poisson) 
summary(m_poisson_abs)

stargazer(
  m_binomial, m_poisson, m_poisson_per, m_poisson_abs, 
  type='latex', df=T, single.row=T, report='vc*s', digits=4, 
  title = 'Non-linear models',
  out='output/4_1_models_nonlinear.tex', 
  model.numbers=T, 
  dep.var.labels=c('Moderate-severe FI', 'Index', 'Index', 'Index'),
  order=
    c('Constant',
      'hot_week_10P',
      'percentile', 
      'mean_utci_mix', 
      'area','age','partner','genderMale','children',
      'log_gni', 
      'log_precip_week', 'utci.avg.country', 'hotdays_year', 
      'year'),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'Percentile on day',
      'UTCI on day', 
      'Area: Urban', 'Age','Gender: Male','Partner: Yes','Children: Yes',
      'GNI per capita (ln)',
      'Precipitation (ln)', 'Year-round UTCI', 'Hot days in last 365 days',
      '2015', '2016', '2017'), 
  add.lines = list(c('Model type', 'Logit', 'Poisson', 'Poisson', 'Poisson')))

##############################################################################@
#### 2 | Binary / Continuous ------------------------------------
##############################################################################@

m_mean_utci <- lme4::lmer(
  fi_binary ~ mean_utci_mix +
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + log_gni + 
    year + 
    (1 | country.mix/region.mix), data=data) 
summary(m_mean_utci)

# the higher the percentile, the hotter the day (hotter than X% of days)
m_percentile <- lme4::lmer(
  fi_binary ~ percentile + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year +  
    year + 
    (1 | country.mix/region.mix), data=data) 
summary(m_percentile)

data <- 
  data %>% 
  mutate(
    utci.4bin = cut(mean_utci_mix, breaks=seq(from=-26, to=42, by=4)) 
  )

data$utci.4bin = relevel(as.factor(data$utci.4bin), '(14,18]')

m_4bin <- lme4::lmer(
  fi_binary ~ as.factor(utci.4bin) + log_gni + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year +  
    year + as.factor(relative.season) +
    (1 | country.mix/region.mix), data=data) 
summary(m_4bin)

stargazer(
  m_mean_utci, m_percentile, m_4bin, 
  df=T, single.row=T, report='vc*s', model.numbers=T, 
  omit = c('2015', '2016', '2017'), 
  notes = 'Fixed effects for the year are not displayed in table',
  title='Models using continuous heat measures',
  type='latex', 
  out='output/4_2_models_continuous.tex', 
  dep.var.labels=c('Moderate-severe food insecurity') ,
  order=
    c('Constant',
      'mean_utci_mix', 
      'percentile',
      'utci.4bin', 
      'area','age','partner','genderMale','children',
      'log_gni',
      'log_precip_week', 'utci.avg.country', 'hotdays_year', 
      'year'), 
  covariate.labels = 
    c('Intercept',
       'Daily UTCI',
       'Daily Percentile',
       '(-26,-22]',
       '(-22,-18]',
       '(-18,-14]',
       '(-14,-10]',
       '(-10,-6]',
       '(-6, -2]',
       '(-2, 2]',
       '(2, 6]',
       '(6, 10]',
       '(10, 14]',
       #'(14, 18]',
       '(18, 22]',
       '(22, 26]',
       '(26, 30]',
       '(30, 34]',
       '(34, 38]',
       '(38, 42]',
       'Area: Urban','Age','Gender: Male', 'Partner: Yes','Children: Yes',
       'GNI per capita (ln)',
       'Precipitation (ln)', 'Year-round UTCI', 'Hot days in the last 365 days',
       '2015', '2016', '2017') )


##############################################################################@
#### 3 | Absolute / Relative heat ----------------------------
##############################################################################@

# ABSOLUTE
##############################################################################@

model_26_week <- lme4::lmer(
  fi_binary ~ hot_week_26 +
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + log_gni + 
    year + 
    (1 | country.mix/region.mix), data=data, control=lmerControl(optimizer='bobyqa') ) 
summary(model_26_week)

model_28_week <- lme4::lmer(
  fi_binary ~ hot_week_28 +
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + log_gni + 
    year + 
    (1 | country.mix/region.mix), data=data, control=lmerControl(optimizer='bobyqa')) 
summary(model_28_week)

model_30_week <- lme4::lmer(
  fi_binary ~ hot_week_30 +
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + log_gni + 
    year + 
    (1 | country.mix/region.mix), data=data, control=lmerControl(optimizer='bobyqa')) 
summary(model_30_week)

stargazer(
  model_30_week, model_28_week, model_26_week,
  type = 'latex', df = T, single.row = T, report = 'vc*s', model.numbers = T,
  title = 'Models using absolute heat measures',
  out = 'output/4_3_models_absolute.tex', 
  dep.var.labels = c('Moderate-severe food insecurity'),
  order =
    c('Constant',
      'hot_week_30', 
      'hot_week_28', 
      'hot_week_26',
      'area','age','partner','genderMale','children',
      'log_gni', 
      'log_precip_week', 'utci.avg.country', 'hotdays_year', 
      'year'),
  covariate.labels = 
    c('Intercept',
      'Hot week: 30',
      'Hot week: 28',
      'Hot week: 26',
      'Area: Urban', 'Age', 'Gender: Male', 'Partner: Yes', 'Children: Yes',
      'GNI per capita', 
      'Precipitation (ln)', 'Year-round UTCI', 'Hot days in the last 365 days',
      '2015', '2016', '2017') )

# Check which countries are considered to have experienced a 'hot week' at 30

cor(data$hot_week_26, data$utci.avg.state, method='pearson', use='complete.obs')
cor(data$percentile, data$log_gni, method='pearson', use='complete.obs')
cor(data$mean_utci_mix, data$log_gni, method='pearson', use='complete.obs')

plot_hot30 <- 
  map_hot30 <- 
  merge(
    countries, 
    data %>% 
      filter(hot_week_30 ==1) %>% 
      group_by(ISO_3) %>% 
      reframe(n_hot = n()), 
    by='ISO_3', all.x=T) %>% 
  ggplot() + 
  geom_sf(aes(fill = n_hot), size = 0.00) +
  labs(x='Number of observations', y = '') +
  ggtitle('') +
  scale_fill_viridis(option='viridis', name='') +
  theme_classic() +  
  theme(legend.position='bottom', legend.box = 'horizontal', legend.key.size = unit(0.5, 'cm'),
        text = element_text(size = 12, family='Times New Roman'), 
        axis.line = element_line(color='white'),
        axis.text=element_blank(), 
        axis.ticks=element_blank())

ggsave(plot_hot30, file='output/4_3_plot_hot30.pdf', 
       device= cairo_pdf, 
       width=7, height=3) # 10 and 4

# Fit without utci.avg.state
#############################################################################@

m_26_week_no <- lme4::lmer(
  fi_binary ~ hot_week_26 +
    area + children + age + partner + gender + 
    log_precip_week + hotdays_year + log_gni + 
    year + 
    (1 | country.mix), data=data) 
summary(m_26_week_no)

m_28_week_no <- lme4::lmer(
  fi_binary ~ hot_week_28 +
    area + children + age + partner + gender + 
    log_precip_week + hotdays_year + log_gni + 
    year + 
    (1 | country.mix), data=data) 
summary(m_28_week_no)

m_30_week_no <- lme4::lmer(
  fi_binary ~ hot_week_30 +
    area + children + age + partner + gender + 
    log_precip_week + hotdays_year + log_gni + 
    year + 
    (1 | country.mix), data=data) 
summary(m_30_week_no)

stargazer(
  m_30_week_no, m_28_week_no, m_26_week_no,
  type='latex', df=T, single.row=T, report='vc*s', model.numbers=T,
  title='Models using absolute heat measures without controlling for year-round UTCI',
  out='output/4_3_models_absolute_noUTCI.tex', 
  dep.var.labels=c('Moderate-severe food insecurity'),
  order=
    c('Constant',
      'hot_week_30', 
      'hot_week_28', 
      'hot_week_26',
      'area','age','partner','genderMale','children',
      'log_gni', 
      'log_precip_week', 'hotdays_year', 
      'year'),
  covariate.labels = 
    c('Intercept',
      'Hot week: 30',
      'Hot week: 28',
      'Hot week: 26',
      'Area: Urban', 'Age', 'Gender: Male', 'Partner: Yes', 'Children: Yes',
      'GNI per capita', 
      'Precipitation (ln)', 'Hot days in the last 365 days',
      '2015', '2016', '2017') )


# RELATIVE 
##############################################################################@

model_15P <- lmerTest::lmer(
  fi_binary ~ hot_week_15P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data=data) 
summary(model_15P)
confint(model_15P, parm='hot_week_15P', level=0.95)

# 10 P is our main model 
model_10P <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year +  year + 
    (1 | country.mix/region.mix), data=data) 
summary(model_10P)

model_05P <- lme4::lmer(
  fi_binary ~ hot_week_05P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year +  year + 
    (1 | country.mix/region.mix), data=data, control=lmerControl(optimizer='bobyqa')) 
summary(model_05P)
confint(model_05P, parm='hot_week_05P', level=0.95)

stargazer(
  model_05P, model_10P, model_15P, 
  type='latex', df=T, single.row=T, report='vc*s', model.numbers=T, 
  title='Models using relative heat measures',
  out=paste0('output/',
             '4_3_relative_heat_week.tex'), 
  dep.var.labels=c('Moderate-severe food insecurity') ,
  order=
    c('Constant',
      'hot_week_05P', 
      'hot_week_10P',
      'hot_week_15P',
      'area','age','partner','genderMale','children',
      'log_precip_week', 'utci.avg.country', 'hotdays_year', 
      'year'),
  covariate.labels = 
    c('Intercept',
      'Hot week: 05P',
      'Hot week: 10P',
      'Hot week: 15P',
      'Area: Urban', 'Age', 'Gender: Male', 'Partner: Yes', 'Children: Yes',
      'Precipitation (ln)', 'Year-round UTCI', 'Hot days in the last 365 days',
      '2015', '2016', '2017') )

# Check how many observations with >30 degree UTCI there are by continent
##############################################################################@

data %>% 
  group_by(continent) %>% 
  reframe(n_30 = sum(mean_utci_mix > 30), 
            total_obs = n(), 
            share_30 = 100*n_30/total_obs)

# Plot thresholds
##############################################################################@

plot_thresholds.10P <- 
  data %>% 
  ggplot() + 
  geom_density(aes(threshold_10P), fill='#31688e', col='#31688e', alpha=0.10) + 
  geom_vline(xintercept = summary(data$threshold_10P)['1st Qu.'], linetype=2) +
  geom_vline(xintercept = summary(data$threshold_10P)['Median'], linetype=2) +
  geom_vline(xintercept = summary(data$threshold_10P)['3rd Qu.'], linetype=2) + 
  annotate('label', x=summary(data$threshold_10P)['1st Qu.'], y=0.01, col='black', fontface=2, label='1st Quartile: 21.5ºC', size=8, family='Times New Roman') +
  annotate('label', x=summary(data$threshold_10P)['Median'], y=0.02, col='black', fontface=2, label='Median: 26.6ºC', size=8, family='Times New Roman') +
  annotate('label', x=summary(data$threshold_10P)['3rd Qu.'], y=0.03, col='black', fontface=2, label='3rd Quartile: 30.8ºC', size=8, family='Times New Roman') + 
  labs(x='UTCI threshold for 10% hottest days in the subregion', y='Density of observations') +
  theme_classic() + 
  theme(text = element_text(size = 18, family='Times New Roman'), 
        axis.text = element_text(size=18) )

plot_thresholds.05P <- 
  data %>% 
  ggplot() + 
  geom_density(aes(threshold_05P), fill='#31688e', col='#31688e', alpha=0.10) + 
  geom_vline(xintercept = summary(data$threshold_05P)['1st Qu.'], linetype=2) +
  geom_vline(xintercept = summary(data$threshold_05P)['Median'], linetype=2) +
  geom_vline(xintercept = summary(data$threshold_05P)['3rd Qu.'], linetype=2) + 
  annotate('label', x=summary(data$threshold_05P)['1st Qu.'], y=0.01, col='black', fontface=2, label='1st Quartile: 22.9ºC', size=8, family='Times New Roman') +
  annotate('label', x=summary(data$threshold_05P)['Median'], y=0.02, col='black', fontface=2, label='Median: 27.7ºC', size=8, family='Times New Roman') +
  annotate('label', x=summary(data$threshold_05P)['3rd Qu.'], y=0.03, col='black', fontface=2, label='3rd Quartile: 31.4ºC', size=8, family='Times New Roman') + 
  labs(x='UTCI threshold for 5% hottest days in the subregion', y='Density of observations') +
  theme_classic() + 
  theme(text = element_text(size = 18, family='Times New Roman'), 
        axis.text = element_text(size=18) )

plot_thresholds <- ggarrange(plot_thresholds.05P, plot_thresholds.10P, labels=c('A','B'))

ggsave(plot_thresholds, file='output/4_3_plot_thresholds.pdf', 
       width=15, height=6, device = cairo_pdf)


###############################################################################@
#### 4 | Mild / Moderate / Severe FI -----------------------------------
###############################################################################@

model_index <- lme4::lmer(
  fi_index ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), 
  data=data) 

model_worried <- lme4::lmer(
  WP14778 ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), 
  data=data) 

stargazer(model_index, model_mild, model_moderate, model_severe, 
          type='latex', df=T, single.row=F, report='vc*s', model.numbers=T, 
          title='Different measures of food insecurity',
          out='output/4_4_fi_variables.tex', 
          dep.var.labels=c('Index', 'Mild-to-severe', 'Moderate-to-severe', 'Severe') ,
          omit='year', 
          notes = 'Models include fixed effects for the year', 
          order=c('Constant',
                  'hot_week_10P',
                  'area','age','partner','genderMale','children',
                  'log_precip_week', 'utci.avg.country', 'hotdays_year', 
                  'year'),
          covariate.labels = c('Intercept',
                               'Hot week',
                               'Area: Urban',
                               'Age',
                               'Gender: Male', 
                               'Partner: Yes', 
                               'Children: Yes',
                               'Precipitation (ln)', 
                               'Year-round UTCI', 
                               'Hot days in last 365 days',
                               '2015', '2016', '2017') )


###############################################################################@
# 5 | Temporal -----------------------------------
###############################################################################@


###############################################################################@
#### 1 | Year long / seasonality -----------------------------------
###############################################################################@

# stratified: #years x 12 (4*12) 
###############################################################################@

m_strat_s <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + 
    as.factor(relative.season)/year + 
    (1 | country.mix/region.mix), data=data) 

m_strat_m <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + 
    as.factor(month)/year + 
    (1 | country.mix/region.mix), data=data) 
# rank deficient 

stargazer(
  m_strat_s, m_strat_m, 
  type = 'latex',
  out = 'output/5_1_models_stratified.tex', 
  single.row = T, report = 'vc*s', model.numbers = T, 
  omit = c('month', 'relative.season', 'year'),
  title ='Time-stratified models',
  dep.var.labels = 'Moderate-severe food insecurity',
  order =
    c('Constant',
      'hot_week_10P',
      'area', 'age', 'gender', 'children', 'partner', 
      'log_precip_week', 'utci.avg.state', 
     
      'relative.season',
      'year',
      
      'month', 
      'year', 
      
      'hotdays_year'),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'Area: Urban', 'Age', 'Gender: Male', 'Children <15 in HH', 'Having a partner', 
      'Precipitation (ln)', 'Year-round UTCI in subregion' ))

m_month <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    area + children + age + gender + partner + 
    log_precip_week + utci.avg.state + hotdays_year + year +
    as.factor(month) +
    (1 | country.mix/region.mix), data=data) 

m_season <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    area + children + age + gender + partner + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    as.factor(relative.season) +
    (1 | country.mix/region.mix), data=data) 

stargazer(
  m_month, m_season, 
  type = 'latex',
  out = 'output/5_1_models_factor.tex', 
  single.row = T, report = 'vc*s', model.numbers = T, 
  title = 'Models using fixed effects for months and season',
  dep.var.labels ='Moderate-severe food insecurity',
  order =
    c('Constant',
      'hot_week_10P',
      'area','age','gender','children','partner', 
      'log_precip_week','utci.avg.state', 'hotdays_year',
      'relative.season',
      'month', 
      'year'  ),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'Area: Urban', 'Age','Gender: Male', 'Children <15 in HH', 'Having a partner', 
      'Precipitation (ln)', 'Year-round UTCI in subregion', 'Hot days in the last 365 days', 
      'Second coldest season', 'Second hottest season', 'Hottest season', 
      'February', 'March', 'April', 'May', 'June', 
      'July', 'August', 'September', 'October', 
      'November', 'December',
      '2015', '2016', '2017') )

m_hottest_week <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    area + children + age + partner + gender +
    log_precip_week + utci.avg.state + hotdays_year + year +
    (1 | country.mix/region.mix), 
  data=data %>% filter(relative.season==4 | relative.season==3)) 
summary(m_hottest_week)

m_constant <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    area + children + age + partner + gender + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), 
  data=data %>% filter(sd_utci_year > 10)) 

stargazer(
  m_hottest_week, m_constant, 
  type='latex',
  out='output/5_1_models_subset.tex', 
  single.row=T, report='vc*s', model.numbers=T, 
  title='Models using different seasonal subsets of the data',
  dep.var.labels='Moderate-severe food insecurity',
  order=
    c('Constant',
      'hot_week_10P',
      'area', 'age', 'gender', 'children', 'partner', 
      'log_precip_week', 'utci.avg.state', 'hotdays_year',
      'year'  ),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'Area: Urban','Age','Gender: Male', 'Children <15 in HH', 'Having a partner', 
      'Precipitation (ln)', 'Year-round UTCI in subregion', 'Hot days in the last 365 days', 
      '2015', '2016', '2017'))


##############################################################################@
#### 2  | Short term effects --------------------------------------------
##############################################################################@

# Lags
##############################################################################@

m_days <- lme4::lmer(
  fi_binary ~ as.factor(hotdays_week_10P) + 
    area + children + age + gender + partner + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data=data) 

model_lag_hot <- lme4::lmer(
  fi_binary ~ 
    hot_day_10P + 
    day_1_hot + day_2_hot + day_3_hot +
    day_4_hot + day_5_hot + day_6_hot + 
    area + children + age + gender + partner + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data=data) 


model_lag_utci <- lme4::lmer(
  fi_binary ~ 
    mean_utci_mix + 
    day_1_utci + day_2_utci + day_3_utci +
    day_4_utci + day_5_utci + day_6_utci + 
    area + children + age + gender + partner + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data=data) 
summary(model_lag_utci)

stargazer(
  m_days, model_lag_hot, 
  type = 'latex',
  out = 'output/5_2_model_lags.tex', 
  single.row = T, report = 'vc*s', model.numbers = T, 
  title = 'Models using different lagged heat variables',
  dep.var.labels = 'Moderate-severe food insecurity',
  order=
    c('Constant',
      'hotdays_week_10P', 
      'hot_day_10P','day_1_hot', 'day_2_hot', 'day_3_hot', 'day_4_hot', 
      'day_5_hot', 'day_6_hot', 
      'area', 'age', 'gender', 'children', 'partner', 
      'log_precip_week', 'utci.avg.state','hotdays_year', 
      'year'),
  covariate.labels = 
    c('Intercept',
      '1 hot day in last week', 
      '2 hot day in last week', 
      '3 hot day in last week', 
      '4 hot day in last week', 
      '5 hot day in last week', 
      '6 hot day in last week', 
      '7 hot day in last week', 
     
     'Current day: Hot', 
     'Current day -1: Hot', 
     'Current day -2: Hot', 
     'Current day -3: Hot', 
     'Current day -4: Hot', 
     'Current day -5: Hot', 
     'Current day -6: Hot', 
     
     'Area: Urban','Age', 'Gender: Male', 'Children <15 in HH', 'Having a partner', 
     'Precipitation (ln)', 'Year-round UTCI in subregion', 'Hot days in last year', 
     '2015', '2016', '2017') )


# Last and second to last week
##############################################################################@

model_1week <- lme4::lmer(
  fi_binary ~  hotdays_week_10P +
    area + children + age + gender + partner + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data=data) 
summary(model_1week)

model_2week <- lme4::lmer(
  fi_binary ~ hotdays_2week + 
    area + children + age + gender + partner + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data=data, control=lmerControl(optimizer='bobyqa')) 


model_1and2week <- lme4::lmer(
  fi_binary ~ hotdays_week_10P + hotdays_2week +
    area + children + age + gender + partner + 
    log_precip_week + utci.avg.state + hotdays_year + year + 
    (1 | country.mix/region.mix), data=data) 
summary(model_1and2week)

stargazer(
  model_1week, model_2week, model_1and2week, 
  type='latex',
  out='output/5_2_model_timeframe.tex', 
  single.row=T, report='vc*s', model.numbers=T, 
  title = 'Models using different time frames for heat measures',
  dep.var.labels='Moderate-severe food insecurity',
  order=
    c('Constant',
      'hotdays_week_10P', 
      'hotdays_2week',
      'hot_week_10P',
      'area', 'age', 'gender', 'children', 'partner', 
      'log_precip_week', 'utci.avg.state','hotdays_year', 
      'year'),
  covariate.labels = 
    c('Intercept',
      'Hot days in last week',
      'Hot days in second to last week',
      'Area: Urban','Age', 'Gender: Male', 'Children <15 in HH','Having a partner', 
      'Precipitation (ln)', 'Year-round UTCI in subregion', 'Hot days in last year', 
      '2015', '2016', '2017') )


##############################################################################@
# 6 | Sensitivity  -------------------------------------------------
##############################################################################@


##############################################################################@
#### 1 | Drop countries  ------------------------------------------------------
##############################################################################@

# Time saver 

# Sequentially drop countries
x1 <-  as.vector(unique(data$country.mix))
x1 <- sort(x1)

# Set up data frame to store results 
df_drop <- data.frame(effect=character(),
                      term=character(),
                      estimate=numeric(),
                      std.error=numeric(),
                      statistic=numeric(),
                      df=numeric(), 
                      p.value=numeric(),
                      conf.low=numeric(),
                      conf.high=numeric(),
                      country=character(),
                      stringsAsFactors=FALSE)

# Run loop to create lm fit, vcov, coefplot and coeftest
for (i in x1) {
  
  # Live update which model is being fitted
  print(i)
  
  # Fit model
  data_drop = data %>% filter(country.mix!=i)
  model <- lmer(fi_binary ~ hot_week_10P +
                    area + children + age + partner + gender + 
                    log_precip_week + utci.avg.state + hotdays_year + year + 
                    (1 | country.mix/region.mix), data=data_drop) 

  # Extract results 
  coefficients <- model %>% tidy(effects = 'fixed', conf.int=T)
  coefficients <- coefficients %>% mutate(country = paste(i))
  
  # Bind together 
  df_drop = rbind(df_drop, coefficients)  }

write_csv(df_drop, 'time-savers/df_drop.csv')

# Add the DF BETA to the df_drop
df_drop <- 
  df_drop %>%
  mutate(ISO_3 = countrycode(country, origin='country.name', destination='iso3c'), 
         ISO_3 = ifelse(country=='Kosovo', 'XKO', ISO_3),
         ISO_3 = ifelse(country=='Northern Cyprus', 'CYP2', ISO_3),
         
         beta_full = fixef(model_base)[['hot_week_10P']], 
         dfbeta = beta_full - estimate, 
         n = nobs(model_base), 
         threshold = 2/ (sqrt(n) ), 
         mean_beta = mean(estimate, na.rm=T))

plot_drop <- 
  df_drop %>% 
  filter(term=='hot_week_10P') %>% 
  ggplot() + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, x=reorder(ISO_3, -estimate)), col='grey', width=0.01) +
  geom_point(aes(x=reorder(ISO_3, -estimate), y=estimate)) +
  labs(x='Dropped country', y='Coefficient') + 
  geom_hline(yintercept=0, col='grey', linetype='dashed') + 
  theme_classic() + 
  theme(text = element_text(size = 12, family = 'Times New Roman'), 
        axis.text.y = element_text(size = 12, family = 'Times New Roman'), 
        #axis.text.x = element_text(size = 5, angle = 90, hjust = 1)
        axis.text.x = element_blank(), axis.ticks.x = element_blank()    ) 

plot_dfbeta <- 
  df_drop %>%
  filter(term == 'hot_week_10P') %>% 
  ggplot() + 
  geom_point(aes(x=ISO_3, y=dfbeta), size=0.5) +
  geom_segment(aes(x=ISO_3, y=dfbeta, xend=ISO_3, yend=0)) + 
  geom_hline(yintercept=0, col='grey', linetype='dashed') +
  geom_hline(aes(yintercept=threshold), col='red') + 
  geom_hline(aes(yintercept=-threshold), col='red') + 
  geom_text(aes(x=ISO_3, y=dfbeta, 
                label=ifelse(dfbeta>0.00075 | dfbeta < -0.00075,as.character(ISO_3),'')),hjust=0,vjust=0, 
            family = 'Times New Roman', size=4) + 
  labs(x='Dropped country', y='DFBETA') +
  theme_classic() + 
  theme(text = element_text(size = 12, family='Times New Roman'),
        axis.text.y = element_text(size = 12, family = 'Times New Roman'), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) 

plots_dfbeta <- plot_grid(plot_drop, plot_dfbeta,
                      labels = c('A', 'B'), align = 'hv', axis = 't',
                      ncol=1,
                      scale=0.9, rel_heights = c(1,0.6))

ggsave(plots_dfbeta, file='output/6_1_plots_dfbeta.pdf', 
       width=8, height=5, dpi=300, device = cairo_pdf)

###############################################################################@
#### 2 | Covariates -----------------------------------
###############################################################################@

model_noc <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    (1 | country.mix/region.mix), 
  data=data) 
summary(model_noc) 

model_sec <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    area + children + age + partner + gender + # individual covariates
    (1 | country.mix/region.mix), 
  data=data) 
summary(model_sec) 

model_weather <- lme4::lmer(
  fi_binary ~ hot_week_10P + 
    log_precip_week + utci.avg.state + hotdays_year + # heat controls 
    year + 
    (1 | country.mix/region.mix), 
  data=data) 
summary(model_weather) 

# noc
num_region_noc <- as.numeric(sapply(ranef(model_noc),nrow)[1])
num_country_noc <- as.numeric(sapply(ranef(model_noc),nrow)[2])
sd_region_noc <- round(as.numeric(attributes(VarCorr(model_noc)$'region.mix')$stddev), 4)
sd_country_noc <- round(as.numeric(attributes(VarCorr(model_noc)$'country.mix')$stddev), 4)

num_region_sec <- as.numeric(sapply(ranef(model_sec),nrow)[1])
num_country_sec <- as.numeric(sapply(ranef(model_sec),nrow)[2])
sd_region_sec <- round(as.numeric(attributes(VarCorr(model_sec)$'region.mix')$stddev), 4)
sd_country_sec <- round(as.numeric(attributes(VarCorr(model_sec)$'country.mix')$stddev), 4)

num_region_weather <- as.numeric(sapply(ranef(model_weather),nrow)[1])
num_country_weather <- as.numeric(sapply(ranef(model_weather),nrow)[2])
sd_region_weather <- round(as.numeric(attributes(VarCorr(model_weather)$'region.mix')$stddev), 4)
sd_country_weather <- round(as.numeric(attributes(VarCorr(model_weather)$'country.mix')$stddev), 4)

stargazer(
  model_noc, model_sec, model_weather, 
  df=T, single.row=T, report='vc*s', digits = 4, 
  type='latex', 
  out='output/6_2_models_covariates.tex', 
  title = 'Models using different sets of covariates',
  odel.numbers=T, dep.var.labels='Moderate-severe food insecurity',
  order=
    c('Constant',
      'hot_week_10P',
      'areaUrban', 'age', 'partner', 'genderMale', 'children',
      'log_precip_week', 'hotdays_year', 'utci.avg.state',
      'year2015', 'year2016', 'year2017'),
  covariate.labels = 
    c('Intercept',
      'Hot week',
      'Area: Urban',  'Age', 'Partner: Yes', 'Gender: Male', 'Children: Yes',
      'Precipitation (ln)', 'Hot days in the last 365 days', 'Year-round UTCI in subregion',
      '2015', '2016', '2017'), 
  add.lines = 
    list(c('Number of subregions', num_region_noc, num_region_sec, num_region_weather),
         c('Number of countries', num_country_noc, num_country_sec, num_country_weather),
         c('Standard deviation of subregions', sd_region_noc, sd_region_sec, sd_region_weather),
         c('Standard deviation of countries', sd_country_noc, sd_country_sec, sd_country_weather)))


##############################################################################@
# 7 | R session ---------------------------------------------------------------
##############################################################################@

library(report)

# Get list of reported pacakges and version
report_packages(include_R = FALSE)

# get references for packages 
session <- sessionInfo()
r <- report(session)
r

writeLines(capture.output(sessionInfo()), "output/7_1_sessionInfo.htm")

