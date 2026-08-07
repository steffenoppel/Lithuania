### ##################################################
### LITHUANIA SEABIRD BYCATCH ANALYSIS - EVALUATION OF KITES AND NIGHT FISHING
### written by steffen.oppel@vogelwarte.ch
### ##################################################

## goal is to examine whether kites and night fishing reduce seabird bycatch
## also test whether fish catch is maintained
## major complication is that some sets declared as 'night' fishing straddle daytime
## these are specified in Adj_trial_types and Adj_trial subtypes

## 9 Feb 2024: added a compositional analysis to test whether bycatch depth distribution and effort are significantly different
## 9 Feb 2024: added species-specific analyses
## re-run on 30 August 2024 after receiving final data from Rasa Morkune
## updated on 25 Sept to use updated definition of 'night' (many fisherman were very liberal with that term)
## revised analysis based on Julius Morkunas email suggestion (7 Oct 2024): create matrix of sunset and sunrise time diffs

### UPDATED 9 AUG 2026: reviewer demanded BCAboot and NB GLM

## added tests: will GLM meet distribution assumptions?
## does boot distribution approach stable value?

### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(MASS)
library(pscl)
library(DHARMa)
library(performance)
library(AER)
library(marginaleffects)
filter<-dplyr::filter
select<-dplyr::select





#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     DATA IMPORT AND MANIPULATION -------------------------
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

try(setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\Lithuania"), silent=T)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\Lithuania"), silent=T)

# Read the data from formatted CSV files (one for each mitigation trial)
data<-readRDS("data/LIT_bycatch_data_formatted.rds")
head(data)
dim(data)
unique(data$Season)
length(unique(data$Trip_ID))


### because fish catch should be compared irrespective of time, we recalculate CPUE
data <- data %>%
  mutate(CPUE=catch/Total_net_area) %>% ## not taking soak time into account because that is subject to experimental manipulation
  mutate(night=if_else(TrialType2hSS=="Night",1,0), kite=if_else(Trial_type_by_fishermen=="Kites",1,0))


# ensure variables are coded correctly
data$night <- factor(data$night)
data$kite <- factor(data$kite)

# response must be integer counts
summary(data$bycatch)
table(data$bycatch)



# 2.   EXPLORE RESPONSE DISTRIBUTION -------------------------------
hist(data$bycatch,
     breaks = 20,
     main = "Bycatch distribution",
     xlab = "Bycatch")

mean(data$bycatch)
var(data$bycatch)

prop_zero <- mean(data$bycatch == 0)

cat("Mean =", mean(data$bycatch), "\n")
cat("Variance =", var(data$bycatch), "\n")
cat("Proportion zeros =", prop_zero, "\n")


# 3.   POISSON GLM -------------------------------
m_pois <- glm(
  bycatch ~ Effort + night * kite,
  family = poisson,
  data = data
)

summary(m_pois)

## 3.1. assess overdispersion -------

dispersion <- sum(residuals(m_pois, type = "pearson")^2) /
  df.residual(m_pois)

dispersion  ## 1 = acceptable, 1.5 = overdispersion, 2 = serious overdispersion; we have 3 = off-the-chart overdispersion!!
AER::dispersiontest(m_pois)

res_pois <- simulateResiduals(m_pois)
plot(res_pois)

testDispersion(res_pois)
testZeroInflation(res_pois)


## 3.2. predict response -------
plotdat<-expand.grid(night=unique(data$night),kite=unique(data$kite)) %>% mutate(Effort=12322.71)

OUT<-cbind(plotdat, 
      Mean = predict(m_pois, newdata = plotdat, type = "response"), 
      SE = predict(m_pois, newdata = plotdat, type = "response", se.fit = T)$se.fit
) %>%
  mutate(MODEL="Poisson", lcl=Mean-1.96*SE, ucl=Mean+19.6*SE)
OUT





# 4.   NEGATIVE BINOMIAL GLM -------------------------------

m_nb <- MASS::glm.nb(
  bycatch ~ Effort + night * kite,
  data = data
)

summary(m_nb)
AIC(m_pois, m_nb) ## fits better than the Poisson
odTest(m_nb, alpha=.05, digits = max(3, getOption("digits") - 3))  ### Poisson does not fit

## 4.1. assess model fit -------
1 - pchisq(summary(m_nb)$deviance,
           summary(m_nb)$df.residual
)  ## GOF test suggests no fit at all!!

res_nb <- simulateResiduals(m_nb)
plot(res_nb)

testDispersion(res_nb)
testZeroInflation(res_nb)




## 4.2. predict response -------
OUT<-cbind(plotdat, 
      Mean = predict(m_nb, newdata = plotdat, type = "response"), 
      SE = predict(m_nb, newdata = plotdat, type="response", se.fit = T)$se.fit
) %>%
  mutate(MODEL="NegBin", lcl=Mean-1.96*SE, ucl=Mean+19.6*SE) %>%
  bind_rows(OUT)
OUT






# 5.   ZERO-INFLATED POISSON -------------------------------

m_zip <- zeroinfl(
  bycatch ~ Effort + night * kite |
    Effort + night + kite,
  dist = "poisson",
  data = data
)

summary(m_zip)




# 6.   ZERO-INFLATED NEGATIVE BINOMIAL -------------------------------

m_zinb <- zeroinfl(
  bycatch ~ Effort + night * kite |
    Effort + night + kite,
  dist = "negbin",
  data = data
)

summary(m_zinb)


## 6.1. assess model fit -------
res_zinb <- simulateResiduals(m_zinb)
plot(res_zinb)

testDispersion(res_zinb)
testZeroInflation(res_zinb)




# 7.   HURDLE MODEL -------------------------------

m_hurdle <- hurdle(
  bycatch ~ Effort + night * kite,
  dist = "negbin",
  data = data
)

summary(m_hurdle)



# 8.   MODEL COMPARISON -------------------------------
AIC(
  m_pois,
  m_nb,
  m_zip,
  m_zinb,
  m_hurdle
)




# 9.   NEGATIVE BINOMIAL GLM FITS BEST - EXPLORE ALTERNATIVES-------------------------------

m_nb3 <- MASS::glm.nb(bycatch ~ Effort + night * kite,  data = data)
#m_nb2 <- MASS::glm.nb(bycatch ~ Effort + night : kite,  data = data)
m_nb1 <- MASS::glm.nb(bycatch ~ Effort + night + kite,  data = data)
AIC(m_nb1, m_nb3) 
summary(m_nb1)

## 4.1. assess model fit -------
1 - pchisq(summary(m_nb)$deviance,
           summary(m_nb)$df.residual
)  ## GOF test suggests no fit at all!!

res_nb <- simulateResiduals(m_nb)
plot(res_nb)

testDispersion(res_nb)
testZeroInflation(res_nb)




## 4.2. predict response -------
OUT<-cbind(plotdat, 
           Mean = predict(m_nb, newdata = plotdat, type = "response"), 
           SE = predict(m_nb, newdata = plotdat, type="response", se.fit = T)$se.fit
) %>%
  mutate(MODEL="NegBin", lcl=Mean-1.96*SE, ucl=Mean+19.6*SE) %>%
  bind_rows(OUT)
OUT

data$pred <- predict(
  m_nb1,
  type = "response"
)

plot(data$pred,
     data$bycatch,
     xlab = "Predicted",
     ylab = "Observed")
abline(0,1,col="red")


library(emmeans)

emmeans(
  m_nb1,
  ~ night * kite,
  type = "response"
)



# 10. PLOT RESPONSES ------------------------------------

library(MASS)
library(pscl)
library(ggplot2)
library(dplyr)
library(tidyr)

#--------------------------------------------------
# Fit models
#--------------------------------------------------

m_pois <- glm(
  bycatch ~ Effort + night * kite,
  family = poisson,
  data = data
)

m_nb <- glm.nb(
  bycatch ~ Effort + night * kite,
  data = data
)

m_zip <- zeroinfl(
  bycatch ~ Effort + night * kite |
    Effort + night + kite,
  dist = "poisson",
  data = data
)

m_zinb <- zeroinfl(
  bycatch ~ Effort + night * kite |
    Effort + night + kite,
  dist = "negbin",
  data = data
)

m_hurdle <- hurdle(
  bycatch ~ Effort + night * kite,
  dist = "negbin",
  data = data
)

#--------------------------------------------------
# Create prediction dataset
#--------------------------------------------------

newdat <- expand.grid(
  night = levels(data$night),
  kite  = levels(data$kite)
)

newdat$Effort <- mean(data$Effort, na.rm = TRUE)

#--------------------------------------------------
# Function to obtain predictions + 95% CI
#--------------------------------------------------

get_predictions <- function(model, model_name, newdata) {
  
  #-------------------------
  # GLM / Negative Binomial
  #-------------------------
  if (inherits(model, "glm") || inherits(model, "negbin")) {
    
    pred <- predict(
      model,
      newdata = newdata,
      type = "link",
      se.fit = TRUE
    )
    
    out <- newdata
    
    out$fit <- exp(pred$fit)
    out$lwr <- exp(pred$fit - 1.96 * pred$se.fit)
    out$upr <- exp(pred$fit + 1.96 * pred$se.fit)
    
  }
  
  #-------------------------
  # Zero-inflated / Hurdle
  #-------------------------
  else if (
    inherits(model, "zeroinfl") ||
    inherits(model, "hurdle")
  ) {
    
    library(marginaleffects)
    
    pred <- predictions(
      model,
      newdata = newdata,
      type = "response"
    )
    
    out <- newdata
    
    out$fit <- pred$estimate
    out$lwr <- pred$conf.low
    out$upr <- pred$conf.high
    
  } else {
    
    stop(
      "Unsupported model class: ",
      paste(class(model), collapse = ", ")
    )
    
  }
  
  out$model <- model_name
  
  rownames(out) <- NULL
  
  out
}

#--------------------------------------------------
# Collect predictions
#--------------------------------------------------

pred_all <- bind_rows(
  
  get_predictions(m_pois,   "Poisson",      newdat),
  get_predictions(m_nb,     "NegBin",       newdat),
  get_predictions(m_zip,    "ZIP",          newdat),
  get_predictions(m_zinb,   "ZINB",         newdat),
  get_predictions(m_hurdle, "Hurdle NB",    newdat)
  
)

# create treatment labels

pred_all <- pred_all %>%
  mutate(
    treatment = paste0(
      "Night=", night,
      ", Kite=", kite
    )
  )

# inspect estimates
pred_all




ggplot(
  pred_all,
  aes(
    x = treatment,
    y = fit,
    color = model
  )
) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 3
  ) +
  geom_errorbar(
    aes(ymin = lwr, ymax = upr),
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  labs(
    x = "",
    y = "Predicted mean bycatch",
    color = "Model"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )



ggplot(
  pred_all,
  aes(
    x = treatment,
    y = fit
  )
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lwr, ymax = upr),
    width = 0.15
  ) +
  facet_wrap(~ model) +
  labs(
    x = "",
    y = "Predicted mean bycatch"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )










