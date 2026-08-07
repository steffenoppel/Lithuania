### ##################################################
### LITHUANIA SEABIRD BYCATCH ANALYSIS - EVALUATION OF KITES AND NIGHT FISHING
### written by steffen.oppel@vogelwarte.ch
### ##################################################

## goal is to examine whether kites and night fishing reduce seabird bycatch
## added tests: will GLM meet distribution assumptions?
## REVISION IN AUGUST 2026

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
library(emmeans)
filter<-dplyr::filter
select<-dplyr::select



# 1.    DATA IMPORT AND MANIPULATION -------------------------------

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
## cannot estimate SE
m_zip <- zeroinfl(
  bycatch ~ Effort + night* kite |
    Effort + night* kite,
  dist = "poisson",
  data = data
)

summary(m_zip)




# 6.   ZERO-INFLATED NEGATIVE BINOMIAL -------------------------------

m_zinb <- zeroinfl(
  bycatch ~ Effort + night |
    Effort + kite,
  dist = "negbin",
  data = data
)

summary(m_zinb)


## 6.1. assess model fit -------
# res_zinb <- simulateResiduals(m_zinb)
# plot(res_zinb)
# 
# testDispersion(res_zinb)
# testZeroInflation(res_zinb)




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
) %>% arrange(AIC)




# 9.   NEGATIVE BINOMIAL GLM FITS BEST - EXPLORE ALTERNATIVES-------------------------------

m_nb3 <- MASS::glm.nb(bycatch ~ Effort + night * kite,  data = data)
#m_nb2 <- MASS::glm.nb(bycatch ~ Effort + night : kite,  data = data)
m_nb1 <- MASS::glm.nb(bycatch ~ Effort + night + kite,  data = data)
AIC(m_nb1, m_nb3) 
summary(m_nb1)

## 9.1. assess model fit -------
1 - pchisq(summary(m_nb1)$deviance,
           summary(m_nb1)$df.residual
)  ## GOF test suggests no fit at all!!

res_nb <- simulateResiduals(m_nb1)
plot(res_nb)

testDispersion(res_nb)
testZeroInflation(res_nb)




## 9.2. predict response -------


data$pred <- predict(
  m_nb1,
  type = "response"
)

plot(data$pred,
     data$bycatch,
     xlab = "Predicted",
     ylab = "Observed")
abline(0,1,col="red")




emmeans(
  m_nb1,
  ~ night * kite,
  type = "response"
)





## 9.3. predict response with parametric bootstrap -----------------------------
# from https://stat.ethz.ch/pipermail/r-help/2008-December/182806.html



predict.zeroinfl <- function(object, newdata, type = c("response", "prob"),
                             
                             se=FALSE,MC=1000,level=.95,
                             
                             na.action = na.pass, ...)
  
{
  
  type <- match.arg(type)
  
  
  
  ## if no new data supplied
  
  if(missing(newdata)){
    
    rval <- object$fitted.values
    
    if(!is.null(object$x)) {
      
      X <- object$x$count
      
      Z <- object$x$zero
      
    }
    
    else if(!is.null(object$model)) {
      
      X <- model.matrix(object$terms$count, object$model, contrasts = object$contrasts$count)
      
      Z <- model.matrix(object$terms$zero,  object$model, contrasts = object$contrasts$zero)	
      
    }
    
    else {
      
      stop("no X and/or Z matrices can be extracted from fitted model")
      
    }
    
    if(type == "prob") {
      
      mu <- exp(X %*% object$coefficients$count)[,1]
      
      phi <- object$linkinv(Z %*% object$coefficients$zero)[,1]
      
    }
    
  }
  
  else {
    
    mf <- model.frame(delete.response(object$terms$full), newdata, na.action = na.action, xlev = object$levels)
    
    X <- model.matrix(delete.response(object$terms$count), mf, contrasts = object$contrasts$count)
    
    Z <- model.matrix(delete.response(object$terms$zero),  mf, contrasts = object$contrasts$zero)
    
    
    
    mu <- exp(X %*% object$coefficients$count)[,1]
    
    phi <- object$linkinv(Z %*% object$coefficients$zero)[,1]
    
    rval <- (1-phi) * mu
    
  }   
  
  
  
  if(se & !is.null(X) & !is.null(Z)){
    
    require(mvtnorm)
    
    vc <- -solve(object$optim$hessian)
    
    kx <- length(object$coefficients$count)
    
    kz <- length(object$coefficients$zero)
    
    parms <- object$optim$par
    
    if(type!="prob"){
      
      yhat.sim <- matrix(NA,MC,dim(X)[1])
      
      for(i in 1:MC){
        
        cat(paste("MC iterate",i,"of",MC,"\n"))
        
        parms.sim <- rmvnorm(n=1,mean=parms,sigma=vc)
        
        beta <- parms.sim[1:kx]
        
        gamma <- parms.sim[(kx+1):(kx+kz)]
        
        mu.sim <- exp(X%*%beta)[,1]
        
        phi.sim <- object$linkinv(Z%*%gamma)[,1]
        
        yhat.sim[i,] <- (1-phi.sim)*mu.sim
        
      }
      
    }
    
    out <- list()
    
    out$lower <- apply(yhat.sim,2,quantile,(1-level)/2)
    
    out$upper <- apply(yhat.sim,2,quantile,1-((1-level)/2))
    
    out$se <- apply(yhat.sim,2,sd)
    
  }
  
  
  
  ## predicted probabilities
  
  if(type == "prob") {
    
    if(!is.null(object$y)) y <- object$y
    
    else if(!is.null(object$model)) y <- model.response(object$model)
    
    else stop("predicted probabilities cannot be computed for fits with y = FALSE and model = FALSE")
    
    
    
    yUnique <- min(y):max(y)
    
    nUnique <- length(yUnique)
    
    rval <- matrix(NA, nrow = length(rval), ncol = nUnique)
    
    dimnames(rval) <- list(rownames(X), yUnique)
    
    
    
    switch(object$dist,
           
           "poisson" = {
             
             rval[, 1] <- phi + (1-phi) * exp(-mu)
             
             for(i in 2:nUnique) rval[,i] <- (1-phi) * dpois(yUnique[i], lambda = mu)
             
           },
           
           "negbin" = {
             
             theta <- object$theta
             
             rval[, 1] <- phi + (1-phi) * dnbinom(0, mu = mu, size = theta)
             
             for(i in 2:nUnique) rval[,i] <- (1-phi) * dnbinom(yUnique[i], mu = mu, size = theta)
             
           },
           
           "geometric" = {
             
             rval[, 1] <- phi + (1-phi) * dnbinom(0, mu = mu, size = 1)
             
             for(i in 2:nUnique) rval[,i] <- (1-phi) * dnbinom(yUnique[i], mu = mu, size = 1)
             
           })
    
    
    
  }
  
  
  
  if(se)
    
    rval <- list(rval,out)
  
  
  
  rval
  
}







# 10. PLOT RESPONSES ------------------------------------

# 
# #--------------------------------------------------
# # Fit models
# #--------------------------------------------------
# 
# m_pois <- glm(
#   bycatch ~ Effort + night * kite,
#   family = poisson,
#   data = data
# )
# 
# m_nb <- glm.nb(
#   bycatch ~ Effort + night * kite,
#   data = data
# )
# 
# m_zip <- zeroinfl(
#   bycatch ~ Effort + night * kite |
#     Effort + night + kite,
#   dist = "poisson",
#   data = data
# )
# 
# m_zinb <- zeroinfl(
#   bycatch ~ Effort + night * kite |
#     Effort + night + kite,
#   dist = "negbin",
#   data = data
# )

## 10.1.  Create prediction dataset --------------------------------------------

newdat <- expand.grid(
  night = levels(data$night),
  kite  = levels(data$kite)
)

newdat$Effort <- mean(data$Effort, na.rm = TRUE)



## 10.2.  Function to obtain predictions + 95% CI --------------------------------------------------

get_predictions <- function(model, model_name, newdata) {
  
  # GLM / Negative Binomial
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
  
  # Zero-inflated / Hurdle
  else if (
    inherits(model, "zeroinfl") ||
    inherits(model, "hurdle")
  ) {
    
    pred <- predict.zeroinfl(
      model,
      se=T,
      newdata = newdata,
      type = "response"
    )
    
    out <- newdata
    
    out$fit <- pred[[1]]
    out$lwr <- pred[[2]]$lower
    out$upr <- pred[[2]]$upper
    
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

## 10.3. Collect predictions --------------------------------------------------

pred_all <- bind_rows(
  
  get_predictions(m_pois,   "Poisson",      newdat),
  get_predictions(m_nb,     "NegBin",       newdat),
  #get_predictions(m_zip,    "ZIP",          newdat),
  get_predictions(m_zinb,   "ZINB",         newdat)
  #get_predictions(m_hurdle, "Hurdle NB",    newdat)
  
)

## 10.3. create treatment labels ----------------------------------------------

pred_all <- pred_all %>%
  mutate(
    treatment = if_else(kite==0,
                        if_else(night==0,"control","night"),
                        if_else(night==0,"kite","night_kite"))
  )

# inspect estimates
pred_all
pred_all<-pred_all %>%
  mutate(upr=if_else(is.infinite(upr),0.5,upr))




## 10.4. PLOT ALL JOINED ESTIMATES ----------------------------------------------

## 10.4.1 LOAD bootstrap data 
boot<-fread("output/boot_estimates.csv") %>%
  mutate(model=if_else(model=="bca","BCA_boot","simple_boot")) %>%
  dplyr::select(model, treatment, mean, lcl, ucl)

COMP_PLOT<-pred_all %>%
  rename(mean=fit, lcl=lwr, ucl=upr) %>%
  dplyr::select(model, treatment, mean, lcl, ucl) %>%
  bind_rows(boot) %>%
  mutate(model=factor(model, levels=c('simple_boot','BCA_boot','NegBin','Poisson','ZINB'))) %>%



ggplot(
  aes(
    x = model,
    y = mean,
    col=treatment
  )
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lcl, ymax = ucl),
    width = 0.15
  ) +
  facet_wrap(~ treatment, scales="fixed") +
  labs(
    x = "Model type",
    y = "Predicted mean BPUE"
  ) +
  scale_colour_discrete(guide="none")+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.x=element_text(size=12, color="black", angle=45, ,hjust = 1), 
        axis.text.y=element_text(size=16, color="black"), 
        axis.title=element_text(size=20),
        axis.title.y=element_text(margin=margin(0,15,0,0)),
        axis.title.x=element_text(margin=margin(15,0,0,0)), 
        strip.text=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
COMP_PLOT
ggsave("output/BPUE_GLM_assessment.jpg", width=11, height=9)










