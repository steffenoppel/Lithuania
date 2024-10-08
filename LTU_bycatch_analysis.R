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


### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(adehabitatHS)
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


#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    BASIC COMPOSITIONAL ANALYSIS TO TEST WHETHER BYCATCH IS DEPTH-DEPENDENT -----------------
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
### THIS FUNCTION FREEZES IF N depths is > N species
## https://stackoverflow.com/questions/50047339/adehabitat-compana-doesnt-work-or-returns-lambda-nan
## bin depths into 5 m bins

depthprops<-readRDS("data/LIT_bycatch_depths.rds")

## bin depths
depthprops<-depthprops %>%
  gather(key='depth', value='prop',-Type) %>%
  mutate(depth=as.numeric(depth)) %>%
  mutate(depthcat=ifelse(depth<5,"shallow",ifelse(depth>9,"deep","intermediate"))) %>%
  group_by(Type,depthcat) %>%
  summarise(prop=sum(prop)) %>%
  spread(key=depthcat,value=prop)

avail=depthprops %>% ungroup() %>% slice(rep(2, each = 8)) %>% select(-Type)
used=depthprops[-2,] %>% ungroup() %>% select(-Type)
DA<-adehabitatHS::compana(used=used,avail=avail,
                      test = "parametric", rnv=0.001, nrep=500)
str(DA)

## Eigenanalysis of selection ratios
ii <- eisera(used=round(used), avail=avail, scannf = FALSE)
scatter(ii, grid = FALSE, clab = 0.7)


### fairly uninformative analysis - there is no 'selection' of bycatch to occur in certain water depths and effort in deep water was too low


#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    BASIC BOOTSTRAP ANALYSIS COMPARING BPUE AND CPUE GLOBALLY -------------------------------
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

### SWITCHED TO TrialType2hSS

# controls<- data %>% filter(Trial_type_by_fishermen=="Control") 
# kites<- data %>% filter(Trial_type_by_fishermen=="Kites")
# nights<- data %>% filter(Trial_type_by_fishermen=="Night")
controls<- data %>% filter(TrialType2hSS=="Control") %>% filter(Trial_type_by_fishermen!="Kites")
kites<- data %>% filter(TrialType2hSS=="Control") %>% filter(Trial_type_by_fishermen=="Kites")
nights<- data %>% filter(TrialType2hSS=="Night")


## bootstrapping the BPUE samples (10000 random draws)
control.samples <- matrix(sample(controls$BPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
control.statistics <- apply(control.samples, 1, mean)
BPUE_control<-data.frame(treatment="control",mean=mean(control.statistics),
                         lcl=quantile(control.statistics,0.025),ucl=quantile(control.statistics,0.975))

kite.samples <- matrix(sample(kites$BPUE, size = 10000 * nrow(kites), replace = TRUE),10000, nrow(kites))
kite.statistics <- apply(kite.samples, 1, mean)
BPUE_kite<-data.frame(treatment="kite",mean=mean(kite.statistics),
                         lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975))

night.samples <- matrix(sample(nights$BPUE, size = 10000 * nrow(nights), replace = TRUE),10000, nrow(nights))
night.statistics <- apply(night.samples, 1, mean)
BPUE_night<-data.frame(treatment="night",mean=mean(night.statistics),
                         lcl=quantile(night.statistics,0.025),ucl=quantile(night.statistics,0.975))


### calculating the BPUE differences

KITE_DIFF_BPUE<-data.frame(treatment="kite_control_diff",mean=mean(kite.statistics-control.statistics),
                      lcl=quantile(kite.statistics-control.statistics,0.025),ucl=quantile(kite.statistics-control.statistics,0.975))
NIGHT_DIFF_BPUE<-data.frame(treatment="night_control_diff",mean=mean(night.statistics-control.statistics),
                      lcl=quantile(night.statistics-control.statistics,0.025),ucl=quantile(night.statistics-control.statistics,0.975))



### quantifying differences in per cent
KITE_DIFF_BPUE[,5:7]<-(KITE_DIFF_BPUE[,2:4]/BPUE_control[,2:4])*100
NIGHT_DIFF_BPUE[,5:7]<-(NIGHT_DIFF_BPUE[,2:4]/BPUE_control[,2:4])*100




## bootstrapping the CPUE samples (10000 random draws)
control.samples <- matrix(sample(controls$CPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
control.statistics <- apply(control.samples, 1, mean)
CPUE_control<-data.frame(treatment="control",mean=mean(control.statistics),
                         lcl=quantile(control.statistics,0.025),ucl=quantile(control.statistics,0.975))

kite.samples <- matrix(sample(kites$CPUE, size = 10000 * nrow(kites), replace = TRUE),10000, nrow(kites))
kite.statistics <- apply(kite.samples, 1, mean)
CPUE_kite<-data.frame(treatment="kite",mean=mean(kite.statistics),
                      lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975))

night.samples <- matrix(sample(nights$CPUE, size = 10000 * nrow(nights), replace = TRUE),10000, nrow(nights))
night.statistics <- apply(night.samples, 1, mean)
CPUE_night<-data.frame(treatment="night",mean=mean(night.statistics),
                       lcl=quantile(night.statistics,0.025),ucl=quantile(night.statistics,0.975))


### calculating the BPUE differences

KITE_DIFF_CPUE<-data.frame(treatment="kite_control_diff",mean=mean(kite.statistics-control.statistics),
                      lcl=quantile(kite.statistics-control.statistics,0.025),ucl=quantile(kite.statistics-control.statistics,0.975))
NIGHT_DIFF_CPUE<-data.frame(treatment="night_control_diff",mean=mean(night.statistics-control.statistics),
                       lcl=quantile(night.statistics-control.statistics,0.025),ucl=quantile(night.statistics-control.statistics,0.975))



### quantifying differences in per cent
KITE_DIFF_CPUE[,5:7]<-(KITE_DIFF_CPUE[,2:4]/CPUE_control[,2:4])*100
NIGHT_DIFF_CPUE[,5:7]<-(NIGHT_DIFF_CPUE[,2:4]/CPUE_control[,2:4])*100



### COMBINE NUMBERS FOR OUTPUT FILE

DIFF_SUMMARY<-bind_rows(KITE_DIFF_BPUE,NIGHT_DIFF_BPUE,KITE_DIFF_CPUE,NIGHT_DIFF_CPUE) %>%
  mutate(mean=mean*12322.71,lcl=lcl*12322.71,ucl=ucl*12322.71) %>%
  rename(`prop.mean.diff(%)`=mean.1,`prop.lcl.diff(%)`=lcl.1,`prop.ucl.diff(%)`=ucl.1) %>%
  rename(`abs.mean.diff(N)`=mean,`abs.lcl.diff(N)`=lcl,`abs.ucl.diff(N)`=ucl) %>%
  mutate(Type=c(rep("Seabird bycatch",2),rep("Fish catch",2))) %>%
  select(Type,treatment,everything())

fwrite(DIFF_SUMMARY,"output/LIT_mitigation_difference_summary_2hSSnight.csv")



### PLOT predicted OUTPUT FOR FISH AND SEABIRDS ###

bind_rows(BPUE_control,BPUE_kite,BPUE_night,CPUE_control,CPUE_kite,CPUE_night) %>%
  mutate(Type=c(rep("BPUE",3),rep("CPUE",3))) %>%
  ggplot(aes(y=mean*12322.71, x=treatment, col=Type)) + geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl*12322.71, ymax=ucl*12322.71), width=.05)+
  facet_wrap(~Type, ncol=1,scales="free_y",strip.position = "left", 
             labeller = as_labeller(c(BPUE = "Seabird bycatch (N)", CPUE = "Fish catch (kg)") ) ) +
  #scale_y_continuous(limits=c(0,0.5), breaks=seq(0,0.5,0.1)) +
  xlab("") +
  ylab("Catch per average set effort") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.position="none",
        strip.background = element_blank(),
        strip.placement = "outside", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("output/LIT_bycatch_mitigation_summary_2hSSnight.jpg", width=8, height=11)



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    BASIC BOOTSTRAP ANALYSIS COMPARING BPUE AND CPUE FOR SEADUCKS ----------------------------
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########


### SWITCHED TO TrialType2hSS

# controls<- data %>% filter(Trial_type_by_fishermen=="Control") 
# kites<- data %>% filter(Trial_type_by_fishermen=="Kites")
# nights<- data %>% filter(Trial_type_by_fishermen=="Night")
controls<- data %>% filter(TrialType2hSS=="Control") %>% filter(Trial_type_by_fishermen!="Kites")
kites<- data %>% filter(TrialType2hSS=="Control") %>% filter(Trial_type_by_fishermen=="Kites")
nights<- data %>% filter(TrialType2hSS=="Night")


## bootstrapping the BPUE samples (10000 random draws)
control.samples <- matrix(sample(controls$SDBPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
control.statistics <- apply(control.samples, 1, mean)
BPUE_control<-data.frame(treatment="control",mean=mean(control.statistics),
                         lcl=quantile(control.statistics,0.025),ucl=quantile(control.statistics,0.975))

kite.samples <- matrix(sample(kites$SDBPUE, size = 10000 * nrow(kites), replace = TRUE),10000, nrow(kites))
kite.statistics <- apply(kite.samples, 1, mean)
BPUE_kite<-data.frame(treatment="kite",mean=mean(kite.statistics),
                      lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975))

night.samples <- matrix(sample(nights$SDBPUE, size = 10000 * nrow(nights), replace = TRUE),10000, nrow(nights))
night.statistics <- apply(night.samples, 1, mean)
BPUE_night<-data.frame(treatment="night",mean=mean(night.statistics),
                       lcl=quantile(night.statistics,0.025),ucl=quantile(night.statistics,0.975))


### calculating the BPUE differences

KITE_DIFF_BPUE<-data.frame(treatment="kite_control_diff",mean=mean(kite.statistics-control.statistics),
                           lcl=quantile(kite.statistics-control.statistics,0.025),ucl=quantile(kite.statistics-control.statistics,0.975))
NIGHT_DIFF_BPUE<-data.frame(treatment="night_control_diff",mean=mean(night.statistics-control.statistics),
                            lcl=quantile(night.statistics-control.statistics,0.025),ucl=quantile(night.statistics-control.statistics,0.975))



### quantifying differences in per cent
KITE_DIFF_BPUE[,5:7]<-(KITE_DIFF_BPUE[,2:4]/BPUE_control[,2:4])*100
NIGHT_DIFF_BPUE[,5:7]<-(NIGHT_DIFF_BPUE[,2:4]/BPUE_control[,2:4])*100




## bootstrapping the CPUE samples (10000 random draws)
control.samples <- matrix(sample(controls$CPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
control.statistics <- apply(control.samples, 1, mean)
CPUE_control<-data.frame(treatment="control",mean=mean(control.statistics),
                         lcl=quantile(control.statistics,0.025),ucl=quantile(control.statistics,0.975))

kite.samples <- matrix(sample(kites$CPUE, size = 10000 * nrow(kites), replace = TRUE),10000, nrow(kites))
kite.statistics <- apply(kite.samples, 1, mean)
CPUE_kite<-data.frame(treatment="kite",mean=mean(kite.statistics),
                      lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975))

night.samples <- matrix(sample(nights$CPUE, size = 10000 * nrow(nights), replace = TRUE),10000, nrow(nights))
night.statistics <- apply(night.samples, 1, mean)
CPUE_night<-data.frame(treatment="night",mean=mean(night.statistics),
                       lcl=quantile(night.statistics,0.025),ucl=quantile(night.statistics,0.975))


### calculating the BPUE differences

KITE_DIFF_CPUE<-data.frame(treatment="kite_control_diff",mean=mean(kite.statistics-control.statistics),
                           lcl=quantile(kite.statistics-control.statistics,0.025),ucl=quantile(kite.statistics-control.statistics,0.975))
NIGHT_DIFF_CPUE<-data.frame(treatment="night_control_diff",mean=mean(night.statistics-control.statistics),
                            lcl=quantile(night.statistics-control.statistics,0.025),ucl=quantile(night.statistics-control.statistics,0.975))



### quantifying differences in per cent
KITE_DIFF_CPUE[,5:7]<-(KITE_DIFF_CPUE[,2:4]/CPUE_control[,2:4])*100
NIGHT_DIFF_CPUE[,5:7]<-(NIGHT_DIFF_CPUE[,2:4]/CPUE_control[,2:4])*100



### COMBINE NUMBERS FOR OUTPUT FILE

DIFF_SUMMARY<-bind_rows(KITE_DIFF_BPUE,NIGHT_DIFF_BPUE,KITE_DIFF_CPUE,NIGHT_DIFF_CPUE) %>%
  mutate(mean=mean*12322.71,lcl=lcl*12322.71,ucl=ucl*12322.71) %>%
  rename(`prop.mean.diff(%)`=mean.1,`prop.lcl.diff(%)`=lcl.1,`prop.ucl.diff(%)`=ucl.1) %>%
  rename(`abs.mean.diff(N)`=mean,`abs.lcl.diff(N)`=lcl,`abs.ucl.diff(N)`=ucl) %>%
  mutate(Type=c(rep("seaduck bycatch",2),rep("Fish catch",2))) %>%
  select(Type,treatment,everything())

fwrite(DIFF_SUMMARY,"output/LIT_seaduck_mitigation_difference_summary_2hSS.csv")



### PLOT predicted OUTPUT FOR FISH AND SEABIRDS ###

bind_rows(BPUE_control,BPUE_kite,BPUE_night,CPUE_control,CPUE_kite,CPUE_night) %>%
  mutate(Type=c(rep("BPUE",3),rep("CPUE",3))) %>%
  ggplot(aes(y=mean*12322.71, x=treatment, col=Type)) + geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl*12322.71, ymax=ucl*12322.71), width=.05)+
  facet_wrap(~Type, ncol=1,scales="free_y",strip.position = "left", 
             labeller = as_labeller(c(BPUE = "seaduck bycatch (N)", CPUE = "Fish catch (kg)") ) ) +
  #scale_y_continuous(limits=c(0,0.5), breaks=seq(0,0.5,0.1)) +
  xlab("") +
  ylab("Catch per average set effort") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.position="none",
        strip.background = element_blank(),
        strip.placement = "outside", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("output/LIT_seaduck_bycatch_mitigation_summary_2hSS.jpg", width=8, height=11)


#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    BASIC BOOTSTRAP ANALYSIS COMPARING BPUE AND CPUE FOR LONG TAILED DUCKs ----------
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########


### SWITCHED TO TrialType2hSS

# controls<- data %>% filter(Trial_type_by_fishermen=="Control") 
# kites<- data %>% filter(Trial_type_by_fishermen=="Kites")
# nights<- data %>% filter(Trial_type_by_fishermen=="Night")
controls<- data %>% filter(TrialType2hSS=="Control") %>% filter(Trial_type_by_fishermen!="Kites")
kites<- data %>% filter(TrialType2hSS=="Control") %>% filter(Trial_type_by_fishermen=="Kites")
nights<- data %>% filter(TrialType2hSS=="Night")


## bootstrapping the BPUE samples (10000 random draws)
control.samples <- matrix(sample(controls$LTBPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
control.statistics <- apply(control.samples, 1, mean)
BPUE_control<-data.frame(treatment="control",mean=mean(control.statistics),
                         lcl=quantile(control.statistics,0.025),ucl=quantile(control.statistics,0.975))

kite.samples <- matrix(sample(kites$LTBPUE, size = 10000 * nrow(kites), replace = TRUE),10000, nrow(kites))
kite.statistics <- apply(kite.samples, 1, mean)
BPUE_kite<-data.frame(treatment="kite",mean=mean(kite.statistics),
                      lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975))

night.samples <- matrix(sample(nights$LTBPUE, size = 10000 * nrow(nights), replace = TRUE),10000, nrow(nights))
night.statistics <- apply(night.samples, 1, mean)
BPUE_night<-data.frame(treatment="night",mean=mean(night.statistics),
                       lcl=quantile(night.statistics,0.025),ucl=quantile(night.statistics,0.975))


### calculating the BPUE differences

KITE_DIFF_BPUE<-data.frame(treatment="kite_control_diff",mean=mean(kite.statistics-control.statistics),
                           lcl=quantile(kite.statistics-control.statistics,0.025),ucl=quantile(kite.statistics-control.statistics,0.975))
NIGHT_DIFF_BPUE<-data.frame(treatment="night_control_diff",mean=mean(night.statistics-control.statistics),
                            lcl=quantile(night.statistics-control.statistics,0.025),ucl=quantile(night.statistics-control.statistics,0.975))



### quantifying differences in per cent
KITE_DIFF_BPUE[,5:7]<-(KITE_DIFF_BPUE[,2:4]/BPUE_control[,2:4])*100
NIGHT_DIFF_BPUE[,5:7]<-(NIGHT_DIFF_BPUE[,2:4]/BPUE_control[,2:4])*100




## bootstrapping the CPUE samples (10000 random draws)
control.samples <- matrix(sample(controls$CPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
control.statistics <- apply(control.samples, 1, mean)
CPUE_control<-data.frame(treatment="control",mean=mean(control.statistics),
                         lcl=quantile(control.statistics,0.025),ucl=quantile(control.statistics,0.975))

kite.samples <- matrix(sample(kites$CPUE, size = 10000 * nrow(kites), replace = TRUE),10000, nrow(kites))
kite.statistics <- apply(kite.samples, 1, mean)
CPUE_kite<-data.frame(treatment="kite",mean=mean(kite.statistics),
                      lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975))

night.samples <- matrix(sample(nights$CPUE, size = 10000 * nrow(nights), replace = TRUE),10000, nrow(nights))
night.statistics <- apply(night.samples, 1, mean)
CPUE_night<-data.frame(treatment="night",mean=mean(night.statistics),
                       lcl=quantile(night.statistics,0.025),ucl=quantile(night.statistics,0.975))


### calculating the BPUE differences

KITE_DIFF_CPUE<-data.frame(treatment="kite_control_diff",mean=mean(kite.statistics-control.statistics),
                           lcl=quantile(kite.statistics-control.statistics,0.025),ucl=quantile(kite.statistics-control.statistics,0.975))
NIGHT_DIFF_CPUE<-data.frame(treatment="night_control_diff",mean=mean(night.statistics-control.statistics),
                            lcl=quantile(night.statistics-control.statistics,0.025),ucl=quantile(night.statistics-control.statistics,0.975))



### quantifying differences in per cent
KITE_DIFF_CPUE[,5:7]<-(KITE_DIFF_CPUE[,2:4]/CPUE_control[,2:4])*100
NIGHT_DIFF_CPUE[,5:7]<-(NIGHT_DIFF_CPUE[,2:4]/CPUE_control[,2:4])*100



### COMBINE NUMBERS FOR OUTPUT FILE

DIFF_SUMMARY<-bind_rows(KITE_DIFF_BPUE,NIGHT_DIFF_BPUE,KITE_DIFF_CPUE,NIGHT_DIFF_CPUE) %>%
  mutate(mean=mean*12322.71,lcl=lcl*12322.71,ucl=ucl*12322.71) %>%
  rename(`prop.mean.diff(%)`=mean.1,`prop.lcl.diff(%)`=lcl.1,`prop.ucl.diff(%)`=ucl.1) %>%
  rename(`abs.mean.diff(N)`=mean,`abs.lcl.diff(N)`=lcl,`abs.ucl.diff(N)`=ucl) %>%
  mutate(Type=c(rep("LTDU bycatch",2),rep("Fish catch",2))) %>%
  select(Type,treatment,everything())

fwrite(DIFF_SUMMARY,"output/LIT_LTDU_mitigation_difference_summary_2hSS.csv")



### PLOT predicted OUTPUT FOR FISH AND SEABIRDS ###

bind_rows(BPUE_control,BPUE_kite,BPUE_night,CPUE_control,CPUE_kite,CPUE_night) %>%
  mutate(Type=c(rep("BPUE",3),rep("CPUE",3))) %>%
  ggplot(aes(y=mean*12322.71, x=treatment, col=Type)) + geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl*12322.71, ymax=ucl*12322.71), width=.05)+
  facet_wrap(~Type, ncol=1,scales="free_y",strip.position = "left", 
             labeller = as_labeller(c(BPUE = "LTDU bycatch (N)", CPUE = "Fish catch (kg)") ) ) +
  #scale_y_continuous(limits=c(0,0.5), breaks=seq(0,0.5,0.1)) +
  xlab("") +
  ylab("Catch per average set effort") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.position="none",
        strip.background = element_blank(),
        strip.placement = "outside", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("output/LIT_LTDU_bycatch_mitigation_summary_2hSS.jpg", width=8, height=11)
    



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    CONDUCT ANALYSIS OF BYCATCH WITH RESPECT TO NIGHTTIME  ~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

#### MANAGEMENT GOAL IS TO DEFINE A TIME AROUND TWILIGHT OR SUNSET WHEN BYCATCH IS MINIMAL
head(data)
hist(data$daylightTripoverlap)

nightfishsummary<-data.frame()

cutoffs<-seq(-1.5,3,0.5)


for (t in cutoffs){
  
  controls<- data %>% filter(daylightTripoverlap>t) %>% filter(Trial_type_by_fishermen!="Kites")
  nights<- data %>% filter(daylightTripoverlap<t)
  
  
  ## bootstrapping the BPUE samples (10000 random draws)
  control.samples <- matrix(sample(controls$BPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
  control.statistics <- apply(control.samples, 1, mean)
  BPUE_control<-data.frame(treatment="control",mean=mean(control.statistics),
                           lcl=quantile(control.statistics,0.025),ucl=quantile(control.statistics,0.975),
                           n=dim(controls)[1])

  night.samples <- matrix(sample(nights$BPUE, size = 10000 * nrow(nights), replace = TRUE),10000, nrow(nights))
  night.statistics <- apply(night.samples, 1, mean)
  BPUE_night<-data.frame(treatment="night",mean=mean(night.statistics),
                         lcl=quantile(night.statistics,0.025),ucl=quantile(night.statistics,0.975),
                         n=dim(nights)[1])
  
  
  # ### calculating the BPUE differences
  # NIGHT_DIFF_BPUE<-data.frame(treatment="night_control_diff",mean=mean(night.statistics-control.statistics),
  #                             lcl=quantile(night.statistics-control.statistics,0.025),ucl=quantile(night.statistics-control.statistics,0.975))
  # 
  # ### quantifying differences in per cent
  # NIGHT_DIFF_BPUE[,5:7]<-(NIGHT_DIFF_BPUE[,2:4]/BPUE_control[,2:4])*100
  # 
  # ### COMBINE NUMBERS FOR OUTPUT FILE
  # 
  # nightfishsummary<-NIGHT_DIFF_BPUE %>%
  #   mutate(mean=mean*12322.71,lcl=lcl*12322.71,ucl=ucl*12322.71) %>%
  #   rename(`prop.mean.diff(%)`=mean.1,`prop.lcl.diff(%)`=lcl.1,`prop.ucl.diff(%)`=ucl.1) %>%
  #   rename(`abs.mean.diff(N)`=mean,`abs.lcl.diff(N)`=lcl,`abs.ucl.diff(N)`=ucl) %>%
  #   mutate(Night_definition=t) %>%
  #   bind_rows(nightfishsummary)
  
  nightfishsummary<-bind_rows(BPUE_night,BPUE_control) %>%
    mutate(mean=mean*12322.71,lcl=lcl*12322.71,ucl=ucl*12322.71) %>%
    mutate(Night_definition=t) %>%
    bind_rows(nightfishsummary)
  
}

fwrite(nightfishsummary,"output/LIT_night_BPUE_summary_by_hr.csv")


### PLOT predicted OUTPUT ###

nightfishsummary %>%
  # mutate(`prop.lcl.diff(%)`=ifelse(`prop.lcl.diff(%)`<(-100),-100,`prop.lcl.diff(%)`)) %>%
  # mutate(`prop.ucl.diff(%)`=ifelse(`prop.ucl.diff(%)`>100,100,`prop.ucl.diff(%)`)) %>%
  # 
  # ggplot(aes(y=`prop.mean.diff(%)`, x=Night_definition)) + geom_point(size=3)+
  # geom_errorbar(aes(ymin=`prop.lcl.diff(%)`, ymax=`prop.ucl.diff(%)`), width=.05)+
  mutate(Night_definition=ifelse(treatment=="control", Night_definition+0.075, Night_definition-0.075)) %>%
  
  ggplot(aes(y=mean, x=Night_definition, col=treatment, label=n)) +
  geom_point(size=3)+
  geom_text(hjust=-0.25, vjust=0) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl, col=treatment), width=.05)+

  #scale_y_continuous(limits=c(0,0.5), breaks=seq(0,0.5,0.1)) +
  xlab("Fishing restricted to hrs before sunset / after sunrise") +
  #ylab("Reduction in seabird bycatch compared to day trips (%)") +
  ylab("Mean seabird bycatch per unit effort") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.position="inside",
        legend.position.inside=c(0.1,0.9),
        legend.background=element_blank(),
        legend.key=element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("output/LIT_bycatch_by_night_definition.jpg", width=10, height=8)

