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

### TODO:
# create tables 1 and 2 with real N
# create a histogram or table for monthly distribution of sets
# summarise diff in bycatch for seaducks, LTDU and cormorant

### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(adehabitatHS)
library(janitor)
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
#####     SUMMARIES FOR MANUSCRIPT -------------------------
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

### GENERATE TABLES 1 and 2 and a histogram for months
# Table 1. Trials performed during different seasons with different mitigation measures during winter seasons in the Baltic Sea Lithuanian coastal waters. 

Table1<- data %>%
  mutate(Type=ifelse(TrialType2hSS=="Control" & Trial_type_by_fishermen!="Kites","Control",
                     ifelse(TrialType2hSS=="Control" & Trial_type_by_fishermen=="Kites","Kite",
                            ifelse(TrialType2hSS=="Night" & Trial_type_by_fishermen=="Kites","Night+Kite",
                                   ifelse(TrialType2hSS=="Night" & Trial_type_by_fishermen!="Kites","Night","WTF"))))) %>%
  group_by(Season,Type) %>%
  summarise(N=length(unique(Set_ID))) %>%
  spread(key=Season, value=N) %>%
  janitor::adorn_totals(where = c("row","col"))
write.table(Table1, sep="\t", "clipboard", row.names=F)


FIGS1<- data %>%
  mutate(Type=ifelse(TrialType2hSS=="Control" & Trial_type_by_fishermen!="Kites","Control",
                     ifelse(TrialType2hSS=="Control" & Trial_type_by_fishermen=="Kites","Kite",
                            ifelse(TrialType2hSS=="Night" & Trial_type_by_fishermen=="Kites","Night+Kite",
                                   ifelse(TrialType2hSS=="Night" & Trial_type_by_fishermen!="Kites","Night","WTF"))))) %>%
  mutate(fakedate=if_else(Month<7,ymd("2020-01-01")+months(Month-1),
                          ymd("2019-01-01")+months(Month-1))) %>%
  group_by(fakedate,Type) %>%
  summarise(N=length(unique(Set_ID))) %>%
  left_join(Table1, by="Type") %>%
  mutate(prop=N/Total) %>%
  
  ggplot(aes(x=fakedate,y=prop, fill=Type, colour=Type)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y="Proportion of all net sets") +
  scale_x_date(name="Month",date_breaks = "1 month", date_labels =  "%B") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.position="inside",
        legend.position.inside=c(0.1,0.85),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("output/Figure_S1.jpg", width=11, height=9)






#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    BASIC BOOTSTRAP ANALYSIS COMPARING BPUE AND CPUE GLOBALLY -------------------------------
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

### because fish catch should be compared irrespective of time, we recalculate CPUE
data <- data %>%
  mutate(CPUE=catch/Total_net_area)  ## not taking soak time into account because that is subject to experimental manipulation

### SINGLE ANALYSIS FOR MANUALLY ANNOTATED TrialType2hSS
## added nightkite category on 14 Oct 2024

# controls<- data %>% filter(Trial_type_by_fishermen=="Control") 
# kites<- data %>% filter(Trial_type_by_fishermen=="Kites")
# nights<- data %>% filter(Trial_type_by_fishermen=="Night")
controls<- data %>% filter(TrialType2hSS=="Control") %>% filter(Trial_type_by_fishermen!="Kites")
kites<- data %>% filter(TrialType2hSS=="Control") %>% filter(Trial_type_by_fishermen=="Kites")
nights<- data %>% filter(TrialType2hSS=="Night") %>% filter(Trial_type_by_fishermen!="Kites")
nightkites<- data %>% filter(TrialType2hSS=="Night") %>% filter(Trial_type_by_fishermen=="Kites")  ### added to tease apart the effect of using both

dim(controls)
dim(kites)
dim(nights)
dim(nightkites)


### calculating effort differences between controls and nights
mean(controls$Hours_deployed)-mean(nights$Hours_deployed)/mean(c(controls$Hours_deployed,nights$Hours_deployed))
mean(nights$Hours_deployed)



## bootstrapping the BPUE samples (10000 random draws)
control.samples <- matrix(sample(controls$BPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
control.statistics <- apply(control.samples, 1, mean)
BPUE_control<-data.frame(treatment="control",n=nrow(controls),mean=mean(control.statistics),
                         lcl=quantile(control.statistics,0.025),ucl=quantile(control.statistics,0.975))

kite.samples <- matrix(sample(kites$BPUE, size = 10000 * nrow(kites), replace = TRUE),10000, nrow(kites))
kite.statistics <- apply(kite.samples, 1, mean)
BPUE_kite<-data.frame(treatment="kite",n=nrow(kites),mean=mean(kite.statistics),
                         lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975))

night.samples <- matrix(sample(nights$BPUE, size = 10000 * nrow(nights), replace = TRUE),10000, nrow(nights))
night.statistics <- apply(night.samples, 1, mean)
BPUE_night<-data.frame(treatment="night",n=nrow(nights),mean=mean(night.statistics),
                         lcl=quantile(night.statistics,0.025),ucl=quantile(night.statistics,0.975))

nightkite.samples <- matrix(sample(nightkites$BPUE, size = 10000 * nrow(nightkites), replace = TRUE),10000, nrow(nightkites))
nightkite.statistics <- apply(nightkite.samples, 1, mean)
BPUE_nightkite<-data.frame(treatment="night_kite",n=nrow(nightkites),mean=mean(nightkite.statistics),
                       lcl=quantile(nightkite.statistics,0.025),ucl=quantile(nightkite.statistics,0.975))


### calculating the BPUE differences

KITE_DIFF_BPUE<-data.frame(treatment="kite_control_diff",mean=mean(kite.statistics-control.statistics),
                      lcl=quantile(kite.statistics-control.statistics,0.025),ucl=quantile(kite.statistics-control.statistics,0.975))
NIGHT_DIFF_BPUE<-data.frame(treatment="night_control_diff",mean=mean(night.statistics-control.statistics),
                      lcl=quantile(night.statistics-control.statistics,0.025),ucl=quantile(night.statistics-control.statistics,0.975))
NIGHTKITE_DIFF_BPUE<-data.frame(treatment="nightkite_control_diff",mean=mean(nightkite.statistics-control.statistics),
                            lcl=quantile(nightkite.statistics-control.statistics,0.025),ucl=quantile(nightkite.statistics-control.statistics,0.975))


### quantifying differences in per cent
KITE_DIFF_BPUE[,5:7]<-(KITE_DIFF_BPUE[,2:4]/BPUE_control[,3:5])*100
NIGHT_DIFF_BPUE[,5:7]<-(NIGHT_DIFF_BPUE[,2:4]/BPUE_control[,3:5])*100
NIGHTKITE_DIFF_BPUE[,5:7]<-(NIGHTKITE_DIFF_BPUE[,2:4]/BPUE_control[,3:5])*100



## bootstrapping the CPUE samples (10000 random draws)
control.samples <- matrix(sample(controls$CPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
control.statistics <- apply(control.samples, 1, mean)
CPUE_control<-data.frame(treatment="control",n=nrow(controls),mean=mean(control.statistics),
                         lcl=quantile(control.statistics,0.025),ucl=quantile(control.statistics,0.975))

kite.samples <- matrix(sample(kites$CPUE, size = 10000 * nrow(kites), replace = TRUE),10000, nrow(kites))
kite.statistics <- apply(kite.samples, 1, mean)
CPUE_kite<-data.frame(treatment="kite",n=nrow(kites),mean=mean(kite.statistics),
                      lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975))

night.samples <- matrix(sample(nights$CPUE, size = 10000 * nrow(nights), replace = TRUE),10000, nrow(nights))
night.statistics <- apply(night.samples, 1, mean)
CPUE_night<-data.frame(treatment="night",n=nrow(nights),mean=mean(night.statistics),
                       lcl=quantile(night.statistics,0.025),ucl=quantile(night.statistics,0.975))

nightkite.samples <- matrix(sample(nightkites$CPUE, size = 10000 * nrow(nightkites), replace = TRUE),10000, nrow(nightkites))
nightkite.statistics <- apply(nightkite.samples, 1, mean)
CPUE_nightkite<-data.frame(treatment="night_kite",n=nrow(nightkites),mean=mean(nightkite.statistics),
                           lcl=quantile(nightkite.statistics,0.025),ucl=quantile(nightkite.statistics,0.975))


### calculating the CPUE differences

KITE_DIFF_CPUE<-data.frame(treatment="kite_control_diff",mean=mean(kite.statistics-control.statistics),
                      lcl=quantile(kite.statistics-control.statistics,0.025),ucl=quantile(kite.statistics-control.statistics,0.975))
NIGHT_DIFF_CPUE<-data.frame(treatment="night_control_diff",mean=mean(night.statistics-control.statistics),
                       lcl=quantile(night.statistics-control.statistics,0.025),ucl=quantile(night.statistics-control.statistics,0.975))
NIGHTKITE_DIFF_CPUE<-data.frame(treatment="nightkite_control_diff",mean=mean(nightkite.statistics-control.statistics),
                                lcl=quantile(nightkite.statistics-control.statistics,0.025),ucl=quantile(nightkite.statistics-control.statistics,0.975))



### quantifying differences in per cent
KITE_DIFF_CPUE[,5:7]<-(KITE_DIFF_CPUE[,2:4]/CPUE_control[,3:5])*100
NIGHT_DIFF_CPUE[,5:7]<-(NIGHT_DIFF_CPUE[,2:4]/CPUE_control[,3:5])*100
NIGHTKITE_DIFF_CPUE[,5:7]<-(NIGHTKITE_DIFF_CPUE[,2:4]/CPUE_control[,3:5])*100



### COMBINE NUMBERS FOR OUTPUT FILE

DIFF_SUMMARY<-bind_rows(KITE_DIFF_BPUE,NIGHT_DIFF_BPUE,NIGHTKITE_DIFF_BPUE, KITE_DIFF_CPUE,NIGHT_DIFF_CPUE,NIGHTKITE_DIFF_CPUE) %>%
  mutate(Type=c(rep("Seabird bycatch",3),rep("Fish catch",3))) %>%
  mutate(mean=ifelse(Type=="Seabird bycatch",mean*12322.71,mean*670),
         lcl=ifelse(Type=="Seabird bycatch",lcl*12322.71,lcl*670),
         ucl=ifelse(Type=="Seabird bycatch",ucl*12322.71,ucl*670)) %>%
  rename(`prop.mean.diff(%)`=mean.1,`prop.lcl.diff(%)`=lcl.1,`prop.ucl.diff(%)`=ucl.1) %>%
  rename(`abs.mean.diff(N)`=mean,`abs.lcl.diff(N)`=lcl,`abs.ucl.diff(N)`=ucl) %>%
  select(Type,treatment,everything())

#fwrite(DIFF_SUMMARY,"output/LIT_mitigation_difference_summary_2hSSnight.csv")

### MANUSCRIPT OUTPUT TABLE

TABLE3<-bind_rows(BPUE_control,BPUE_kite,BPUE_night,BPUE_nightkite,CPUE_control,CPUE_kite,CPUE_night,CPUE_nightkite) %>%
  mutate(Type=c(rep("Seabird bycatch",4),rep("Fish catch",4))) %>%
  mutate(mean=ifelse(Type=="Seabird bycatch",mean*12322.71,mean*670),
         lcl=ifelse(Type=="Seabird bycatch",lcl*12322.71,lcl*670),
         ucl=ifelse(Type=="Seabird bycatch",ucl*12322.71,ucl*670)) %>%
  mutate(catch=paste(round(mean,2)," (",round(lcl,2)," - ",round(ucl,2),")", sep="")) %>%
  select(Type,treatment,n,catch) %>%
  spread(key=Type,value=catch)
write.table(TABLE3, sep="\t", "clipboard")
kableExtra::kable(TABLE3)

# ### PLOT predicted OUTPUT FOR FISH AND SEABIRDS ###
# 
# bind_rows(BPUE_control,BPUE_kite,BPUE_night,BPUE_nightkite,CPUE_control,CPUE_kite,CPUE_night,CPUE_nightkite) %>%
#   mutate(Type=c(rep("BPUE",4),rep("CPUE",4))) %>%
#   mutate(mean=ifelse(Type=="BPUE",mean*12322.71,mean*670),
#          lcl=ifelse(Type=="BPUE",lcl*12322.71,lcl*670),
#          ucl=ifelse(Type=="BPUE",ucl*12322.71,ucl*670)) %>%
#   ggplot(aes(y=mean, x=treatment, col=Type)) + geom_point(size=3)+
#   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.05)+
#   facet_wrap(~Type, ncol=1,scales="free_y",strip.position = "left", 
#              labeller = as_labeller(c(BPUE = "Seabird bycatch (N)", CPUE = "Fish catch (kg)") ) ) +
#   #scale_y_continuous(limits=c(0,0.5), breaks=seq(0,0.5,0.1)) +
#   xlab("") +
#   ylab("Catch per average set effort") +
#   theme(panel.background=element_rect(fill="white", colour="black"), 
#         axis.text=element_text(size=16, color="black"), 
#         axis.title=element_text(size=18), 
#         strip.text=element_text(size=18, color="black"),
#         legend.position="none",
#         strip.background = element_blank(),
#         strip.placement = "outside", 
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.border = element_blank())


### PLOT predicted PERCENTAGE CHANGE FOR FISH AND SEABIRDS ###

DIFF_SUMMARY %>%
  mutate(`prop.lcl.diff(%)`=ifelse(`prop.lcl.diff(%)`<(-100),-100,`prop.lcl.diff(%)`)) %>%
  mutate(`prop.ucl.diff(%)`=ifelse(`prop.ucl.diff(%)`>100,100,`prop.ucl.diff(%)`)) %>%
  mutate(treatment=ifelse(treatment=="kite_control_diff","kite",
                          ifelse(treatment=="night_control_diff","night","night + kite"))) %>%
  ggplot(aes(y=`prop.mean.diff(%)`, x=treatment, col=Type)) +
  geom_point(size=3)+
  geom_errorbar(aes(ymin=`prop.lcl.diff(%)`, ymax=`prop.ucl.diff(%)`), width=.07, linewidth=1)+
  facet_wrap(~Type, ncol=1,scales="fixed",strip.position = "left") +
  geom_hline(aes(yintercept=0), linetype='dashed', col="grey", linewidth=1) +
  xlab("") +
  ylab("Difference compared to control nets (%)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        axis.text.y.right = element_blank(),
        strip.text=element_text(size=18, color="black"),
        title=element_text(size=16),
        legend.position="none",
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("output/FIGURE3.jpg", width=11, height=9)




# ### CREATE GRAPH FOR PRESENTATION IN GERMANY
# 
# bind_rows(BPUE_control,BPUE_kite,BPUE_night,BPUE_nightkite,CPUE_control,CPUE_kite,CPUE_night,CPUE_nightkite) %>%
#   mutate(Type=c(rep("BPUE",4),rep("CPUE",4))) %>%
#   mutate(mean=ifelse(Type=="BPUE",mean*12322.71,mean*670),
#          lcl=ifelse(Type=="BPUE",lcl*12322.71,lcl*670),
#          ucl=ifelse(Type=="BPUE",ucl*12322.71,ucl*670)) %>%
#   mutate(treatment=ifelse(treatment=="control", "Kontrolle",
#                           ifelse(treatment=="kite", "Drachen",
#                                  ifelse(treatment=="night", "Nacht","Nacht + Drachen")))) %>%
#   transform(treatment=factor(treatment,levels=c("Kontrolle","Drachen","Nacht","Nacht + Drachen"))) %>% 
#   
#   ggplot(aes(y=mean, x=treatment, col=Type)) + geom_point(size=3)+
#   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.05, linewidth=1)+
#   facet_wrap(~Type, ncol=1,scales="free_y",strip.position = "left", 
#              labeller = as_labeller(c(BPUE = "Seevögel (N)", CPUE = "Fisch (g)") ) ) +
#   
#   
#   #scale_y_continuous(limits=c(0,0.5), breaks=seq(0,0.5,0.1)) +
#   xlab("") +
#   ylab("Fang pro standard Aufwand") +
#   theme(panel.background=element_rect(fill="white", colour="black"), 
#         axis.text=element_text(size=16, color="black"), 
#         axis.title=element_text(size=18), 
#         strip.text=element_text(size=18, color="black"),
#         legend.position="none",
#         strip.background = element_blank(),
#         strip.placement = "outside", 
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.border = element_blank())
# 










#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    LOOP OVER COMBINATIONS OF SUNRISE AND SUNSET DIFFERENCE -------------------------------
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
### because fish catch should be compared irrespective of time, we recalculate CPUE
data <- data %>%
  mutate(CPUE=catch/Total_net_area)  ## not taking soak time into account because that is subject to experimental manipulation


### NEGATIVE DIFFERENCES INDICATE TOWARDS THE NIGHT SIDE, POSITIVE DIFFERENCES ARE TOWARDS DAY SIDE
### to select trips their SS/SR diffs must always be SMALLER than what is specified below

SRdiffs<-c(-1,0,1,2,3) ### hours after sunrise
SSdiffs<-c(0,1,2,3,4) ### hours before sunset

scenarios<-expand.grid(deplSSdiff=SSdiffs,haulSRdiff=SRdiffs) %>%
  mutate(Scenario=seq_along(deplSSdiff)) 


SCEN_DIFFS<-tibble()
SCEN_ABS<-tibble()

for(s in scenarios$Scenario) {
  
  nights<- data %>% filter(Trial_type_by_fishermen!="Kites") %>%
    filter(haulSRdiff<scenarios$haulSRdiff[scenarios$Scenario==s]) %>%
    filter(deplSSdiff<scenarios$deplSSdiff[scenarios$Scenario==s])
  nightkites<- data %>% filter(Trial_type_by_fishermen=="Kites") %>%
    filter(haulSRdiff<scenarios$haulSRdiff[scenarios$Scenario==s]) %>%
    filter(deplSSdiff<scenarios$deplSSdiff[scenarios$Scenario==s])  ### added to tease apart the effect of using both
  controls<- data %>% filter(Trial_type_by_fishermen!="Kites") %>%
    filter(!(Trip_ID %in% nights$Trip_ID))
  kites<- data %>% filter(Trial_type_by_fishermen=="Kites") %>%
    filter(!(Trip_ID %in% nightkites$Trip_ID))


## bootstrapping the BPUE samples (10000 random draws)
control.samples <- matrix(sample(controls$BPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
control.statistics <- apply(control.samples, 1, mean)
BPUE_control<-data.frame(treatment="control",mean=mean(control.statistics),
                         lcl=quantile(control.statistics,0.025),ucl=quantile(control.statistics,0.975),n=dim(controls)[1])

kite.samples <- matrix(sample(kites$BPUE, size = 10000 * nrow(kites), replace = TRUE),10000, nrow(kites))
kite.statistics <- apply(kite.samples, 1, mean)
BPUE_kite<-data.frame(treatment="kite",mean=mean(kite.statistics),
                      lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975),n=dim(kites)[1])

night.samples <- matrix(sample(nights$BPUE, size = 10000 * nrow(nights), replace = TRUE),10000, nrow(nights))
night.statistics <- apply(night.samples, 1, mean)
BPUE_night<-data.frame(treatment="night",mean=mean(night.statistics),
                       lcl=quantile(night.statistics,0.025),ucl=quantile(night.statistics,0.975),n=dim(nights)[1])

nightkite.samples <- matrix(sample(nightkites$BPUE, size = 10000 * nrow(nightkites), replace = TRUE),10000, nrow(nightkites))
nightkite.statistics <- apply(nightkite.samples, 1, mean)
BPUE_nightkite<-data.frame(treatment="night_kite",mean=mean(nightkite.statistics),
                           lcl=quantile(nightkite.statistics,0.025),ucl=quantile(nightkite.statistics,0.975),n=dim(nightkites)[1])


### calculating the BPUE differences

KITE_DIFF_BPUE<-data.frame(treatment="kite_control_diff",mean=mean(kite.statistics-control.statistics),
                           lcl=quantile(kite.statistics-control.statistics,0.025),ucl=quantile(kite.statistics-control.statistics,0.975))
NIGHT_DIFF_BPUE<-data.frame(treatment="night_control_diff",mean=mean(night.statistics-control.statistics),
                            lcl=quantile(night.statistics-control.statistics,0.025),ucl=quantile(night.statistics-control.statistics,0.975))
NIGHTKITE_DIFF_BPUE<-data.frame(treatment="nightkite_control_diff",mean=mean(nightkite.statistics-control.statistics),
                                lcl=quantile(nightkite.statistics-control.statistics,0.025),ucl=quantile(nightkite.statistics-control.statistics,0.975))


### quantifying differences in per cent
KITE_DIFF_BPUE[,5:7]<-(KITE_DIFF_BPUE[,2:4]/BPUE_control[,2:4])*100
NIGHT_DIFF_BPUE[,5:7]<-(NIGHT_DIFF_BPUE[,2:4]/BPUE_control[,2:4])*100
NIGHTKITE_DIFF_BPUE[,5:7]<-(NIGHTKITE_DIFF_BPUE[,2:4]/BPUE_control[,2:4])*100



## bootstrapping the CPUE samples (10000 random draws)
control.samples <- matrix(sample(controls$CPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
control.statistics <- apply(control.samples, 1, mean)
CPUE_control<-data.frame(treatment="control",mean=mean(control.statistics),
                         lcl=quantile(control.statistics,0.025),ucl=quantile(control.statistics,0.975),n=dim(controls)[1])

kite.samples <- matrix(sample(kites$CPUE, size = 10000 * nrow(kites), replace = TRUE),10000, nrow(kites))
kite.statistics <- apply(kite.samples, 1, mean)
CPUE_kite<-data.frame(treatment="kite",mean=mean(kite.statistics),
                      lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975), n=dim(kites)[1])

night.samples <- matrix(sample(nights$CPUE, size = 10000 * nrow(nights), replace = TRUE),10000)
night.statistics <- apply(night.samples, 1, mean)
CPUE_night<-data.frame(treatment="night",mean=mean(night.statistics),
                       lcl=quantile(night.statistics,0.025),ucl=quantile(night.statistics,0.975), n=dim(nights)[1])

nightkite.samples <- matrix(sample(nightkites$CPUE, size = 10000 * nrow(nightkites), replace = TRUE),10000)
nightkite.statistics <- apply(nightkite.samples, 1, mean)
CPUE_nightkite<-data.frame(treatment="night_kite",mean=mean(nightkite.statistics),
                           lcl=quantile(nightkite.statistics,0.025),ucl=quantile(nightkite.statistics,0.975), n=dim(nightkites)[1])


### calculating the CPUE differences

KITE_DIFF_CPUE<-data.frame(treatment="kite_control_diff",mean=mean(kite.statistics-control.statistics),
                           lcl=quantile(kite.statistics-control.statistics,0.025),ucl=quantile(kite.statistics-control.statistics,0.975))
NIGHT_DIFF_CPUE<-data.frame(treatment="night_control_diff",mean=mean(night.statistics-control.statistics),
                            lcl=quantile(night.statistics-control.statistics,0.025),ucl=quantile(night.statistics-control.statistics,0.975))
NIGHTKITE_DIFF_CPUE<-data.frame(treatment="nightkite_control_diff",mean=mean(nightkite.statistics-control.statistics),
                                lcl=quantile(nightkite.statistics-control.statistics,0.025),ucl=quantile(nightkite.statistics-control.statistics,0.975))



### quantifying differences in per cent
KITE_DIFF_CPUE[,5:7]<-(KITE_DIFF_CPUE[,2:4]/CPUE_control[,2:4])*100
NIGHT_DIFF_CPUE[,5:7]<-(NIGHT_DIFF_CPUE[,2:4]/CPUE_control[,2:4])*100
NIGHTKITE_DIFF_CPUE[,5:7]<-(NIGHTKITE_DIFF_CPUE[,2:4]/CPUE_control[,2:4])*100



### COMBINE ABSOLUTE NUMBERS FOR OUTPUT FILE
ABS_SUMMARY<-bind_rows(BPUE_control, BPUE_kite, BPUE_night,BPUE_nightkite,CPUE_control, CPUE_kite, CPUE_night,CPUE_nightkite) %>%
  mutate(n=rep(c(dim(controls)[1],dim(kites)[1],dim(nights)[1],dim(nightkites)[1]),2)) %>%
  mutate(Type=c(rep("Seabird bycatch",4),rep("Fish catch",4))) %>%
  mutate(mean=ifelse(Type=="Seabird bycatch",mean*12322.71,mean*670),
         lcl=ifelse(Type=="Seabird bycatch",lcl*12322.71,lcl*670),
         ucl=ifelse(Type=="Seabird bycatch",ucl*12322.71,ucl*670)) %>%
  select(Type,treatment,everything()) %>%
  mutate(Scenario=s)
SCEN_ABS<-bind_rows(SCEN_ABS,ABS_SUMMARY)


### COMBINE DIFFERENCE NUMBERS FOR OUTPUT FILE
DIFF_SUMMARY<-bind_rows(KITE_DIFF_BPUE,NIGHT_DIFF_BPUE,NIGHTKITE_DIFF_BPUE, KITE_DIFF_CPUE,NIGHT_DIFF_CPUE,NIGHTKITE_DIFF_CPUE) %>%
  mutate(n=rep(c(dim(kites)[1],dim(nights)[1],dim(nightkites)[1]),2)) %>%
  mutate(Type=c(rep("Seabird bycatch",3),rep("Fish catch",3))) %>%
  mutate(mean=ifelse(Type=="Seabird bycatch",mean*12322.71,mean*670),
         lcl=ifelse(Type=="Seabird bycatch",lcl*12322.71,lcl*670),
         ucl=ifelse(Type=="Seabird bycatch",ucl*12322.71,ucl*670)) %>%
  # rename(`prop.mean.diff(%)`=mean.1,`prop.lcl.diff(%)`=lcl.1,`prop.ucl.diff(%)`=ucl.1) %>%
  # rename(`abs.mean.diff(N)`=mean,`abs.lcl.diff(N)`=lcl,`abs.ucl.diff(N)`=ucl) %>%
  select(Type,treatment,everything()) %>%
  mutate(Scenario=s)
SCEN_DIFFS<-bind_rows(SCEN_DIFFS,DIFF_SUMMARY)


} ## end loop over all scenarios

SCEN_DIFFS<-SCEN_DIFFS %>% left_join(scenarios, by="Scenario") 
SCEN_ABS<-SCEN_ABS %>% left_join(scenarios, by="Scenario") 


### FOR MANUSCRIPT TEXT
SCEN_ABS %>%  filter(Type=="Seabird bycatch") %>%
  filter(treatment!="kite") %>% 
  filter(n>20) %>% arrange(mean)

SCEN_DIFFS %>%  filter(Type=="Fish catch") %>%
  filter(treatment!="kite") %>% 
  filter(haulSRdiff<1) %>% 
  filter(deplSSdiff<3) %>%
  filter(n>20) %>% 
  group_by(treatment) %>%
  summarise(red=mean(mean.1), red.lcl=mean(lcl.1),red.ucl=mean(ucl.1))

### SUMMARISE THE SEABIRD BYCATCH DIFFERENCES IN A PLOT ###

SCEN_DIFFS %>%
  filter(n>15) %>%
  mutate(deplSSdiff=deplSSdiff*-1) %>%
  #(deplSSdiff=factor(deplSSdiff, levels=c(-4,-3,-2,-1,0)))
  #mutate(mean.1=ifelse(mean.1<(-100),-100,mean.1)) %>%
  mutate(lcl.1=ifelse(lcl.1<-100,-100,lcl.1)) %>%
  mutate(ucl.1=ifelse(ucl.1>100,100,ucl.1)) %>%
  mutate(group=ifelse(ucl.1<0,"good","bad")) %>%
  mutate(treatment=ifelse(treatment=="kite_control_diff",'kite',ifelse(treatment=="night_control_diff",'night','night+kite'))) %>%
  filter(Type=="Seabird bycatch") %>%
  filter(treatment!="kite") %>% 
  ggplot(aes(y=mean.1, x=treatment, label=n, colour=group)) +
  geom_point(size=3)+
  geom_text(hjust=-1, vjust=0, size=3) + 
  geom_errorbar(aes(ymin=lcl.1, ymax=ucl.1), width=0.3, linewidth=1)+
  facet_grid(haulSRdiff~deplSSdiff, scales="free_y") +
  geom_hline(aes(yintercept=0), linetype='dashed', col="grey", linewidth=1) +
  scale_y_continuous(
    name = "% change in seabird bycatch",
    sec.axis = sec_axis(transform=~.*1, name="Net haul time in relation to sunrise (hrs)")
  ) +
  scale_colour_manual(values=c(good= "forestgreen", bad="firebrick")) +
  labs(title = "Net set time in relation to sunset (hrs)",
       x="Nocturnal experiment of gill nets",
       y="% change in seabird bycatch") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        axis.text.y.right = element_blank(),
        strip.text=element_text(size=18, color="black"),
        title=element_text(size=16),
        legend.position="none",
        strip.background = element_blank(),
        strip.placement = "outside",
        plot.margin = unit(c(2,3,2,2), "lines"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("output/Figure6.jpg", width=11, height=9)




# ### CREATE GERMAN PLOT FOR PRESENTATION
# 
# SCEN_DIFFS %>%
#   filter(n>15) %>%
#   filter(deplSSdiff>0) %>%
#   filter(deplSSdiff<4) %>%
#   filter(haulSRdiff>-1) %>%
#   filter(haulSRdiff<3) %>%
#   mutate(lcl.1=ifelse(lcl.1<-100,-100,lcl.1)) %>%
#   mutate(ucl.1=ifelse(ucl.1>200,200,ucl.1)) %>%
#   mutate(treatment=ifelse(treatment=="kite_control_diff",'Drachen',
#                           ifelse(treatment=="night_control_diff",'Nacht','Nacht + Drachen'))) %>%
#   filter(Type=="Seabird bycatch") %>%
#   filter(treatment!="Drachen") %>%
#   mutate(type=ifelse(haulSRdiff==0,"firebrick","grey15")) %>%
#   ggplot(aes(y=mean.1, x=treatment, label=n, col=type)) +
#   geom_point(size=3)+
#   geom_text(hjust=-1, vjust=0, size=3) + 
#   geom_errorbar(aes(ymin=lcl.1, ymax=ucl.1), width=.05)+
#   facet_grid(haulSRdiff~deplSSdiff, scales="free_y") +
#   geom_hline(aes(yintercept=0), linetype='dashed', col="forestgreen", linewidth=0.5) +
#   labs(title = "Stunden vor Sonnenuntergang",
#        x="Technische Massnahmen",
#        y="Differenz im Seevogel Beifang (%)") +
#   theme(panel.background=element_rect(fill="white", colour="black"), 
#         axis.text=element_text(size=14, color="black"), 
#         axis.title=element_text(size=18), 
#         strip.text=element_text(size=18, color="black"),
#         title=element_text(size=18),
#         legend.position="none",
#         strip.background = element_blank(),
#         strip.placement = "outside", 
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.border = element_blank())
# 
# ggsave("output/LIT_relative_bycatch_mitigation_25scenarios.jpg", width=11, height=9)



### SUMMARISE THE FISH CATCH DIFFERENCES IN A PLOT ###

SCEN_DIFFS %>%
  filter(n>10) %>%
  mutate(deplSSdiff=deplSSdiff*-1) %>%
  mutate(lcl.1=ifelse(lcl.1<-100,-100,lcl.1)) %>%
  mutate(ucl.1=ifelse(ucl.1>100,100,ucl.1)) %>%
  mutate(group=ifelse(mean.1>-15,"good","bad")) %>%
  mutate(treatment=ifelse(treatment=="kite_control_diff",'kite',ifelse(treatment=="night_control_diff",'no kite','with kite'))) %>%
  filter(Type=="Fish catch") %>%
  filter(treatment!="kite") %>%
  ggplot(aes(y=mean.1, x=treatment, label=n, colour=group)) +
  geom_point(size=3)+
  geom_text(hjust=-1, vjust=0, size=3) + 
  geom_errorbar(aes(ymin=lcl.1, ymax=ucl.1), width=0.3, linewidth=1)+
  facet_grid(haulSRdiff~deplSSdiff, scales="free_y") +
  geom_hline(aes(yintercept=0), linetype='dashed', col="grey", linewidth=1) +
  scale_y_continuous(
    name = "% change in fish catch",
    sec.axis = sec_axis(transform=~.*1, name="Net haul time in relation to sunrise (hrs)")
  ) +
  scale_colour_manual(values=c(good= "forestgreen", bad="orange")) +
  labs(title = "Net set time in relation to sunset (hrs)",
       x="Nocturnal experiment of gill nets",
       y="% change in seabird bycatch") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        axis.text.y.right = element_blank(),
        strip.text=element_text(size=18, color="black"),
        title=element_text(size=16),
        legend.position="none",
        strip.background = element_blank(),
        strip.placement = "outside",
        plot.margin = unit(c(2,3,2,2), "lines"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("output/Figure7.jpg", width=11, height=9)




# ### SUMMARISE THE ABSOLUTE BYCATCH RATES IN A PLOT ###
# 
# SCEN_ABS %>%
#   filter(n>15) %>%
#   mutate(deplSSdiff=deplSSdiff*-1) %>%
#   mutate(group=ifelse(lcl<0,"good","bad")) %>%
#   filter(Type=="Seabird bycatch") %>%
#   ggplot(aes(y=mean, x=treatment, label=n, colour=group)) +
#   geom_point(size=3)+
#   geom_text(hjust=-0.5, vjust=0, size=3) + 
#   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.3, linewidth=1)+
#   facet_grid(haulSRdiff~deplSSdiff, scales="fixed") +
#   geom_hline(aes(yintercept=0), linetype='dashed', col="grey", linewidth=1) +
#   scale_y_continuous(
#     name = "seabird bycatch rate (per average unit effort)",
#     sec.axis = sec_axis(transform=~.*1, name="Net haul time in relation to sunrise (hrs)")
#   ) +
#   scale_colour_manual(values=c(good= "forestgreen", bad="firebrick")) +
#   theme(panel.background=element_rect(fill="white", colour="black"), 
#         axis.text=element_text(size=14, color="black"), 
#         axis.title=element_text(size=18),
#         axis.text.y.right = element_blank(),
#         strip.text=element_text(size=18, color="black"),
#         title=element_text(size=16),
#         legend.position="none",
#         strip.background = element_blank(),
#         strip.placement = "outside",
#         plot.margin = unit(c(2,3,2,2), "lines"),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.border = element_blank())
# 
# ggsave("output/LIT_bycatch_means_25scenarios.jpg", width=11, height=9)



# ### SUMMARISE THE ABSOLUTE FISH CATCH RATES IN A PLOT ###
# 
# SCEN_ABS %>%
#   filter(n>15) %>%
#   filter(Type=="Fish catch") %>%
#   ggplot(aes(y=mean, x=treatment, label=n)) +
#   geom_point(size=3)+
#   geom_text(hjust=-0.5, vjust=0, size=3) + 
#   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.05)+
#   facet_grid(haulSRdiff~deplSSdiff, scales="fixed") +
#   labs(title = "Nets set hours before sunset",
#        x="Experimental treatment",
#        y="Fish catch (per average net size)") +
#   theme(panel.background=element_rect(fill="white", colour="black"), 
#         axis.text=element_text(size=14, color="black"), 
#         axis.title=element_text(size=18),
#         axis.text.y.right = element_blank(),
#         strip.text=element_text(size=18, color="black"),
#         title=element_text(size=16),
#         legend.position="none",
#         strip.background = element_blank(),
#         strip.placement = "outside",
#         plot.margin = unit(c(2,3,2,2), "lines"),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.border = element_blank())
# 
# ggsave("output/LIT_fishcatch_means_25scenarios.jpg", width=11, height=9)



SCEN_DIFFS<-SCEN_DIFFS %>% 
  rename(`prop.mean.diff(%)`=mean.1,`prop.lcl.diff(%)`=lcl.1,`prop.ucl.diff(%)`=ucl.1) %>%
  rename(`abs.mean.diff(N)`=mean,`abs.lcl.diff(N)`=lcl,`abs.ucl.diff(N)`=ucl)
fwrite(SCEN_DIFFS,"output/LIT_mitigation_difference_summary_25scenarios.csv")
fwrite(SCEN_ABS,"output/LIT_bycatch_rates_summary_25scenarios.csv")








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
