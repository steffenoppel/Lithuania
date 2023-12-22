### ##################################################
### LITHUANIA SEABIRD BYCATCH ANALYSIS - DATA PREPARATION
### written by steffen.oppel@vogelwarte.ch
### ##################################################

## takes data from .xlsx file and reads in various tables to connect them and calculate CPUE and BPUE
## creates simple map of fishing trials
## produces basic raw data summary


### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
filter<-dplyr::filter
select<-dplyr::select
library(marmap)




#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     DATA IMPORT FROM EXCEL FILE  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

try(setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\Lithuania"), silent=T)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\Lithuania"), silent=T)

# Read the data from sheets in Excel file - stripping Lithuanian header in second row
tripcols <- as.character(read_excel("data/trial analysis August 2023 clean version.xlsx", sheet="tbl_trip", n_max = 1, col_names = FALSE))
trips <- read_excel("data/trial analysis August 2023 clean version.xlsx", sheet="tbl_trip", skip=3, col_names = tripcols)
head(trips)
setcols <- as.character(read_excel("data/trial analysis August 2023 clean version.xlsx", sheet="tbl_set", n_max = 1, col_names = FALSE))
sets <- read_excel("data/trial analysis August 2023 clean version.xlsx", sheet="tbl_set", skip=3, col_names = setcols[1:31])
hour(sets$Depl_Date)<-ifelse(is.na(sets$Depl_Time),12,hour(sets$Depl_Time))
minute(sets$Depl_Date)<-ifelse(is.na(sets$Depl_Time),0,minute(sets$Depl_Time))
hour(sets$Depl_Date)<-ifelse(is.na(sets$Depl_Time),12,hour(sets$Depl_Time))
minute(sets$Depl_Date)<-ifelse(is.na(sets$Depl_Time),0,minute(sets$Depl_Time))
# month(sets$Depl_Time)<-month(sets$Depl_Date)
# day(sets$Depl_Time)<-day(sets$Depl_Date)
head(sets)
gearcols <- as.character(read_excel("data/trial analysis August 2023 clean version.xlsx", sheet="tbl_gear", n_max = 1, col_names = FALSE))
gear <- read_excel("data/trial analysis August 2023 clean version.xlsx", sheet="tbl_gear", skip=3, col_names = gearcols[1:9])
head(gear)

# Read the data from sheets in Excel file - stripping Lithuanian header in first row
fish <- read_excel("data/trial analysis August 2023 clean version.xlsx", sheet="tbl_fish", skip=0)
head(fish)
bycatch <- read_excel("data/trial analysis August 2023 clean version.xlsx", sheet="tbl_bycatch", skip=1)
head(bycatch)




#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     COMBINE TABLES TO CALCULATE FISHING EFFORT AND CPUE AND BPUE  ~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

### calculate fishing effort per set
effortsummary<-sets  %>%
  left_join(gear, by="Set_ID") %>%
  select(Trip_ID,Set_ID,Cont_Treat,Season, Month,Depl_Date,Valandos, Total_net_area) %>%
  mutate(Hours_deployed=ifelse(is.na(Valandos),12,Valandos)) %>%    ## make up number for sets with no soak time
  mutate(Hours_deployed=ifelse(Hours_deployed==0,12,Hours_deployed)) %>%   ## make up number for sets with 0 soak time
  #mutate(Hours_deployed=as.numeric(hour(Hours_deployed)+(minute(Hours_deployed)/60))) %>%  ## this does not work because it cannot deal with numbers >23:59
  mutate(Effort=Hours_deployed*Total_net_area)

### calculate fish caught per set
fishsummary<-fish  %>%
  rename(mass=`Weight, kg`, Cont_Treat=Net_modification) %>%
  group_by(Trip_ID,Cont_Treat,  Set_ID, Species) %>%
  summarise(catch=sum(mass, na.rm=T))
totalfishsummary<-fish  %>%
  rename(mass=`Weight, kg`, Cont_Treat=Net_modification) %>%
  group_by(Set_ID) %>%
  summarise(catch=sum(mass, na.rm=T))

### calculate bird bycatch per set
birdsummary<-bycatch  %>%
  mutate(n=1) %>%
  group_by(Set_ID, Species) %>%
  summarise(bycatch=sum(n, na.rm=T))
totalbirdsummary<-bycatch  %>%
  mutate(n=1) %>%
  group_by(Set_ID) %>%
  summarise(bycatch=sum(n, na.rm=T))


#### COMBINE SUMMARIES TO CALCULATE CPUE AND BPUE
alldata<-effortsummary %>%
  left_join(totalfishsummary, by="Set_ID") %>%
  mutate(catch=ifelse(is.na(catch),0,catch)) %>% ## fill in the 0 for sets with no fish catch
  left_join(totalbirdsummary, by="Set_ID") %>%
  mutate(bycatch=ifelse(is.na(bycatch),0,bycatch)) %>% ## fill in the 0 for sets with no bird bycatch
  mutate(CPUE=catch/Effort, BPUE=bycatch/Effort)
head(alldata)





#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    PROVIDE BASIC DATA SUMMARY FOR REPORT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

SUMMARY <- alldata %>% group_by(Season, Cont_Treat) %>%
  summarise(n_sets=length(unique(Set_ID)), Tot_effort=sum(Effort,na.rm=T), Tot_fish=sum(catch,na.rm=T), Tot_bycatch=sum(bycatch,na.rm=T), CPUE=mean(CPUE,na.rm=T), BPUE=mean(BPUE,na.rm=T))

SUMMARY

mean(alldata$Effort)




#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    PLOT A SUMMARY OF BYCATCH DATA ###   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

alldata %>%
  mutate(Cont_Treat=if_else(Cont_Treat=="Control2","Control",Cont_Treat)) %>%
  
  ggplot(aes(y=BPUE*12322.71, x=Cont_Treat)) +
  #geom_point(size=2, colour="firebrick",position=position_jitter(width = 0.15, height = 0))+
  geom_violin(colour="firebrick")+
  #scale_y_continuous(limits=c(-1.5,1.5), breaks=seq(-1.5,1.5,0.5)) +
  geom_hline(aes(yintercept=0), colour="darkgrey", linetype=2) +
  xlab("") +
  ylab("N seabirds caught per average set effort") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.key=element_blank(),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("output/LTU_raw_bycatch_data.jpg", width=11, height=9)
