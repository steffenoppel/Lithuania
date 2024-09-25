### ##################################################
### LITHUANIA SEABIRD BYCATCH ANALYSIS - DATA PREPARATION
### written by steffen.oppel@vogelwarte.ch
### ##################################################

## takes data from .xlsx file and reads in various tables to connect them and calculate CPUE and BPUE
## creates simple map of fishing trials
## produces basic raw data summary

## updated on 12 Jan 2024 after receiving new database

## amended on 9 Feb 2024 to include depth and species info (on request of Yann Rouxel)

## updated on 30 August 2024 after receiving final data from Rasa Morkune

## updated on 25 Sept 2025 to include columns of nighttime (with cutoffs of sunset, civil, and nautical twilight)


### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
library(suncalc) ## to estimate daytime
filter<-dplyr::filter
select<-dplyr::select
#library(marmap)
library(janitor)



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     DATA IMPORT FROM EXCEL FILE  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

try(setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\Lithuania"), silent=T)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\Lithuania"), silent=T)

# Read the data from sheets in Excel file - stripping Lithuanian header in second row
tripcols <- as.character(read_excel("data/2019-2024 all trials final table.xlsx", sheet="tbl_trip", n_max = 1, col_names = FALSE))
trips <- read_excel("data/2019-2024 all trials final table.xlsx", sheet="tbl_trip", skip=3, col_names = tripcols)
head(trips)
setcols <- as.character(read_excel("data/2019-2024 all trials final table.xlsx", sheet="tbl_set", n_max = 1, col_names = FALSE))
setcols[c(4:5)]<-c("TrialType3hSS","TrialType2hSS")
sets <- read_excel("data/2019-2024 all trials final table.xlsx", sheet="tbl_set", skip=3, col_names = setcols[1:33])

## combine date and time into a single field
hour(sets$Depl_Date)<-ifelse(is.na(sets$Depl_Time),12,hour(sets$Depl_Time))
minute(sets$Depl_Date)<-ifelse(is.na(sets$Depl_Time),0,minute(sets$Depl_Time))
hour(sets$Haul_Date)<-ifelse(is.na(sets$Haul_time),12,hour(sets$Haul_time))
minute(sets$Haul_Date)<-ifelse(is.na(sets$Haul_time),0,minute(sets$Haul_time))
head(sets)
gearcols <- as.character(read_excel("data/2019-2024 all trials final table.xlsx", sheet="tbl_gear", n_max = 1, col_names = FALSE))
gear <- read_excel("data/2019-2024 all trials final table.xlsx", sheet="tbl_gear", skip=3, col_names = gearcols[1:9])
head(gear)

## calculate sunset and sunrise times for setting and hauling and calculate difference between sunset and deployment (and sunrise and haul)
sets$sunset<-suncalc::getSunlightTimes(date=as.Date(sets$Depl_Date),lat=55.737232,lon=20.988563,keep="sunset")$sunset
sets$sunrise<-suncalc::getSunlightTimes(date=as.Date(sets$Haul_Date),lat=55.737232,lon=20.988563,keep="sunrise")$sunrise
sets$nauticalDusk<-suncalc::getSunlightTimes(date=as.Date(sets$Depl_Date),lat=55.737232,lon=20.988563,keep="nauticalDusk")$nauticalDusk
sets$nauticalDawn<-suncalc::getSunlightTimes(date=as.Date(sets$Haul_Date),lat=55.737232,lon=20.988563,keep="nauticalDawn")$nauticalDawn

sets <- sets %>%
  mutate(deplSSdiff=as.numeric(difftime(sunset, Depl_Date, unit="hours")), haulSRdiff=as.numeric(difftime(Haul_Date, sunrise, unit="hours")),
         deplNDdiff=as.numeric(difftime(nauticalDusk, Depl_Date, unit="hours")), haulNDdiff=as.numeric(difftime(Haul_Date, nauticalDawn, unit="hours"))) %>%
  rowwise() %>%
  mutate(daylightTripoverlap=max(deplSSdiff,haulSRdiff, na.rm=T), twilightTripoverlap=max(deplNDdiff,haulNDdiff, na.rm=T)) %>%
  ungroup() %>%
  mutate(SStrip=ifelse(daylightTripoverlap<0,"Night",ifelse(Trial_type_by_fishermen=="Night","Control",Trial_type_by_fishermen))) %>%  ## this could be adjusted to use 1 or 2 hr tolerance to allow fishing within 1-2 hrs of sunset
  mutate(TLtrip=ifelse(twilightTripoverlap<0,"Night",ifelse(Trial_type_by_fishermen=="Night","Control",Trial_type_by_fishermen)))   ## this could be adjusted to use 1 or 2 hr tolerance to allow fishing within 1-2 hrs of twilight
## inspect erroneous assignments
sets %>%
  dplyr::select(Trial_type_by_fishermen,TrialType3hSS,daylightTripoverlap,SStrip,deplSSdiff,haulSRdiff,Depl_Date,sunset,Haul_Date,sunrise) %>%
  dplyr::filter(SStrip=="Night" & TrialType3hSS!="Night") 

# Read the data from sheets in Excel file - stripping Lithuanian header in first row
fishcols <- as.character(read_excel("data/2019-2024 all trials final table.xlsx", sheet="tbl_fish", n_max = 1, col_names = FALSE))
fish <- read_excel("data/2019-2024 all trials final table.xlsx", sheet="tbl_fish", skip=3, col_names = c(fishcols,"ignore"))
head(fish)
bycatchcols <- as.character(read_excel("data/2019-2024 all trials final table.xlsx", sheet="tbl_bycatch", n_max = 1, col_names = FALSE))
bycatch <- read_excel("data/2019-2024 all trials final table.xlsx", sheet="tbl_bycatch", skip=2, col_names = bycatchcols)
head(bycatch)




#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     COMBINE TABLES TO CALCULATE FISHING EFFORT AND CPUE AND BPUE  ~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

### calculate fishing effort per set
effortsummary<-sets  %>%
  left_join(gear, by="Set_ID") %>%
  select(Trip_ID,Set_ID,Trial_type_by_fishermen,Season, Month,Depl_Date,Haul_Date, Total_net_area,Fishing_depth,SStrip,TLtrip,TrialType3hSS,TrialType2hSS,daylightTripoverlap,twilightTripoverlap) %>%
  mutate(Hours_deployed=as.numeric(difftime(Haul_Date,Depl_Date, units="hours"))) %>%  ## this does not work because it cannot deal with numbers >23:59
  mutate(Hours_deployed=ifelse(is.na(Hours_deployed),12,Hours_deployed)) %>%    ## make up number for sets with no soak time
  mutate(Hours_deployed=ifelse(Hours_deployed==0,12,Hours_deployed)) %>%   ## make up number for sets with 0 soak time
  mutate(Effort=Hours_deployed*Total_net_area)

### calculate fish caught per set
fishsummary<-fish  %>%
  rename(mass=`Weight, kg`, Species=`Fish species`) %>%
  group_by(Trip_ID,Trial_type_by_fishermen,  Set_ID, Species) %>%
  summarise(catch=sum(mass, na.rm=T))
totalfishsummary<-fish  %>%
  rename(mass=`Weight, kg`) %>%
  group_by(Set_ID) %>%
  summarise(catch=sum(mass, na.rm=T))

### calculate bird bycatch per set
birdsummary<-bycatch  %>%
  rename(Species=`Bird species`) %>%
  mutate(n=1) %>%
  group_by(Set_ID, Species) %>%
  summarise(bycatch=sum(n, na.rm=T))
totalbirdsummary<-bycatch  %>%
  mutate(n=1) %>%
  group_by(Set_ID) %>%
  summarise(bycatch=sum(n, na.rm=T))

birdsummary %>% group_by(Species) %>%
  summarise(Total=sum(bycatch)) %>%
  arrange(desc(Total)) %>%
  mutate(Prop=adorn_percentages(.,denominator = "col", na.rm = TRUE)[,2])

LTDUsummary<-bycatch  %>%
  rename(Species=`Bird species`) %>%
  filter(Species=="Clangula hyemalis") %>%
  mutate(n=1) %>%
  group_by(Set_ID) %>%
  summarise(LTDUbycatch=sum(n, na.rm=T))

seaducksummary<-bycatch  %>%
  rename(Species=`Bird species`) %>%
  filter(Species %in% c("Clangula hyemalis","Melanitta fusca","Melanitta nigra")) %>%
  mutate(n=1) %>%
  group_by(Set_ID) %>%
  summarise(seaduckbycatch=sum(n, na.rm=T))



#### COMBINE SUMMARIES TO CALCULATE CPUE AND BPUE
alldata<-effortsummary %>%
  left_join(totalfishsummary, by="Set_ID") %>%
  mutate(catch=ifelse(is.na(catch),0,catch)) %>% ## fill in the 0 for sets with no fish catch
  left_join(totalbirdsummary, by="Set_ID") %>%
  mutate(bycatch=ifelse(is.na(bycatch),0,bycatch)) %>% ## fill in the 0 for sets with no bird bycatch
  left_join(LTDUsummary, by="Set_ID") %>%
  mutate(LTDUbycatch=ifelse(is.na(LTDUbycatch),0,LTDUbycatch)) %>% ## fill in the 0 for sets with no bird bycatch
  left_join(seaducksummary, by="Set_ID") %>%
  mutate(seaduckbycatch=ifelse(is.na(seaduckbycatch),0,seaduckbycatch)) %>% ## fill in the 0 for sets with no bird bycatch
  mutate(CPUE=catch/Effort, BPUE=bycatch/Effort, LTBPUE=LTDUbycatch/Effort, SDBPUE=seaduckbycatch/Effort)
head(alldata)

#### SAVE DATA FOR ANALYSIS
analysisdata<-alldata %>%
  select(Trip_ID,Set_ID,Trial_type_by_fishermen,Season,Month,Total_net_area, Hours_deployed, Fishing_depth,Effort, catch,bycatch, CPUE, BPUE,LTBPUE,SDBPUE,,SStrip,TLtrip,TrialType3hSS,TrialType2hSS,daylightTripoverlap,twilightTripoverlap) %>%
  left_join(sets[,c(2,4,30)], by="Set_ID")
saveRDS(analysisdata,"data/LIT_bycatch_data_formatted.rds")


#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    PROVIDE BASIC DATA SUMMARY FOR REPORT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

SUMMARY1 <- alldata %>%
  mutate(Trial_type_by_fishermen=ifelse(Trial_type_by_fishermen=="Control2","Control",Trial_type_by_fishermen)) %>%
  group_by(Season, Trial_type_by_fishermen) %>%
  summarise(n_sets=length(unique(Set_ID)), Tot_effort=sum(Effort,na.rm=T), Tot_fish=sum(catch,na.rm=T), Tot_bycatch=sum(bycatch,na.rm=T), CPUE=mean(CPUE,na.rm=T), BPUE=mean(BPUE,na.rm=T)) %>%
  mutate(NightDefinition="by fisherman")

SUMMARY2 <- alldata %>%
  mutate(Trial_type_by_fishermen=if_else(TrialType3hSS=="Control2","Control",TrialType3hSS)) %>%
  group_by(Season, Trial_type_by_fishermen) %>%
  summarise(n_sets=length(unique(Set_ID)), Tot_effort=sum(Effort,na.rm=T), Tot_fish=sum(catch,na.rm=T), Tot_bycatch=sum(bycatch,na.rm=T), CPUE=mean(CPUE,na.rm=T), BPUE=mean(BPUE,na.rm=T)) %>%
  mutate(NightDefinition="within 3 hrs of sunset")

SUMMARY3 <- alldata %>%
  mutate(Trial_type_by_fishermen=if_else(TrialType2hSS=="Control2","Control",TrialType2hSS)) %>%
  group_by(Season, Trial_type_by_fishermen) %>%
  summarise(n_sets=length(unique(Set_ID)), Tot_effort=sum(Effort,na.rm=T), Tot_fish=sum(catch,na.rm=T), Tot_bycatch=sum(bycatch,na.rm=T), CPUE=mean(CPUE,na.rm=T), BPUE=mean(BPUE,na.rm=T)) %>%
  mutate(NightDefinition="within 2 hrs of sunset")

SUMMARY4 <- alldata %>%
  mutate(Trial_type_by_fishermen=if_else(SStrip=="Control2","Control",SStrip)) %>%
  group_by(Season, Trial_type_by_fishermen) %>%
  summarise(n_sets=length(unique(Set_ID)), Tot_effort=sum(Effort,na.rm=T), Tot_fish=sum(catch,na.rm=T), Tot_bycatch=sum(bycatch,na.rm=T), CPUE=mean(CPUE,na.rm=T), BPUE=mean(BPUE,na.rm=T)) %>%
  mutate(NightDefinition="sunset to sunrise")

SUMMARY5 <- alldata %>%
  mutate(Trial_type_by_fishermen=if_else(TLtrip=="Control2","Control",TLtrip)) %>%
  group_by(Season, Trial_type_by_fishermen) %>%
  summarise(n_sets=length(unique(Set_ID)), Tot_effort=sum(Effort,na.rm=T), Tot_fish=sum(catch,na.rm=T), Tot_bycatch=sum(bycatch,na.rm=T), CPUE=mean(CPUE,na.rm=T), BPUE=mean(BPUE,na.rm=T)) %>%
  mutate(NightDefinition="nautical dusk to dawn")

SUMMARY<-bind_rows(SUMMARY1,SUMMARY2,SUMMARY3,SUMMARY4,SUMMARY5)

fwrite(SUMMARY,"output/Raw_data_summary_by_night_type.csv")




#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    PLOT A SUMMARY OF BYCATCH DATA ###   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

## for fisherman NIGHT definition
alldata %>%
  mutate(Trial_type_by_fishermen=if_else(Trial_type_by_fishermen=="Control2","Control",Trial_type_by_fishermen)) %>%
  
  ggplot(aes(y=BPUE*12322.71, x=Trial_type_by_fishermen)) +
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



## for NIGHT definition with 3 hrs around sunset

alldata %>%
  mutate(Trial_type_by_fishermen=if_else(TrialType3hSS=="Control2","Control",TrialType3hSS)) %>%
  
  ggplot(aes(y=BPUE*12322.71, x=Trial_type_by_fishermen)) +
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

ggsave("output/LTU_raw_bycatch_data_3hrSS.jpg", width=11, height=9)



## for NIGHT definition with 2 hrs around sunset

alldata %>%
  mutate(Trial_type_by_fishermen=if_else(TrialType2hSS=="Control2","Control",TrialType2hSS)) %>%
  
  ggplot(aes(y=BPUE*12322.71, x=Trial_type_by_fishermen)) +
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

ggsave("output/LTU_raw_bycatch_data_2hrSS.jpg", width=11, height=9)



## for NIGHT definition with sunset

alldata %>%
  mutate(Trial_type_by_fishermen=if_else(SStrip=="Control2","Control",SStrip)) %>%
  
  ggplot(aes(y=BPUE*12322.71, x=Trial_type_by_fishermen)) +
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

ggsave("output/LTU_raw_bycatch_data_SS.jpg", width=11, height=9)




## for NIGHT definition with nautical twilight

alldata %>%
  mutate(Trial_type_by_fishermen=if_else(TLtrip=="Control2","Control",TLtrip)) %>%
  
  ggplot(aes(y=BPUE*12322.71, x=Trial_type_by_fishermen)) +
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

ggsave("output/LTU_raw_bycatch_data_TL.jpg", width=11, height=9)





#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    SUMMARISE BYCATCH AT DEPTH DATA  ###   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
hist(sets$Fishing_depth)

deptheffort<-analysisdata %>%
  group_by(Fishing_depth) %>%
  summarise(effort=sum(Effort, na.rm=T)) %>%
  arrange(Fishing_depth)

depthbycatch<-bycatch  %>%
  mutate(n=1) %>%
  group_by(Depth) %>%
  summarise(bycatch=sum(n, na.rm=T)) %>%
  rename(Fishing_depth=Depth) %>%
  arrange(Fishing_depth)


### merge and show proportions in plot

adorn_percentages(deptheffort,denominator = "col", na.rm = TRUE) %>%
  left_join(adorn_percentages(depthbycatch,denominator = "col", na.rm = TRUE), by="Fishing_depth") %>%
  mutate(bycatch=ifelse(is.na(bycatch),0,bycatch)) %>%
  gather(key="Type",value="Proportion",-Fishing_depth) %>%
  
  ggplot(aes(y=Proportion*100, x=Fishing_depth, col=Type, fill=Type)) +
  geom_bar(position="dodge", stat="identity")+
  xlab("Depth of fishing (m)") +
  ylab("Proportion (%)") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.key=element_blank(),
        legend.position=c(0.8,0.8), 
        panel.border = element_blank())

ggsave("output/LTU_depth_bycatch_effort.jpg", width=11, height=9)

## prepare data for analysis
depthbycatch<-bycatch  %>%
  rename(Species=`Bird species`) %>%
  mutate(n=1) %>%
  group_by(Depth,Species) %>%
  summarise(bycatch=sum(n, na.rm=T)) %>%
  rename(Fishing_depth=Depth) %>%
  arrange(Fishing_depth) %>%
  spread(key=Species, value=bycatch, fill=0)

depthprops<-adorn_percentages(deptheffort,denominator = "col", na.rm = TRUE) %>%
  left_join(adorn_percentages(depthbycatch,denominator = "col", na.rm = TRUE), by="Fishing_depth") %>%
  gather(key="Type",value="Proportion",-Fishing_depth) %>%
  mutate(Proportion=ifelse(is.na(Proportion),0,Proportion*100)) %>%
  spread(key=Fishing_depth, value=Proportion)
saveRDS(depthprops,"data/LIT_bycatch_depths.rds")


#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    SUMMARISE FISHING EFFORT AT DEPTH BY MITIGATION  ###   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

trialdeptheffort<-analysisdata %>%
  mutate(Trial_type_by_fishermen=if_else(Trial_type_by_fishermen=="Control2","Control",Trial_type_by_fishermen)) %>%
  group_by(Fishing_depth,Trial_type_by_fishermen) %>%
  summarise(effort=sum(Effort, na.rm=T)) %>%
  spread(key=Trial_type_by_fishermen,value=effort, fill=0) %>%
  arrange(Fishing_depth)

### merge and show proportions in plot

adorn_percentages(trialdeptheffort,denominator = "col", na.rm = TRUE) %>%
  gather(key="Trial",value="PropEffort",-Fishing_depth) %>%
  
  ggplot(aes(y=PropEffort*100, x=Fishing_depth, col=Trial, fill=Trial)) +
  geom_bar(position="dodge", stat="identity")+
  xlab("Depth of fishing (m)") +
  ylab("Proportion (%)") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.key=element_blank(),
        legend.position=c(0.8,0.8), 
        panel.border = element_blank())

ggsave("output/LTU_depth_by_trial_effort.jpg", width=11, height=9)

