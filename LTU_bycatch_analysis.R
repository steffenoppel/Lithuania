### ##################################################
### LITHUANIA SEABIRD BYCATCH ANALYSIS - EVALUATION OF KITES AND NIGHT FISHING
### written by steffen.oppel@vogelwarte.ch
### ##################################################

## goal is to examine whether kites and night fishing reduce seabird bycatch
## also test whether fish catch is maintained
## major complication is that some sets declared as 'night' fishing straddle daytime
## these are specified in Adj_trial_types and Adj_trial subtypes

### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
filter<-dplyr::filter
select<-dplyr::select





#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     DATA IMPORT AND MANIPULATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

try(setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\Lithuania"), silent=T)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\Lithuania"), silent=T)

# Read the data from formatted CSV files (one for each mitigation trial)
data<-readRDS("data/LIT_bycatch_data_formatted.rds")
head(data)
dim(data)





#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    BASIC BOOTSTRAP ANALYSIS COMPARING BPUE AND CPUE GLOBALLY ~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########


controls<- data %>% filter(Trial_type_by_fishermen=="Control") 
kites<- data %>% filter(Trial_type_by_fishermen=="Kites")
nights<- data %>% filter(Trial_type_by_fishermen=="Night")


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

fwrite(DIFF_SUMMARY,"output/LIT_mitigation_difference_summary.csv")



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

ggsave("output/LIT_bycatch_mitigation_summary.jpg", width=8, height=11)


    



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    CONDUCT PAIRED SAMPLES BOOTSTRAP ANALYSIS          ###   ~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

# ### NOT STARTED YET
# 
# ### calculating differences per trip and set to account for seasonal and timing differences
# ### also excluding the 'not really at night' sets to increase contrast
# 
# # ~~~~~~~~ commented out because it takes 2 hrs to run ~~~~~~~~~~~  ##
# ## FORMERLY CALLED APPROACH 2 - calculate difference per fishing trip and then bootstrap over difference
# ## because we have multiple control nets per LEB, we have two layers of random sampling
# ## we ensure that a sample is taken from each fishing trip by looping over trips
# # abandoned on 27 June as it is incredibly slow
# # resurrected on 11 July 2022 to include the depth matching of LEB and control nets
# # no longer calculates difference per fishing trip, but matches sampling from same trips
# # because of very long loop, calculate all matrices in the same loop
# 
# bootstraps<-10000
# # one execution takes 0.06983399 sec, so calculate length of duration as
# (round(0.06983399,2) * bootstraps * nrow(LEBs))/3600  ## in hours
# 
# boot.samples <- matrix(sample(LEBs$fishing_trip_id, size = bootstraps * nrow(LEBs), replace = TRUE),bootstraps, nrow(LEBs))
# LEB.samples <- array(NA,dim=c(bootstraps, nrow(LEBs),6),dimnames=list(NULL,NULL,c("BPUE","EPUE","GPUE","LPUE","CPUE","MPUE")))
# control.samples <- array(NA,dim=c(bootstraps, nrow(LEBs),6),dimnames=list(NULL,NULL,c("BPUE","EPUE","GPUE","LPUE","CPUE","MPUE")))
# 
# boot.samples.obs <- matrix(sample(LEBs$fishing_trip_id[LEBs$observer==1], size = bootstraps * nrow(LEBs), replace = TRUE),bootstraps, nrow(LEBs[LEBs$observer==1,]))
# LEB.samples.obs <- array(NA,dim=c(bootstraps, nrow(LEBs[LEBs$observer==1,]),6),dimnames=list(NULL,NULL,c("BPUE","EPUE","GPUE","LPUE","CPUE","MPUE")))
# control.samples.obs <- array(NA,dim=c(bootstraps, nrow(LEBs[LEBs$observer==1,]),6),dimnames=list(NULL,NULL,c("BPUE","EPUE","GPUE","LPUE","CPUE","MPUE")))
# 
# for(row in 1:bootstraps){
#   for (col in 1:nrow(LEBs)){
#     #start_time <- Sys.time()
#     xd<- data %>% filter(fishing_trip_id==boot.samples[row,col])
#     mean_depth<-mean(xd$depth[xd$EXP=="LEB"])  ## take mean for the odd trip with 2 LEBs
#     target_depth<-rnorm(1,mean_depth,5)
#     LEB.samples[row,col,1:6] <- xd %>% filter(EXP=="LEB") %>%
#       sample_n(1) %>%
#       select(BPUE,EPUE,GPUE,LPUE,CPUE,MPUE) %>%
#       unlist()
# 
#     control.samples[row,col,1:6] <-xd %>% filter(EXP=="control") %>%
#                                         mutate(target=target_depth) %>%
#                                         mutate(diff=abs(depth-target_depth)) %>%
#                                         filter(diff==min(diff)) %>%
#                                         sample_n(1) %>%
#                                         select(BPUE,EPUE,GPUE,LPUE,CPUE,MPUE) %>%
#                                         unlist()
# 
#     ### use only observer data ###
#     if(col <= nrow(LEBs[LEBs$observer==1,])){
#       xdo<- data %>% filter(fishing_trip_id==boot.samples.obs[row,col])
#       mean_depth<-mean(xdo$depth[xdo$EXP=="LEB"])  ## take mean for the odd trip with 2 LEBs
#       target_depth<-rnorm(1,mean_depth,5)
#       LEB.samples.obs[row,col,1:6] <- xdo %>% filter(EXP=="LEB") %>%
#         sample_n(1) %>%
#         select(BPUE,EPUE,GPUE,LPUE,CPUE,MPUE) %>%
#         unlist()
# 
#       control.samples.obs[row,col,1:6] <-xdo %>% filter(EXP=="control") %>%
#         mutate(target=target_depth) %>%
#         mutate(diff=abs(depth-target_depth)) %>%
#         filter(diff==min(diff)) %>%
#         sample_n(1) %>%
#         select(BPUE,EPUE,GPUE,LPUE,CPUE,MPUE) %>%
#         unlist()
#     } ## close if loop for observer data
#     #end_time <- Sys.time()
#     #end_time - start_time
#   } ## close loop over row of fishing trips
#   print(sprintf("finished bootstrap %i",row))
# } ## close loop over bootstrap samples
# 
# 
# 
# ### SUMMARISE ALL BOOTSTRAP SAMPLES
# metrics<-c("all birds","common eider","black and common guillemots","long-tailed duck","fish catch","mammals")
# allout<-data.frame()
# rawout<-data.frame()
# for(m in 1:length(metrics)) {
#   LEB.statistics <- apply(LEB.samples[,,m], 1, mean)
#   RATE_LEB<-data.frame(treatment="with LEB",mean=median(LEB.statistics),
#                        lcl=quantile(LEB.statistics,0.025),ucl=quantile(LEB.statistics,0.975))
#   control.statistics <- apply(control.samples[,,m], 1, mean)
#   RATE_control<-data.frame(treatment="control",mean=median(control.statistics, na.rm=T),
#                            lcl=quantile(control.statistics,0.025, na.rm=T),ucl=quantile(control.statistics,0.975, na.rm=T))
# 
#   ## OUTPUT FOR REPORT
#   rawout<-bind_rows(RATE_LEB,RATE_control) %>%
#     mutate(metric=metrics[m]) %>% mutate(obs="all data") %>%
#     bind_rows(rawout)
# 
#   ## summarise for plot
#   allout<-data.frame(mean=quantile(LEB.statistics-control.statistics,0.5)*2000, lcl=quantile(LEB.statistics-control.statistics,0.025)*2000,ucl=quantile(LEB.statistics-control.statistics,0.975)*2000) %>%
#     mutate(metric=metrics[m]) %>% mutate(obs="all data") %>%
#     bind_rows(allout)
# }
# 
# for(m in 1:length(metrics)) {
#   LEB.statistics <- apply(LEB.samples.obs[,,m], 1, mean)
#   RATE_LEB<-data.frame(treatment="with LEB",mean=median(LEB.statistics),
#                        lcl=quantile(LEB.statistics,0.025),ucl=quantile(LEB.statistics,0.975))
#   control.statistics <- apply(control.samples.obs[,,m], 1, mean)
#   RATE_control<-data.frame(treatment="control",mean=median(control.statistics, na.rm=T),
#                            lcl=quantile(control.statistics,0.025, na.rm=T),ucl=quantile(control.statistics,0.975, na.rm=T))
# 
#   ## OUTPUT FOR REPORT
#   rawout<-bind_rows(RATE_LEB,RATE_control) %>%
#     mutate(metric=metrics[m]) %>% mutate(obs="only observer data") %>%
#     bind_rows(rawout)
# 
#   ## summarise for plot
#   allout<-data.frame(mean=quantile(LEB.statistics-control.statistics,0.5)*2000, lcl=quantile(LEB.statistics-control.statistics,0.025)*2000,ucl=quantile(LEB.statistics-control.statistics,0.975)*2000) %>%
#     mutate(metric=metrics[m]) %>% mutate(obs="only observer data") %>%
#     bind_rows(allout)
# }
# 
# ### SAVE OUTPUT TO CSV
# ## first convert to mean trossa day
# rawout<-rawout %>%
#   mutate(mean=mean*2000,lcl=lcl*2000,ucl=ucl*2000) %>%
#   select(obs,metric,treatment,mean,lcl,ucl) %>%
#   arrange(obs,metric,treatment)
# 
# allout<-allout %>%
#   select(obs,metric,mean,lcl,ucl)%>%
#   arrange(obs,metric)
# 
# fwrite(allout,"Iceland_LEB_bootstrap_differences.csv")
# fwrite(rawout,"Iceland_LEB_bootstrap_bycatch_rates.csv")
# ############################### RESTART #############################################################
# #allout<-fread("Iceland_LEB_bootstrap_differences.csv")
# #rawout<-fread("Iceland_LEB_bootstrap_bycatch_rates.csv")
# 
# ### CREATE SUMMARY PLOT OF ALL TARGET GROUPS
# allout %>% filter(metric != "fish catch") %>%
#   group_by(obs) %>%
# 
#   ggplot(aes(y=mean, x=metric,colour=obs)) + geom_point(size=2, position=position_dodge(width=0.2))+
#   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03, position=position_dodge(width=0.2))+
#   labs(colour='Data source:') +
#   scale_y_continuous(limits=c(-0.5,0.5), breaks=seq(-0.5,0.5,0.1)) +
#   geom_hline(aes(yintercept=0), colour="darkgrey", linetype=2) +
#   xlab("") +
#   ylab("Bycatch difference in trossa per day with LEB") +
#   theme(panel.background=element_rect(fill="white", colour="black"),
#         axis.text=element_text(size=16, color="black"),
#         axis.title=element_text(size=18),
#         strip.text=element_text(size=18, color="black"),
#         legend.text=element_text(size=14, color="black"),
#         legend.title=element_text(size=18, color="black"),
#         legend.key=element_blank(),
#         legend.position=c(0.2,0.1),
#         strip.background=element_rect(fill="white", colour="black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank())
# 
# # ggsave("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\Iceland_Lumpfish\\Fig_1.jpg", width=8, height=9, dpi=1000)
# # ggsave("C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\Iceland_Lumpfish\\Fig_1.jpg", width=8, height=9, dpi=1000)
# # ggsave("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\Iceland_Lumpfish\\Fig3.jpg", width=11, height=8, dpi=1000)
# 
# 
# #### CREATE TABLE 2 FOR MANUSCRIPT
# 
# TABLE2<-rawout %>% filter(obs=="all data") %>%
#   mutate(value=paste(round(mean,2)," (",round(lcl,2), " - ",round(ucl,2),")", sep="")) %>%
#   select(-obs,-mean,-lcl,-ucl) %>%
#   spread(key=treatment, value=value)
# 
# TABLE2<-allout %>% filter(obs=="all data") %>%
#   mutate(difference=paste(round(mean,2)," (",round(lcl,2), " - ",round(ucl,2),")", sep="")) %>%
#   select(-obs,-mean,-lcl,-ucl) %>%
#   left_join(TABLE2, by="metric") %>%
#   select(metric,`with LEB`,control,difference)
# 
# #fwrite(TABLE2,"C:\\Users\\steffenoppel\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\Iceland_Lumpfish\\TABLE2.csv")
# #fwrite(TABLE2,"C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\MANUSCRIPTS\\in_prep\\Iceland_Lumpfish\\TABLE2.csv")
# 
