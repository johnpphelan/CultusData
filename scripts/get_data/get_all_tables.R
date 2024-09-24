
data_list<-list()
source("scripts/get_data/get_abun_data.R")
data_list[[1]]<-capture_raw
names(data_list)[1]<-"abunCaptureRaw"
data_list[[2]]<-recapture_raw
names(data_list)[2]<-"abunRecaptureRaw"
rm(capture_mark, capture_raw, recapture_raw, recapture_wrk)
source("scripts/get_data/get_acoustic_telem.R")
data_list[[3]]<-as_tibble(tag_data23)
names(data_list)[3]<-"tag"
rm(tag_data23)
source("scripts/get_data/get_angling_derby.R")
data_list[[4]]<-angling_derby_data
names(data_list)[4]<-"anglingDerby"
rm(angling_derby_data)
source("scripts/get_data/get_creel.R")
data_list[[5]]<-main_page
names(data_list)[5]<-"creelMain"
data_list[[6]]<-demography_edit
names(data_list)[6]<-"creelDemography"
data_list[[7]]<-fish_edit
names(data_list)[7]<-"creelFish"
data_list[[8]]<-ICE
names(data_list)[8]<-"creelICE"
rm(demography_edit, fish_edit, main_page, ICE)
source("scripts/get_data/get_nest_survey.R")
data_list[[9]]<-raw_nest_survey
names(data_list)[9]<-"nestsurveyRaw"
rm(guarding_males, guarding_males_comb, nest_data_wrk, raw_nest_survey,lifestage)
source("scripts/get_data/get_scale_data.R")
data_list[[10]]<-scale_data
names(data_list)[10]<-"scaleData"
data_list[[11]]<-scale_500_data
names(data_list)[11]<-"scaleData500"
rm(scale_500_data, scale_data)
source("scripts/get_data/get_spring_marking.R")
data_list[[12]]<-spring_marking
names(data_list)[12]<-"springMarking"
rm(spring_marking)

source("scripts/get_data/get_angling_recapture.R")
data_list[[13]]<-angling_data
names(data_list)[13]<-"anglingRecapture"
rm(angling_data)

summary(data_list)


data_list[[1]]

