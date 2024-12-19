library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


file_list<-list.files(path =paste0(lan_folder,"2024 projects/Sweltzer Creek/"),
                      pattern = "*.xlsx", full.names = T) 

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)

creek_survey<-read_excel(path = file_list[[2]], sheet = 1, col_names = FALSE)


creek_info<-creek_survey[1:7,]
creek_info<-creek_info[,1:2]

creek_info <- as.data.frame(t(creek_info))
names(creek_info)<-lapply(creek_info[1,], as.character)
creek_info<-creek_info[-1,]

names(creek_info)<-gsub(":", "", names(creek_info))
names(creek_info)<-gsub(" ", "", names(creek_info))

creek_info$StartTime<-dhours(as.numeric(creek_info$StartTime)*24)
creek_info$StartTime<-as.POSIXct("2000-01-01 00:00:00")+creek_info$StartTime
creek_info$StartTime<-format(creek_info$StartTime, format = "%H:%M:%S")

creek_info$EndTime<-dhours(as.numeric(creek_info$EndTime)*24)
creek_info$EndTime<-as.POSIXct("2000-01-01 00:00:00")+creek_info$EndTime
creek_info$EndTime<-format(creek_info$EndTime, format = "%H:%M:%S")

creek_survey_response<-creek_survey[9:11,]


names(creek_survey_response)<-lapply(creek_survey_response[1,], as.character)
creek_survey_response<-creek_survey_response[-1,]
names(creek_survey_response)<-gsub("[(*)\\.]", "", names(creek_survey_response))
names(creek_survey_response)<-gsub(" ","_", names(creek_survey_response))


creek_survey_response

creek_comments<-creek_survey[14:15,]
creek_comments<-creek_comments[,1]
names(creek_comments)<-"Comments"

all_Creek<-cbind(creek_info, creek_survey_response)
all_Creek<-cbind(all_Creek, creek_comments)

all_Creek<-all_Creek |> 
  mutate(surveyID = row_number())

col_types<-get_col_types(all_Creek)

sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS SweltzerCreek (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (surveyID));")
dbExecute(con, sql)
dbWriteTable(conn = con, "SweltzerCreek", all_Creek, row.names = F, append = T)
dbListTables(con)
