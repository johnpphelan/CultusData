
table_choices <- c("Scale aging", "Acoustic tag data", "Electrofishing", "Nest data", "Angling Basok", "Angling derby", 
                   "Recapture data", "Scale 500", "Sweltzer creek", "Creel - Fish Details", 
                   "Creel - Fishing Results", "Creel - Fisher Survey", "Creel - Shifts", "Creel - Weather Conditions", 
                    "High reward tags", "High reward tags - claimed","Creel - Survey Responses",
                   "Creel - Survey Questions", "Temperature - Shore", "Temperature - CLASS", "Tag Event", "Tag Sold Information")
table_choices<-sort(table_choices)

creel_tables<-c("Creel - Fish Details","Creel - Fisher Survey", "Creel - Fishing Results","Creel - Shifts",
                "Creel - Survey Responses","Creel - Weather Conditions", "Creel - Survey Questions")
creel_tables<-sort(creel_tables)

table_col_search<-c("NA","pitTagNo", "transmitter", "receiver", "stationName", "receiver_name", "location", "acousticTagNo", "length", "weight", "tagID", "ScaleBookNo")
table_col_search <- sort(table_col_search)


template_list<-c("Abundance estiamte" = "", "angling Basok and Buck", "Angling Derby", "High Reward Tags", "High Reward Tags Claimed",
                 "Nest Data", "scale 500", "Scale Aging", "Spring Marking", "Sweltzer Creek", "Tag Data", "Scale Column Name Key")

