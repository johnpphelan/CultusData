library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
source("scripts/utils/fix_col_names_f.R")


insertToMainCreel<-function(maindata){
  survDT<- maindata |> 
    select(Survey_No, Date, Time, Surveyor, Shift) |> 
    mutate(Date = as.Date(Date, origin = "1899-12-30"),
           Time = convertToDateTime(as.numeric(Time))) |> 
    rename(surveyNumber = Survey_No, date = Date, time = Time, surveyor = Surveyor, shift = Shift) |> 
    mutate(date = as.character(date), time = as.character(time), shift = as.character(shift)) 
  
  
  
  
  #recentID<-dbGetQuery(conn = con, "SELECT surveyNumber FROM surveyData")
  #recentID
  dbAppendTable(con, "surveyData", survDT) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work
  
  recordToCheck<- survDT |> 
    arrange(desc(time)) 
 
  #tb<-dbGetQuery(con, paste0("SELECT * FROM surveyData")) 
  upload_test<-list()
 for (i in 1:nrow(recordToCheck)) {
   tester<-recordToCheck[i,]
   upload_test[[i]]<-dbGetQuery(con, paste0("SELECT * FROM surveyData WHERE time = '",tester$time,
                                       "' AND surveyNumber = '",tester$surveyNumber,"';")) 
 }  
 
  
 if(nrow(recordToCheck) == length(upload_test)){
   upload = 'successful'
 } else {
   upload = 'not successful'
 }
 
 cat(paste0("Upload test: ",upload))  
}

insertToFishingCatchCreel<-function(maindata){
  
  catchDF<- maindata |> 
    select(Survey_No, Date, Time, Total_Fish_Caught, Total_Retained, ends_with("_c"), ends_with("_r"),
           Release_Reason) |>
    mutate(Date = as.Date(Date, origin = "1899-12-30"),
           Time = convertToDateTime(as.numeric(Time))) |> 
    rename(surveyNumber = Survey_No, date = Date, time = Time, totFishCaught = Total_Fish_Caught,
           totRetained = Total_Retained, releaseReason = Release_Reason) |>
    rename_with(~ str_replace_all(.x, "_", ""), everything()) |> 
    mutate(date = as.character(date), time = as.character(time)) 
  
  dbAppendTable(con, "fishCatch", catchDF) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work
  
  recordToCheck<- catchDF |> 
    arrange(desc(time)) 
  
  #tb<-dbGetQuery(con, paste0("SELECT * FROM surveyData")) 
  upload_test<-list()
  for (i in 1:nrow(recordToCheck)) {
    tester<-recordToCheck[i,]
    upload_test[[i]]<-dbGetQuery(con, paste0("SELECT * FROM fishCatch WHERE time = '",tester$time,
                                             "' AND surveyNumber = '",tester$surveyNumber,"';")) 
  }  
  
  
  if(nrow(recordToCheck) == length(upload_test)){
    upload = 'successful'
  } else {
    upload = 'not successful'
  }
  cat(paste0("Upload test: ",upload))  
  
}

insertfishingDetailsCreel<-function(maindata){
  
  fishingdetails<- maindata |> 
    select(Survey_No, Date, Time, No_Anglers, No_Rods, Total_Person_Hr_Fished, Vessel,
           Preferred_Catch_Spp, Site) |> 
    mutate(Date = as.Date(Date, origin = "1899-12-30"),
           Time = convertToDateTime(as.numeric(Time))) |> 
    rename(surveyNumber = Survey_No, date = Date, time = Time, numberAnglers = No_Anglers, numberRods = No_Rods, 
           personHoursFished = Total_Person_Hr_Fished, vessel = Vessel, prefferedSpp = Preferred_Catch_Spp,
           site = Site) |>   
    mutate(date = as.character(date), time = as.character(time))
  
  dbAppendTable(con, "fishingDetails", fishingdetails) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work
  
  recordToCheck<- fishingdetails |> 
    arrange(desc(time)) 
  
  #tb<-dbGetQuery(con, paste0("SELECT * FROM surveyData")) 
  upload_test<-list()
  for (i in 1:nrow(recordToCheck)) {
    tester<-recordToCheck[i,]
    upload_test[[i]]<-dbGetQuery(con, paste0("SELECT * FROM fishingDetails WHERE time = '",tester$time,
                                             "' AND surveyNumber = '",tester$surveyNumber,"';")) 
  }  
  
  
  if(nrow(recordToCheck) == length(upload_test)){
    upload = 'successful'
  } else {
    upload = 'not successful'
  }
  cat(paste0("Upload test: ",upload))  
}

insertWeatherCreel<-function(maindata){
  weatherTable<- creelData |> 
    mutate(Date = as.Date(Date, origin = "1899-12-30"),
           Time = convertToDateTime(as.numeric(Time))) |> 
    select(Survey_No, Time, Site, Air__temperature, Cloud_Cover, Wind, Precip) |> 
    rename(surveyNumber = Survey_No, time = Time, site = Site, meanAirTemp= Air__temperature,
           cloudCover = Cloud_Cover, wind = Wind, precip = Precip) |> 
    mutate(time = as.character(time))
  
  
  dbAppendTable(con, "weatherDetails", weatherTable) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work
  
  recordToCheck<- weatherTable |> 
    arrange(desc(time)) 
  
  #tb<-dbGetQuery(con, paste0("SELECT * FROM surveyData")) 
  upload_test<-list()
  for (i in 1:nrow(recordToCheck)) {
    tester<-recordToCheck[i,]
    upload_test[[i]]<-dbGetQuery(con, paste0("SELECT * FROM weatherDetails WHERE time = '",tester$time,
                                             "' AND surveyNumber = '",tester$surveyNumber,"';")) 
  }  
  
  
  if(nrow(recordToCheck) == length(upload_test)){
    upload = 'successful'
  } else {
    upload = 'not successful'
  }
  cat(paste0("Upload test: ",upload))  
}

insertQuestionsCreel<-function(maindata){
  questionTable<- maindata |> 
    colnames()
  ##how to do this better?
  questionTable <- questionTable |> 
    as.data.frame() |> 
    slice(32:length(questionTable)) |> 
    rename(question = questionTable)
  
  query <- "SELECT * FROM surveyQuestions"
  dbExecute(con = con, query)
  #querydelete<-"DROP TABLE surveyData"
  result <- dbSendQuery(conn = con, query)
  df<-fetch(result, -1)
  df
  dbClearResult(result)
  
  miss<- questionTable |> 
    filter(!question %in% df$question)
  if(length(miss)!=0){
      print("Already present")
    }else{
    adding<- miss |> 
      mutate(questionID = max(df$questionID) + row_number())
    
    dbAppendTable(con, "surveyQuestions", adding) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work
    
    recordToCheck<- adding |> 
      arrange(desc(questionID)) 
    
    #tb<-dbGetQuery(con, paste0("SELECT * FROM surveyData")) 
    upload_test<-list()
    for (i in 1:nrow(recordToCheck)) {
      tester<-recordToCheck[i,]
      upload_test[[i]]<-dbGetQuery(con, paste0("SELECT * FROM surveyQuestions WHERE questionID = '",tester$questionID,"';")) 
    }  
    
    
    if(nrow(recordToCheck) == length(upload_test)){
      upload = 'successful'
    } else {
      upload = 'not successful'
    }
    cat(paste0("Upload test: ",upload)) 
    
  }
}

insertSurveyAnswersCreel<-function(maindata){
  query <- "SELECT * FROM surveyQuestions"
  dbExecute(con = con, query)
  #querydelete<-"DROP TABLE surveyData"
  result <- dbSendQuery(conn = con, query)
  df<-fetch(result, -1)
  df
  dbClearResult(result)
  
  answersTable<- maindata |> 
    select(c(Survey_No, Time, contains(df$question)))
  
  
  answersLong <- answersTable |> 
    mutate(Time = convertToDateTime(as.numeric(Time))) |> 
    pivot_longer(cols = -c(Survey_No, Time), names_to = "Question", values_to = "Answer") |> 
    left_join(df, by = c("Question" = "question")) |> 
    mutate(Question = questionID) |> 
    select(-questionID) |> 
    rename( surveyNumber = Survey_No, time = Time, question = Question, answer = Answer)  |> 
    mutate(time = as.character(time))
  
  
  
  dbAppendTable(con, "surveyAnswers", answersLong) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work
  
  recordToCheck<- answersLong |> 
    arrange(desc(time)) 
  
  #tb<-dbGetQuery(con, paste0("SELECT * FROM surveyData")) 
  upload_test<-list()
  for (i in 1:nrow(recordToCheck)) {
    tester<-recordToCheck[i,]
    upload_test[[i]]<-dbGetQuery(con, paste0("SELECT * FROM surveyAnswers WHERE time = '",tester$time,
                                             "' AND surveyNumber = '",tester$surveyNumber,"';")) 
  }  
  
  
  if(nrow(recordToCheck) == length(upload_test)){
    upload = 'successful'
  } else {
    upload = 'not successful'
  }
  cat(paste0("Upload test: ",upload))  
}

insertICEData<-function(iceData){
  ICE2<- iceData |> 
    mutate(Date = as.Date(Date, origin = "1899-12-30")) |> 
    rename(date = Date, time = Time, noAnglingBoats = X..angling.boats, noBoatAnglers = X..boat.anglers, 
           noShoreAnglers = X..shore.anglers, noDockAnglers=X..dock.anglers, airTemp = Air.temperature,
           cloudCover = Cloud.cover, wind = Wind, precip = Precipitation) |> 
    mutate(weather = paste0("Air temperature: ", airTemp,", cloud cover: ", cloudCover, ", wind: ", wind,", precipiation: ", precip)) |> 
    select(date, time, noAnglingBoats, noBoatAnglers, noShoreAnglers, noDockAnglers, weather) |> 
    mutate(date = as.character(date), time = as.character(time))
  
  
  dbAppendTable(con, "iceData", ICE2) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work
  
  
  recordToCheck<- ICE2 |> 
    arrange(desc(time)) 
  
  #tb<-dbGetQuery(con, paste0("SELECT * FROM surveyData")) 
  upload_test<-list()
  for (i in 1:nrow(recordToCheck)) {
    tester<-recordToCheck[i,]
    upload_test[[i]]<-dbGetQuery(con, paste0("SELECT * FROM iceData WHERE time = '",tester$time,"';")) 
  }  
  
  
  if(nrow(recordToCheck) == length(upload_test)){
    upload = 'successful'
  } else {
    upload = 'not successful'
  }
  cat(paste0("Upload test: ",upload))  
  
}

insertAnglerInfo<-function(demoData){
  
  
  anglerData<- demoData |> 
    select(surveyNumber, date, time,  gender, ageClass, licensePeriod, residency,
           cityProvinceCountry, postCode, notes) |> 
    mutate(date = as.character(date), time = as.character(time))
  
  query <- "SELECT * FROM anglerInfo"
  dbExecute(con = con, query)
  #querydelete<-"DROP TABLE surveyData"
  result <- dbSendQuery(conn = con, query)
  df<-fetch(result, -1)
  df
  dbClearResult(result)
  
  countVal<-max(df$anglerID)
  
  anglerData <- anglerData |> 
    mutate(anglerID = row_number()+countVal)
  
  
  
  dbAppendTable(con, "anglerInfo", anglerData) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work
  
  recordToCheck<- anglerData |> 
    arrange(desc(time)) 
  
  #tb<-dbGetQuery(con, paste0("SELECT * FROM surveyData")) 
  upload_test<-list()
  for (i in 1:nrow(recordToCheck)) {
    tester<-recordToCheck[i,]
    upload_test[[i]]<-dbGetQuery(con, paste0("SELECT * FROM anglerInfo WHERE time = '",tester$time,
                                             "' AND anglerID = '",tester$anglerID,"';")) 
  }  
  
  
  if(nrow(recordToCheck) == length(upload_test)){
    upload = 'successful'
  } else {
    upload = 'not successful'
  }
  cat(paste0("Upload test: ",upload)) 
  
  
}


insertfishcaughtDetails<-function(catchfish){
  
  fishcaught<- catchfish |> 
    rename(surveyNumber = Survey.., time = Date...Time, fishNo = Fish.., length = Length..mm, weight = Weight..g,
           pitTagNo = Pit.Tag.., notes = Notes) |> 
    mutate(date = format(as.Date(time), "%Y-%m-%d")) |> 
    select(surveyNumber, time, date, fishNo, Spp, length, weight, pitTagNo, notes)
  
  query <- "SELECT MAX(fishID) AS max_fishID FROM fishCaught;"
  dbExecute(con = con, query)
  #querydelete<-"DROP TABLE surveyData"
  result <- dbSendQuery(conn = con, query)
  df<-fetch(result, -1)
  df
  dbClearResult(result)
  
  fishcaught <- fishcaught |> 
    mutate(fishID = row_number()+df$max_fishID)
  
  dbAppendTable(con, "fishCaught", fishcaught) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work
  
  recordToCheck<- fishcaught |> 
    arrange(desc(fishID)) 
  
  #tb<-dbGetQuery(con, paste0("SELECT * FROM surveyData")) 
  upload_test<-list()
  for (i in 1:nrow(recordToCheck)) {
    tester<-recordToCheck[i,]
    upload_test[[i]]<-dbGetQuery(con, paste0("SELECT * FROM fishCaught WHERE fishID = '",tester$fishID,"';")) 
  }  
  
  
  if(nrow(recordToCheck) == length(upload_test)){
    upload = 'successful'
  } else {
    upload = 'not successful'
  }
  cat(paste0("Upload test: ",upload)) 
  
  
}
