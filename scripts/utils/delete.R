

droptable<-"DROP TABLE IF EXISTS fishCaught"
dbExecute(con, droptable)


dropping<- "DELETE FROM weatherDetails WHERE time = '2024-05-25 07:09:00'"
dbExecute(con, dropping)


dropmult<- "DELETE FROM iceData WHERE date IN (SELECT date FROM iceData ORDER BY date DESC LIMIT 3)"
dbExecute(con, dropmult)

deleteYear<- "DELETE FROM fishingDetails WHERE time > '2024-01-01'"
dbExecute(con, deleteYear)
