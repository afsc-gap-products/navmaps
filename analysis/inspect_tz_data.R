library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "OwnShipRecorder.tzdb")

dat <- dbGetQuery(con, "SELECT date, x, y FROM Data")

head(dat)

dbDisconnect(con)
