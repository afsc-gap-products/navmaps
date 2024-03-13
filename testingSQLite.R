
library(navmaps)
##############
x= get_tzdata(path_tzdb= here::here("assets", "data", "OwnShipRecorder.tzdb"))
##############
x2 <- convert_tz_coords(x=x) 
x3 <- convert_tz_date(x2)


convert_tz_to_gps(path_tzdb= here::here("assets", "data", "OwnShipRecorder.tzdb"), vessel = 162, cruise = 202301, haul = 184, start = "2024-01-25 00:00:00", end = "2024-01-25 06:00:00")

x= get_tzdata(path_tzdb= here::here("assets", "data", "OwnShipRecorder.tzdb"), start = "2024-01-25 00:00:00", end = "2024-01-25 06:00:00")


start = "2024-01-25 00:00:00"
end = "2024-01-25 06:00:00"