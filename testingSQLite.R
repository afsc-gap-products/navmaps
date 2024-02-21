
library(navmaps)
##############
x= get_tzdata(path_tzdb= here::here("assets", "data", "OwnShipRecorder.tzdb"))
##############
convert_tz_coords(x=x) 
convert_tz_date(x=x)
