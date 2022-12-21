rm(list = ls())

devtools::install_github("sean-rohan-noaa/navmaps")

library(navmaps)

# Create a new table
file.remove(here::here("output", "test.mdb"))

write_to_mdb(x = data.frame(x_val = 1, y_val = "test", datetime = Sys.time()),
             dsn = here::here("output", "test.mdb"),
             tablename = "output",
             append = FALSE,
             drop_existing = TRUE)


# Overwrite
write_to_mdb(x = data.frame(x_val = 1, y_val = "test", datetime = Sys.time()),
             dsn = here::here("output", "test.mdb"),
             tablename = "output",
             append = FALSE,
             drop_existing = TRUE)