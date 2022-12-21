rm(list = ls())

# devtools::install_github("sean-rohan-noaa/navmaps")

library(navmaps)

# Create a new table
file.remove(here::here("output", "test.mdb"))
file.remove(here::here("output", "test.accdb"))

# Testing .mdb
write_to_access(x = data.frame(x_val = 1, y_val = "test", datetime = as.character(Sys.time())),
             dsn = here::here("output", "test.mdb"),
             tablename = "output",
             append = FALSE,
             drop_existing = TRUE)


# Overwrite .mdb
write_to_access(x = data.frame(x_val = 1, y_val = "test", datetime = Sys.time()),
             dsn = here::here("output", "test.mdb"),
             tablename = "output",
             append = FALSE,
             drop_existing = TRUE)


# Testing .accdb
write_to_access(x = data.frame(x_val = 1, y_val = "test", datetime = as.character(Sys.time())),
                dsn = here::here("output", "test.mdb"),
                tablename = "output",
                append = FALSE,
                drop_existing = TRUE)


# Overwrite .accdb
write_to_access(x = data.frame(x_val = 1, y_val = "test", datetime = as.character(Sys.time())),
                dsn = here::here("output", "test.mdb"),
                tablename = "output",
                append = FALSE,
                drop_existing = TRUE)
