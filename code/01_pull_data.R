## Download API pulls ----
library(tidyverse)
library(here)
library(fs)
source(here("code", "utils_api.R"))

## CONSTANTS ----
START_DATE <- as.Date("YYYY-MM-DD", tz = "UTC")
DATES <- seq.Date(
    START_DATE,
    Sys.Date() - 1,
    by = "1 day",
    tz = "UTC",
    origin = "1970-01-01"
)

for (i in 1:NROW(DATES)) {
    ## Create basename for the file (same name for each stream)
    d <- DATES[i]
    f_base <- sprintf(
        "data_pull_%04d%02d%02d.RDS",
        lubridate::year(d),
        lubridate::month(d),
        lubridate::day(d)
    )
    
    ## Daily activity
    f_x <- here("data_raw", "daily_activity", f_base)
    if (!file_exists(f_x)) {
        dir_create(dirname(f_x))
        temp_x <- get_daily_activity(PAT,
                                     start = d - 1,
                                     end =   d)
        if (temp_x$status_code == 200) {
            saveRDS(temp_x, f_x)
            print(sprintf("Downloading: %s", d))
        } else {
            print(sprintf("Error downloaing: %s", d))
        }
        Sys.sleep(1)
    } else {
        print(sprintf("Skipping: %s", d))
    }
    
    ## Heartrate
    f_x <- here("data_raw", "heartrate", f_base)
    if (!file_exists(f_x)) {
        dir_create(dirname(f_x))
        temp_x <- get_heartrate(PAT,
                                start = d - 1,
                                end =   d)
        if (temp_x$status_code == 200) {
            saveRDS(temp_x, f_x)
        }
        Sys.sleep(1)
    }
    
    ## Sessions
    f_x <- here("data_raw", "sessions", f_base)
    if (!file_exists(f_x)) {
        dir_create(dirname(f_x))
        temp_x <- get_sessions(PAT,
                               start = d - 1,
                               end =   d)
        if (temp_x$status_code == 200) {
            saveRDS(temp_x, f_x)
        }
        Sys.sleep(1)
    }
    
    ## Tags
    f_x <- here("data_raw", "tags", f_base)
    if (!file_exists(f_x)) {
        dir_create(dirname(f_x))
        temp_x <- get_tags(PAT,
                           start = d - 1,
                           end =   d)
        if (temp_x$status_code == 200) {
            saveRDS(temp_x, f_x)
        }
        Sys.sleep(1)
    }
    
    ## Workouts
    f_x <- here("data_raw", "workouts", f_base)
    if (!file_exists(f_x)) {
        dir_create(dirname(f_x))
        temp_x <- get_workouts(PAT,
                               start = d - 1,
                               end =   d)
        if (temp_x$status_code == 200) {
            saveRDS(temp_x, f_x)
        }
        Sys.sleep(1)
    }
    
    ## Sleep
    ## NOTE: Sleep timestamps correspond to when you go to bed rather than
    ## when you wake up so to match the rest of them, I use d+1 as the end.
    f_x <- here("data_raw", "sleep", f_base)
    if (!file_exists(f_x)) {
        dir_create(dirname(f_x))
        temp_x <- get_sleep(PAT,
                            start = d,
                            end =   d + 1)
        if (temp_x$status_code == 200) {
            saveRDS(temp_x, f_x)
        }
        Sys.sleep(1)
    }
    
    ## Daily sleep
    f_x <- here("data_raw", "daily_sleep", f_base)
    if (!file_exists(f_x)) {
        dir_create(dirname(f_x))
        temp_x <- get_daily_sleep(PAT,
                                  start = d - 1,
                                  end =   d)
        if (temp_x$status_code == 200) {
            saveRDS(temp_x, f_x)
        }
        Sys.sleep(1)
    }
    
    ## Readiness
    f_x <- here("data_raw", "readiness", f_base)
    if (!file_exists(f_x)) {
        dir_create(dirname(f_x))
        temp_x <- get_readiness(PAT,
                                start = d - 1,
                                end =   d)
        if (temp_x$status_code == 200) {
            saveRDS(temp_x, f_x)
        }
        Sys.sleep(1)
    }
    
    ## Daily readiness
    f_x <- here("data_raw", "daily_readiness", f_base)
    if (!file_exists(f_x)) {
        dir_create(dirname(f_x))
        temp_x <- get_daily_readiness(PAT,
                                      start = d - 1,
                                      end =   d)
        if (temp_x$status_code == 200) {
            saveRDS(temp_x, f_x)
        }
        Sys.sleep(1)
    }
}
