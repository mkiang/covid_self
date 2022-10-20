## Endpoints for the Oura API:
## https://cloud.ouraring.com/v2/docs#section/Overview

## Imports ----
library(here)
library(tidyverse)
library(httr)
library(jsonlite)

## Get Personal Access Token ----
source(here("code", "secrets.R"))

return_url <- function(endpoint = "personal_info", 
                       start,
                       end) {
    ## V2: personal_info, daily_activity, heartrate, session, tag, workout
    ## New V2: daily_sleep and daily_readiness and I think sleep (?)
    ## V1: readiness, sleep (?)
    root <- "https://api.ouraring.com"
    api_v <- ifelse(
        # endpoint %in% c("readiness", "sleep"),
        endpoint %in% c("readiness"),
        "v1", 
        "v2")
    if (api_v == "v2") {
        url <- sprintf("%s/%s/usercollection/%s", root, api_v, endpoint)
    } else {
        url <- sprintf("%s/%s/%s", root, api_v, endpoint)
    }
    
    url <- httr::modify_url(url = url,
                            query = make_v2_params(start, end))
    
    return(url)
}

make_header <- function(pat) {
    add_headers(
        "Authorization" = paste("Bearer", pat)
    )
}

make_v2_params <- function(start_date, end_date) {
    list(
        start_date = sprintf(
            '%04d-%02d-%02d',
            lubridate::year(start_date),
            lubridate::month(start_date),
            lubridate::day(start_date)
        ),
        end_date = sprintf(
            '%04d-%02d-%02d',
            lubridate::year(end_date),
            lubridate::month(end_date),
            lubridate::day(end_date)
        )
    )
}

get_personal_info <- function(pat) {
    httr::GET(return_url("personal_info"),
              config = make_header(pat),
              encode = "json")
}

get_daily_activity <- function(pat, start, end) {
    httr::GET(
        url = return_url("daily_activity", start, end),
        config = make_header(pat),
        encode = "json"
    )
}

get_heartrate <- function(pat, start, end) {
    ## Create a URL with datetimes (only one that uses datetime, I think)
    new_url <- return_url("heartrate", start, end)
    new_url <- paste0(
        gsub("&end_",
             "T00:00:00+00:00&end_", 
             new_url, 
             fixed = TRUE),
        "T00:00:00+00:00"
    )
    new_url <- gsub("_date=", "_datetime=", new_url, fixed = TRUE)
    
    httr::GET(
        url = new_url,
        config = make_header(pat),
        encode = "json"
    )
}

get_sessions <- function(pat, start, end) {
    httr::GET(
        url = return_url("session", start, end),
        config = make_header(pat),
        encode = "json"
    )
}

get_tags <- function(pat, start, end) {
    httr::GET(
        url = return_url("tag", start, end),
        config = make_header(pat),
        encode = "json"
    )
}

get_workouts <- function(pat, start, end) {
    httr::GET(
        url = return_url("workout", start, end),
        config = make_header(pat),
        encode = "json"
    )
}

get_sleep <- function(pat, start, end) {
    httr::GET(
        url = return_url("sleep", start, end),
        config = make_header(pat),
        encode = "json"
    )
}
   

get_daily_sleep <- function(pat, start, end) {
    httr::GET(
        url = return_url("daily_sleep", start, end),
        config = make_header(pat),
        encode = "json"
    )
}
 
get_readiness <- function(pat, start, end) {
    httr::GET(
        url = return_url("readiness", start, end),
        config = make_header(pat),
        encode = "json"
    )
}

get_daily_readiness <- function(pat, start, end) {
    httr::GET(
        url = return_url("daily_readiness", start, end),
        config = make_header(pat),
        encode = "json"
    )
}
