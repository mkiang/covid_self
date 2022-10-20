## Imports ----
library(tidyverse)
library(here)
library(fs)

parse_response <- function(x) {
    jsonlite::fromJSON(httr::content(x, "text", encoding = "UTF-8"))
}

parse_daily_activity <- function(x) {
    ## NOTE: This does not parse 'met' or 'class_5_min'
    xx <- parse_response(x)
    
    if (NROW(xx$data) == 0) {
        return(NULL)
    }
    
    tibble(
        date = lubridate::ymd(xx$data[["day"]]),
        tstamp = lubridate::ymd_hms(xx$data[["timestamp"]]),
        tstamp_orig = xx$data[["timestamp"]],
        score = xx$data[["score"]],
        contri_meet_daily_targets = xx$data[["contributors"]][["meet_daily_targets"]],
        contri_move_every_hour = xx$data[["contributors"]][["move_every_hour"]],
        contri_recovery_time = xx$data[["contributors"]][["recovery_time"]],
        contri_stay_active = xx$data[["contributors"]][["stay_active"]],
        contri_training_frequency = xx$data[["contributors"]][["training_frequency"]],
        contri_training_volume = xx$data[["contributors"]][["training_volume"]],
        active_calories = xx$data[["active_calories"]],
        average_met_minutes = xx$data[["average_met_minutes"]],
        equivalent_walking_distance = xx$data[["equivalent_walking_distance"]],
        high_activity_met_minutes = xx$data[["high_activity_met_minutes"]],
        high_activity_time = xx$data[["high_activity_time"]],
        inactivity_alerts = xx$data[["inactivity_alerts"]],
        low_activity_met_minutes = xx$data[["low_activity_met_minutes"]],
        low_activity_time = xx$data[["low_activity_time"]],
        medium_activity_met_minutes = xx$data[["medium_activity_met_minutes"]],
        medium_activity_time = xx$data[["medium_activity_time"]],
        meters_to_target = xx$data[["meters_to_target"]],
        non_wear_time = xx$data[["non_wear_time"]],
        resting_time = xx$data[["resting_time"]],
        sedentary_met_minutes = xx$data[["sedentary_met_minutes"]],
        sedentary_time = xx$data[["sedentary_time"]],
        steps = xx$data[["steps"]],
        target_calories = xx$data[["target_calories"]],
        target_meters = xx$data[["target_meters"]],
        total_calories = xx$data[["total_calories"]]
    ) %>% 
        pivot_longer(cols = score:total_calories,
                     names_to = "metric") %>% 
        mutate(metric_type = "daily_activity",
               resolution = "daily",
               .before = 1) %>% 
        distinct()
}

parse_daily_readiness <- function(x) {
    xx <- parse_response(x)
    
    if (NROW(xx$data) == 0) {
        return(NULL)
    }
    
    tibble(
        date = lubridate::ymd(xx$data[["day"]]),
        tstamp = lubridate::ymd_hms(xx$data[["timestamp"]]),
        tstamp_orig = xx$data[["timestamp"]],
        score = xx$data[["score"]],
        temp_deviation = xx$data[["temperature_deviation"]],
        temperature_trend_deviation = xx$data[["temperature_trend_deviation"]],
        contri_resting_heart_rate = xx$data$contributors[["resting_heart_rate"]],
        contri_sleep_balance = xx$data$contributors[["sleep_balance"]],
        contri_recovery_index = xx$data$contributors[["recovery_index"]],
        contri_previous_night = xx$data$contributors[["previous_night"]],
        contri_previous_day_activity = xx$data$contributors[["previous_day_activity"]],
        contri_activity_balance = xx$data$contributors[["activity_balance"]],
        contri_body_temperature = xx$data$contributors[["body_temperature"]],
        contri_hrv_balance = xx$data$contributors[["hrv_balance"]]
    ) %>% 
        pivot_longer(cols = score:contri_hrv_balance,
                     names_to = "metric") %>% 
        mutate(metric_type = "daily_readiness",
               resolution = "daily",
               .before = 1) %>% 
        distinct()
}

parse_daily_sleep <- function(x) {
    xx <- parse_response(x)
    
    if (NROW(xx$data) == 0) {
        return(NULL)
    }
    
    tibble(
        date = lubridate::ymd(xx$data[["day"]]),
        tstamp = lubridate::ymd_hms(xx$data[["timestamp"]]),
        tstamp_orig = xx$data[["timestamp"]],
        score = xx$data[["score"]],
        contri_total_sleep = xx$contributors[["total_sleep"]],
        contri_timing = xx$contributors[["timing"]],
        contri_restfulness = xx$data$contributors[["restfulness"]],
        contri_rem_sleep = xx$data$contributors[["rem_sleep"]],
        contri_latency = xx$data$contributors[["latency"]],
        contri_efficiency = xx$data$contributors[["efficiency"]],
        contri_deep_sleep = xx$data$contributors[["deep_sleep"]]
    ) %>% 
        pivot_longer(cols = score:contri_deep_sleep,
                     names_to = "metric") %>% 
        mutate(metric_type = "daily_sleep",
               resolution = "daily",
               .before = 1) %>% 
        distinct()
}

parse_heartrate <- function(x) {
    xx <- parse_response(x)
    
    if (NROW(xx$data) == 0) {
        return(NULL)
    }
    
    tibble(
        tstamp = lubridate::ymd_hms(xx$data[["timestamp"]]),
        tstamp_orig = xx$data[["timestamp"]],
        metric_info = xx$data[["source"]],
        metric = "heartrate",
        value = xx$data[["bpm"]]
    ) %>%
        mutate(date = lubridate::ymd(
            sprintf(
                "%s-%s-%s",
                lubridate::year(tstamp),
                lubridate::month(tstamp),
                lubridate::day(tstamp)
            )
        ),
        .before = 1) %>%
        mutate(metric_type = "heartrate",
               resolution = "subdaily",
               .before = 1) %>%
        distinct()
}

parse_readiness <- function(x) {
    ## WARNING READINESS IS STILL API V1 so this will change. 
    xx <- parse_response(x)
    
    if (NROW(xx$data) == 0) {
        return(NULL)
    }
    
    tibble(
        tstamp = NA,
        tstamp_orig = NA,
        metric_info = xx$readiness[["period_id"]],
        score = xx$readiness[["score"]],
        contri_hrv_balance = xx$readiness[["score_hrv_balance"]],
        contri_activity_balance = xx$readiness[["score_activity_balance"]],
        contri_previous_day = xx$readiness[["score_previous_day"]],
        contri_previous_night = xx$readiness[["score_previous_night"]],
        contri_recovery_index = xx$readiness[["score_recovery_index"]],
        contri_resting_hr = xx$readiness[["score_resting_hr"]],
        contri_sleep_balance = xx$readiness[["score_sleep_balance"]],
        contri_temperature = xx$readiness[["score_temperature"]],
        rest_mode_state = xx$readiness[["rest_mode_state"]]
    ) %>% 
        pivot_longer(cols = score:rest_mode_state,
                     names_to = "metric") %>% 
        mutate(metric_type = "readiness",
               resolution = "daily",
               .before = 1) %>% 
        distinct()
}

parse_sleep <- function(x) {
    xx <- parse_response(x)
    
    if (NROW(xx$data) == 0) {
        return(NULL)
    }
    
    tibble(
        date = lubridate::ymd(xx$data[["day"]]),
        tstamp = lubridate::ymd_hms(xx$data[["bedtime_start"]]),
        tstamp_orig = xx$data[["bedtime_start"]],
        metric_info =  xx$data[["type"]], 
        avg_breath = xx$data[["average_breath"]],
        avg_heartrate = xx$data[["average_heart_rate"]],
        avg_hrv = xx$data[["average_hrv"]],
        deep_sleep_duration = xx$data[["deep_sleep_duration"]],
        rem_sleep_duration = xx$data[["rem_sleep_duration"]],
        sleep_efficiency = xx$data[["efficiency"]],
        sleep_latency = xx$data[["latency"]],
    ) %>% 
        pivot_longer(cols = avg_breath:sleep_latency,
                     names_to = "metric") %>% 
        mutate(metric_type = "sleep",
               resolution = "subdaily",
               .before = 1) %>% 
        distinct()
}
