## Imports ----
library(tidyverse)
library(here)
library(fs)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

## Get heartrate and heart rate variability ----
sleep_df <- map_df(.x = dir_ls(here("data_raw", "sleep"), glob = "*.RDS"),
                   .f = ~ parse_sleep(readRDS(.x)))

dready_df <- map_df(.x = dir_ls(here("data_raw", "daily_readiness"), glob = "*.RDS"),
                   .f = ~ parse_daily_readiness(readRDS(.x)))

activity_df <- map_df(.x = dir_ls(here("data_raw", "daily_activity"), glob = "*.RDS"),
                   .f = ~ parse_daily_activity(readRDS(.x)))

## Combine the datasets ----
analytic_df <- bind_rows(
    sleep_df %>%
        filter(metric_info == "long_sleep") %>%
        select(-metric_info),
    activity_df,
    dready_df
) %>%
    mutate(t_delta = as.numeric(difftime(date, as.Date("2022-09-21"), units = "days"))) %>%
    filter(
        metric %in% c(
            "avg_breath",
            "avg_heartrate",
            "heartrate",
            "avg_hrv",
            "temp_deviation",
            "average_met_minutes",
            "high_activity_met_minutes",
            "total_calories",
            "steps"
        )
    ) %>%
    mutate(metric_cat = factor(
        metric,
        levels = c(
            "avg_breath",
            "avg_heartrate",
            "heartrate", 
            "avg_hrv",
            "temp_deviation",
            "average_met_minutes",
            "high_activity_met_minutes",
            "steps", 
            "total_calories"
            ),
        labels = c(
            "Respiratory rate (breaths/min)",
            "Heart rate (beats/min)",
            "Resting heart rate (beats/min)", 
            "Heart rate variability (ms)",
            "Body temperature deviation (F)",
            "Average activity (MET minutes)",
            "High-intensity activity (MET minutes)",
            "Steps (in thousands)", 
            "Energy expended (Calories)"
        ),
        ordered = TRUE
    )) %>% 
    mutate(value = ifelse(metric == "steps", value / 1000, value))

## Plot 1 ----
## Heart rate, temperature, breathing
mean_df <- analytic_df %>%
    filter(
        metric %in% c(
            "avg_breath",
            "avg_heartrate",
            # "heartrate",
            "avg_hrv",
            "temp_deviation" #,
            # "average_met_minutes",
            # "high_activity_met_minutes",
            # "total_calories"
        )
    ) %>% 
    filter(t_delta < 0) %>% 
    group_by(metric_cat) %>% 
    summarize(mean_value = mean(value))

sub_df <- analytic_df %>%
    filter(
        metric %in% c(
            "avg_breath",
            "avg_heartrate",
            # "heartrate",
            "avg_hrv",
            "temp_deviation" #,
            # "average_met_minutes",
            # "high_activity_met_minutes",
            # "total_calories"
        )
    )

## Custom break and label functions ----
breaks_fx <- function(x) {
    ## Resp rate
    if (between(min(x), 14.5, 15.5)) {
        c(15, 15.5, 16, 16.15, 16.5, 17, 17.5)
    ## Heart rate
    } else if (between(min(x), 59, 65)) {
        c(60, 65.4, 70, 80, 90)
    ## Heart rate variability
    } else if (between(min(x), 9, 13)) {
        c(10, 20, 30, 33.9, 40)
    ## Body temp deviation
    } else if (between(min(x), -2, 2)) {
       c(-.5, 0, .5, 1, 1.5)
    }
}

labels_fx <- function(x) {
    l_count <<- l_count + 1L
    switch(
        l_count,
        ## Resp rate
        c("15", "", "", "(16.2)", "", "17", ""),
        ## Heart rate
        c(60, "(65)", 70, 80, 90),
        ## Heart rate variability
        c(10, 20, "", "(34)", 40),
        ## Body temp deviation
        c(-.5, "(0)", .5, 1, 1.5)
    )
}

## Create plot ----
p1 <- ggplot(sub_df,
             aes(x = t_delta,
                 y = value)) +
    geom_hline(
        data = mean_df,
        aes(yintercept = mean_value),
        alpha = .25,
        size = 1
    ) +
    geom_vline(xintercept = 0,
               linetype = "dotted",
               alpha = .7) +
    geom_line(alpha = .7) +
    geom_point(shape = 21,
               color = "white",
               fill = "black") +
    # geom_smooth(formula = y ~ x * I(x<0)) +
    facet_wrap( ~ metric_cat, scales = "free_y", ncol = 1, drop = TRUE) +
    scale_y_continuous("Daily average (pre-exposure average)",
                       breaks = breaks_fx,
                       labels = labels_fx) +
    scale_x_continuous(
        "Days from suspected SARS-CoV-2 exposure",
        limits = c(-42, NA),
        breaks = seq(-42, 28, 7),
        expand = c(0, 1)
    ) +
    mk_nytimes(plot.caption = element_text(size = 8, color = "grey50")) + 
    labs(title = "Impact of COVID-19 on vital signs (N=1)",
         caption = "(Code: https://github.com/mkiang/covid_self)")

## NOTE: YOU HAVE TO RESET THE COUNTERS EVERY TIME YOU RENDER THE PLOT!
l_count <- 0
ggsave(
    here("covid_fig_01.pdf"),
    p1,
    width = 5,
    height = 8,
    scale = 1.1,
    device = cairo_pdf
)

l_count <- 0
ggsave(
    here("covid_fig_01.jpg"),
    p1,
    width = 5,
    height = 8,
    scale = 1.1,
    dpi = 600
)

## Display plot
l_count <- 0
p1


## Plot 2 ----
## Physical activity 
mean_df <- analytic_df %>%
    filter(
        metric %in% c(
            "average_met_minutes",
            "steps", 
            "high_activity_met_minutes",
            "total_calories"
        )
    ) %>% 
    filter(t_delta < 0) %>% 
    group_by(metric_cat) %>% 
    summarize(mean_value = mean(value))

sub_df <- analytic_df %>%
    filter(
        metric %in% c(
            "average_met_minutes",
            "steps", 
            "high_activity_met_minutes",
            "total_calories"
        )
    )

## Custom break and label functions ----
breaks_fx <- function(x) {
    ## Average activity
    if (between(min(x), .9, 1.5)) {
        c(1.5, 1.59, 2.0, 2.5)
        ## high intensity activity
    } else if (between(max(x), 1400, 1600)) {
        c(0, 337, 500, 1000, 1500)
        ## steps
    } else if (between(min(x), -1, 1)) {
        c(0, 5, 7.06, 10, 15, 20)
        ## energy expended
    } else if (between(max(x), 4500, 5500)) {
        c(3000, 3211, 4000, 5000)
    }
}

labels_fx <- function(x) {
    l_count <<- l_count + 1L
    switch(
        l_count,
        ## Average activity
        c("", "(1.6)", "2.0", "2.5"),
        ## high intensity activity
        c("0", "(337)", "", "1000", "1500"),
        ## steps
        c("0", "", "(7)", "10", "15", "20"),
        ## energy expended
        c("", "(3211)", "4000", "5000")
    )
}

## plot ----
p2 <- ggplot(sub_df,
             aes(x = t_delta,
                 y = value)) +
    geom_hline(
        data = mean_df,
        aes(yintercept = mean_value),
        alpha = .25,
        size = 1
    ) +
    geom_vline(xintercept = 0,
               linetype = "dotted",
               alpha = .7) +
    geom_line(alpha = .7) +
    geom_point(shape = 21,
               color = "white",
               fill = "black") +
    facet_wrap( ~ metric_cat, scales = "free_y", 
                ncol = 1, 
                drop = TRUE) +
    scale_y_continuous("Daily average (pre-exposure average)",
                       breaks = breaks_fx,
                       labels = labels_fx
                       ) +
    scale_x_continuous(
        "Days from suspected SARS-CoV-2 exposure",
        limits = c(-42, NA),
        breaks = seq(-42, 28, 7),
        expand = c(0, 1)
    ) +
    mk_nytimes(plot.caption = element_text(size = 8, color = "grey50")) + 
    labs(title = "Impact of COVID-19 on physical activity (N=1)",
         caption = "(Code: https://github.com/mkiang/covid_self)")

l_count <- 0
ggsave(
    here("covid_fig_02.pdf"),
    p2,
    width = 5,
    height = 8,
    scale = 1.1,
    device = cairo_pdf
)

l_count <- 0
ggsave(
    here("covid_fig_02.jpg"),
    p2,
    width = 5,
    height = 8,
    scale = 1.1,
    dpi = 600
)

l_count <- 0
p2
