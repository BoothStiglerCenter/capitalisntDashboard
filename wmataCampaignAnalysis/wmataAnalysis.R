#### PREAMBLE ####
library(tidyverse)
library(lubridate)
library(scales)
library(ggtext)
`%notin%` <- Negate(`%in%`)


today_dt <- today()
# today_format <- 


#### READING DATA IN ####
dmv_data_in <- read_csv(
    "wmataCampaignAnalysis/states_cities_DMV.csv"
)

episodes_core_data_in <- read_csv(
    paste(
        "episodes_downloads-", today_dt, ".csv",
        sep = ""
    )
)

episode_locations_data_in <- read_csv(
    paste(
        "us_cities_episode_locations-", today_dt, ".csv",
        sep = ""
    )
)



#### REFERENCE FUNCTIONS ####
recent_n_episode_ids <- function(
    n = 0,
    release_dates_df
) {
    episode_ids <- release_dates_df %>%
        dplyr::arrange(
            desc(
                release_date
            )
        ) %>%
        head(n) %>%
        dplyr::pull(episode_id)


    return(episode_ids)
}

#### REFERENCES DFs  ####
######## Reference info:
######## Digital (train platforms) : 01/19/2023 -- 02/15/2023
######## Static (train cars): 01/16/2023 -- 02/12/2023

titles_ids_df <- episodes_core_data_in %>%
    select(episode_id, title) %>%
    distinct() %>%
    view()

release_dates_df <- episodes_core_data_in %>%
    select(episode_id, interval) %>%
    mutate(interval = as.Date(interval)) %>%
    group_by(episode_id) %>%
    summarise(
        release_date = min(interval)
    )

campaign_dates_df <- episodes_core_data_in %>%
    select(interval) %>%
    rename(
        "date" = "interval"
    ) %>%
    distinct() %>%
    mutate(
        date = as.Date(date),
        any_ad_strict = ifelse(
            date %within% interval(ymd("20230116"), ymd("20230215")),
            1, 0
        ),
        digital_ad_strict = ifelse(
            date %within% interval(ymd("20230119"), ymd("20230215")),
            1, 0
        ),
        static_ad_strict = ifelse(
            date %within% interval(ymd("20230116"), ymd("20230212")),
            1, 0
        ),
        static_ad_lax = ifelse(
            date %within% interval(ymd("20230116"), today_dt),
            1, 0
        )
    )



dmv_cities_df <- dmv_data_in %>%
    filter(
        state_id %in% c(
            4138106, # DC
            6254928, # VA
            4361885 # MD
        )
    ) %>%
    rename(
        "city_name" = "name",
        "city_id" = "id",
        "relevant_msa" = "within relevant MSA"
    ) %>%
    select(
        city_name, city_id, state_id, relevant_msa
    ) %>%
    view()


### CONTENT-FUL DATA
daily_downloads_df <- episodes_core_data_in %>%
    rename(
        "date" = "interval",
        "daily_downloads" = "downloads_total"
    ) %>%
    left_join(
        release_dates_df,
        by = "episode_id",
        multiple = "all"
    ) %>%
    group_by(episode_id) %>%
    arrange(date) %>%
    mutate(
        days_since_release = row_number(),
        cumulative_downloads = cumsum(daily_downloads)
    ) %>%
    view()





### PLOTS

recent_15_episodes_cumul_perf <- ggplot(
    daily_downloads_df %>%
        filter(
            episode_id %in% recent_n_episode_ids(
                n = 15,
                release_dates_df
            )
        )
) +
    geom_line(
        aes(
            x = days_since_release,
            y = cumulative_downloads,
            group = episode_id,
            color = release_date
        ),
        size = 1.5
    ) +
    scale_color_viridis_c() +
    theme_minimal()
recent_15_episodes_cumul_perf

recent_15_episodes_daily_perf <- ggplot(
    daily_downloads_df %>%
        filter(
            episode_id %in% recent_n_episode_ids(
                n = 15,
                release_dates_df
            )
        )
) +
    geom_line(
        aes(
            x = date,
            y = daily_downloads,
            group = episode_id,
            color = release_date
        ),
        size = 1.5
    ) +
    scale_color_viridis_c() +
    theme_minimal()
recent_15_episodes_daily_perf

recent_15_142842_day_cumul_perf <- ggplot(
    daily_downloads_df %>%
    filter(
        episode_id %in% recent_n_episode_ids(
            n = 15,
            release_dates_df
        ),
        days_since_release %in% c(14, 28, 42)
    ) %>%
    pivot_wider(
        id_cols = c(episode_id, release_date),
        names_from  = days_since_release,
        values_from = cumulative_downloads,
        names_prefix = "downloads_t_"
    )
) +
    geom_segment(
        aes(
            x = release_date,
            xend = release_date,
            y = downloads_t_14,
            yend = downloads_t_42,
            group = episode_id
        ),
        color = "grey",
        size = 1.25
    ) +
    geom_point(
        aes(
            x = release_date,
            y = downloads_t_14,
        ),
        color = "red"
    ) +
    geom_point(
        aes(
            x = release_date,
            y = downloads_t_28,
        ),
        color = "green"
    ) +
    geom_point(
        aes(
            x = release_date,
            y = downloads_t_42,
        ),
        color = "blue"
    ) +
    theme_minimal()

recent_15_142842_day_cumul_perf
