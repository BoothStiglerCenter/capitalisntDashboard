#### PREAMBLE ####
library(tidyverse)
library(lubridate)
library(scales)
library(ggtext)
library(zoo)
source("theme_materials/theme_stigler.R")
`%notin%` <- Negate(`%in%`)
today_dt <- today()

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
            dplyr::desc(release_date)
        ) %>%
        head(n) %>%
        dplyr::pull(episode_id)

    return(episode_ids)
}

released_since_episode_ids <- function(
    cutoff_date_str,
    release_dates_df
) {
    episode_ids <- release_dates_df %>%
        dplyr::arrange(
            dplyr::desc(release_date)
        ) %>%
        dplyr::filter(
            release_date >= lubridate::ymd(cutoff_date_str)
        ) %>%
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

#### CONTENT-FUL DATA ####
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

podcast_daily_downloads_df <- episodes_core_data_in %>%
    rename(
        "date" = "interval",
        "daily_downloads" = "downloads_total"
    ) %>%
    select(
        date, daily_downloads
    ) %>%
    group_by(date) %>%
    mutate(
        daily_downloads = sum(daily_downloads),
    ) %>%
    distinct() %>%
    ungroup() %>%
    arrange(date) %>%
    mutate(
        cumulative_downloads = cumsum(daily_downloads),
        avg_downloads_14 = rollmean(
            daily_downloads,
            k = 14,
            fill = NA,
            align = "right"
        )
    )

#### PLOTS ####
ad_period_shade_geom <- geom_rect(
    aes(
        xmin = ymd("2023-01-16"),
        xmax = ymd("2023-02-15"),
        ymin = 0,
        ymax = Inf,
    ),
    fill = "grey",
    alpha = 0.05
)

recent_podcast_daily_perf <- ggplot(
    podcast_daily_downloads_df %>%
    pivot_longer(
        cols = -c("date"),
        names_to = "download_type",
        values_to = "value"
    ) %>%
    filter(
        download_type %in% c("avg_downloads_14"),
        date >= ymd("2022-10-01")
    )
) +
    ad_period_shade_geom +
    geom_line(
        aes(
            x = date,
            y = value
        ),
        size = 1.5
    ) +
    theme_minimal()
recent_podcast_daily_perf

all_podcast_daily_perf <- ggplot(
    podcast_daily_downloads_df %>%
    pivot_longer(
        cols = -c("date"),
        names_to = "download_type",
        values_to = "value"
    ) %>%
    filter(
        download_type %in% c("avg_downloads_14")
    )
) +
    ad_period_shade_geom +
    geom_line(
        aes(
            x = date,
            y = value
        ),
        size = 1.5
    ) +
    theme_minimal()
all_podcast_daily_perf

recent_20_episodes_cumul_perf <- ggplot(
    daily_downloads_df %>%
        filter(
            episode_id %in% recent_n_episode_ids(
                n = 20,
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
recent_20_episodes_cumul_perf

recent_20_episodes_daily_perf <- ggplot(
    daily_downloads_df %>%
        filter(
            episode_id %in% recent_n_episode_ids(
                n = 20,
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
recent_20_episodes_daily_perf

recent_20_1142842_day_cumul_perf <- ggplot(
    daily_downloads_df %>%
    filter(
        episode_id %in% recent_n_episode_ids(
            n = 20,
            release_dates_df
        ),
        days_since_release %in% c(1, 14, 28, 42)
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
            y = downloads_t_1,
            yend = downloads_t_42,
            group = episode_id
        ),
        color = "grey",
        size = 1.25
    ) +
    geom_point(
        aes(
            x = release_date,
            y = downloads_t_1,
        ),
        size = 2,
        color = "#4D0000"
    ) +
    geom_point(
        aes(
            x = release_date,
            y = downloads_t_14,
        ),
        size = 2,
        color = "#800000"
    ) +
    geom_point(
        aes(
            x = release_date,
            y = downloads_t_28,
        ),
        size = 2,
        color = "#EA6A51"
    ) +
    geom_point(
        aes(
            x = release_date,
            y = downloads_t_42,
        ),
        size = 2,
        color = "#115E67"
    ) +
    ylim(c(0, max(daily_downloads_df$cumulative_downloads))) +

    theme_minimal()
recent_20_1142842_day_cumul_perf


all_1142842_day_cumul_perf <- ggplot(
    daily_downloads_df %>%
    filter(
        days_since_release %in% c(1, 14, 28, 42)
    )
) +
    geom_segment(
        data = daily_downloads_df %>%
            filter(
                days_since_release %in% c(1, 42)
            ) %>%
            pivot_wider(
                id_cols = c(episode_id, release_date),
                names_from  = days_since_release,
                values_from = cumulative_downloads,
                names_prefix = "downloads_t_"
            ),
        aes(
            x = release_date,
            xend = release_date,
            y = downloads_t_1,
            yend = downloads_t_42,
            group = episode_id
        ),
        color = "grey",
        size = 1
    ) +
    geom_point(
        aes(
            x = release_date,
            y = cumulative_downloads,
            group = episode_id,
            color = as.factor(days_since_release)
        ),
        size = 1.5
    ) +
    scale_color_stigler(
        name = "Days after release *t*: "
    ) +
    scale_x_date(
        labels = label_date_short(format = c("%y", "%b")),
        breaks = date_breaks("3 month")
    ) +
    scale_y_continuous(
        labels = scales::comma,
        position = "right",
        limits = c(0, 20000),
        expand = expand_scale(mult = c(0, 0))
    ) +
    labs(
        title = "**Capitlisnt't downloads *t* days after release**",
        tag = "Figure 1"
    ) +
    theme_stigler()
all_1142842_day_cumul_perf
