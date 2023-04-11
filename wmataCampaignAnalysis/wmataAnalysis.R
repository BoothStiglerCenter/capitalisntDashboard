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
    left_join(
        release_dates_df,
        by = "episode_id",
        multiple = "all"
    ) %>%
    group_by(date) %>%
    mutate(
        gap_to_release = as.integer(
            interval(start = date, end = release_date)
        )
    ) %>%
    arrange(date, desc(gap_to_release)) %>%
    mutate(
        ordered_from_closest_release = row_number(),
        active_back_expired_catalog = case_when(
            ordered_from_closest_release == 1 ~ 1,
            (ordered_from_closest_release >= 2) & (ordered_from_closest_release <= 6) ~ 2,
            ordered_from_closest_release >= 7 ~ 3
        )
    ) %>%
    select(
        date, daily_downloads, active_back_expired_catalog
    ) %>%
    group_by(date) %>%
    mutate(
        all_daily_downloads = sum(daily_downloads),
    ) %>%
    group_by(date, active_back_expired_catalog) %>%
    mutate(
        grouped_daily_downloads = sum(daily_downloads)
    ) %>%
    distinct(
        date, active_back_expired_catalog,
        all_daily_downloads, grouped_daily_downloads
    ) %>%
    arrange(active_back_expired_catalog, date) %>%
    group_by(active_back_expired_catalog) %>%
    mutate(
        avg_grouped_daily_downloads_14 = rollmean(
            grouped_daily_downloads,
            k = 14,
            fill = NA,
            align = "right"
        ),
        avg_all_daily_downloads_14 = rollmean(
            all_daily_downloads,
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


recent_podcast_moving_avg_decomp <- ggplot(
    podcast_daily_downloads_df %>%
    pivot_longer(
        cols = -c("date", "active_back_expired_catalog"),
        names_to = "download_type",
        values_to = "value"
    ) %>%
    filter(
        download_type %in% c(
            "avg_all_daily_downloads_14"
        ),
        date >= ymd("2022-10-01")
    )
) +
    ad_period_shade_geom +
    geom_area(
        aes(
            x = date,
            y = value,
            fill = as.factor(active_back_expired_catalog),
            group = active_back_expired_catalog
        ),
        position = "stack",
        alpha = 1
    ) +
    scale_fill_stigler(
        breaks = c(1, 2, 3),
        labels = c("Most-recent", "Next Five", "Older"),
        name = ""
    ) +
    scale_y_continuous(
        labels = scales::comma,
        expand = expand_scale(mult = c(0, 0)),
        position = "right"
    ) +
    scale_x_date(
        breaks = date_breaks("1 month"),
        labels = label_date_short(format = c("%y", "%b"))
    ) +
    labs(
        title = "**Capitalisn't: Composition of daily-downloads moving average**", 
        subtitle = "14-day leading average, since Oct. 1st, 2022",
        tag = "Figure 2A"
    ) +
    theme_stigler()
recent_podcast_moving_avg_decomp

alltime_podcast_moving_avg_decomp <- ggplot(
    podcast_daily_downloads_df %>%
    pivot_longer(
        cols = -c("date", "active_back_expired_catalog"),
        names_to = "download_type",
        values_to = "value"
    ) %>%
    filter(
        download_type %in% c(
            "avg_all_daily_downloads_14"
        )
    )
) +
    ad_period_shade_geom +
    geom_area(
        aes(
            x = date,
            y = value,
            fill = as.factor(active_back_expired_catalog),
            group = active_back_expired_catalog
        ),
        position = "stack",
        alpha = 1
    ) +
    scale_fill_stigler(
        breaks = c(1, 2, 3),
        labels = c("Most-recent", "Next Five", "Older"),
        name = ""
    ) +
    scale_y_continuous(
        labels = scales::comma,
        expand = expand_scale(mult = c(0, 0)),
        position = "right"
    ) +
    scale_x_date(
        breaks = date_breaks("3 month"),
        labels = label_date_short(format = c("%y", "%b"))
    ) +
    labs(
        title = "**Capitalisn't: Composition of daily-downloads moving average**", 
        subtitle = "14-day leading average, all time",
        tag = "Figure 2B"
    ) +
    theme_stigler()
alltime_podcast_moving_avg_decomp




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
            color = as.numeric(release_date)
        ),
        size = 1.5
    ) +
    scale_color_stigler(
        "blues_2",
        reverse = TRUE,
        discrete = FALSE,
        name = "",
        guide = "none"
    ) +
    scale_y_continuous(
        labels = scales::comma,
        position = "right",
        expand = expand_scale(mult = c(0, 0)),
        limits = c(0, 25000)
    ) +
    labs(
        title = "**Capitalisn't: Cumulative daily downloads**",
        subtitle = "20 most recent episodes",
        tag = "Figure 3A"
    ) +
    theme_stigler()
recent_20_episodes_cumul_perf

alltime_episodes_cumul_perf <- ggplot(
    daily_downloads_df %>%
        mutate(release_date = as.numeric(release_date)) %>%
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
    scale_color_stigler(
        "blues_2",
        reverse = TRUE,
        discrete = FALSE,
        guide = "none"
    ) +
    scale_y_continuous(
        labels = scales::comma,
        position = "right",
        expand = expand_scale(mult = c(0, 0)),
        limits = c(0, 30000)
    ) +
    # ggtitle("test") +
    labs(
        title = "**Capitalisn't: Cumulative daily downloads**",
        subtitle = "20 most recent episodes",
        tag = "Figure 3B"
    ) +
    theme_stigler()
alltime_episodes_cumul_perf

recent_20_1142842_day_cumul_perf <- ggplot(
    daily_downloads_df %>%
    filter(
        episode_id %in% recent_n_episode_ids(
            n = 20,
            release_dates_df
        ),
        days_since_release %in% c(1, 14, 28, 42)
    )
) +
    ad_period_shade_geom +
    geom_segment(
        data = daily_downloads_df %>%
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
        ),
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
        breaks = date_breaks("1 month")
    ) +
    scale_y_continuous(
        labels = scales::comma,
        position = "right",
        limits = c(0, 20000),
        expand = expand_scale(mult = c(0, 0))
    ) +
    labs(
        title = "**Capitlisnt't downloads *t* days after release**",
        tag = "Figure 1B"
    ) +
    theme_stigler()
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
        tag = "Figure 1A"
    ) +
    theme_stigler()
all_1142842_day_cumul_perf
