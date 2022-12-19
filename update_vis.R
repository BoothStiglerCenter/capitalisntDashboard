library(tidyverse)
library(lubridate)
library(scales)
library(ggtext)
library(zoo)

downloads_in <- read_csv('episodes_downloads-2022-11-11.csv')


downloads_data <- downloads_in %>%
    mutate(date = as_date(interval, format = "%m/%d/%Y")) %>%
    filter(title != "Capitalisn't Trailer") %>%
    group_by(episode_id) %>%
    arrange(date) %>%
    mutate(
        release_date = min(date),
        days_since_release = time_length(date - release_date, unit = "days"),
        cumulative_downloads = cumsum(downloads_total)
    ) %>%
    ungroup()


ggplot(downloads_data) +
    geom_line(
        aes(
            x = days_since_release,
            y = cumulative_downloads,
            color = release_date,
            group = episode_id
        ),
        size = 1.1
    ) +
    labs(title = "*Capitalisn't*: Cumulative downloads over time") +
    xlab("Days since release") +
    ylab("Cumulative downloads") +
    scale_y_continuous(label = scales::comma) +
    scale_color_viridis_c(
        breaks = c(
            as.numeric(min(downloads_data$release_date)),
            as.numeric(max(downloads_data$release_date))
        ),
        labels = c("Oldest", "Newest"),
        name = "Release Date"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_markdown()
    )

ggplot(downloads_data) +
    geom_line(
        aes(
            x = log(days_since_release + 1),
            y = cumulative_downloads,
            color = release_date,
            group = episode_id
        ),
        size = 1.1
    ) +
    labs(title = "*Capitalisn't*: Cumulative downloads over time") +
    xlab("Log(Days since release+1)") +
    ylab("Cumulative downloads") +
    scale_y_continuous(label = scales::comma) +
    scale_color_viridis_c(
        breaks = c(
            as.numeric(min(downloads_data$release_date)),
            as.numeric(max(downloads_data$release_date))
        ),
        labels = c("Oldest", "Newest"),
        name = "Release Date"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_markdown()
    )


ggplot(downloads_data %>%
    group_by(date) %>%
    mutate(podcast_downloads = sum(downloads_total)) %>%
    ungroup() %>%
    distinct(date, .keep_all = TRUE) %>%
    mutate(
        rolling_avg_downloads = rollmean(
            podcast_downloads,
            k = 14,
            fill = NA,
            align = "center"
        )
    )) +
    geom_line(
        aes(
            x = date,
            y = rolling_avg_downloads,
        ),
        size = 1.2
    ) +
    labs(
        title = "*Capitalisn't*: Rolling average (*t=14*) of downloads over time"
    ) +
    xlab("Date") +
    ylab("Downloads") +
    scale_y_continuous(label = scales::comma) +
    theme_minimal()+
    theme(
        plot.title = element_markdown()
    )

ggplot(downloads_data %>%
    select(-c(interval)) %>%
    group_by(episode_id) %>%
    filter(days_since_release %in% c(14, 28)) %>%
    pivot_wider(
        id_cols = c(episode_id, title, release_date),
        names_from = days_since_release,
        names_prefix = "downloads_t_",
        values_from = cumulative_downloads
    ) %>%
    select(
        episode_id,
        title,
        release_date,
        downloads_t_14,
        downloads_t_28
    )
    ) +
    geom_segment(
        aes(
            x = release_date,
            xend = release_date,
            y = downloads_t_14,
            yend = downloads_t_28
        ),
        color = "#B3B3B3"
    ) +
    geom_point(
        aes(
            x = release_date,
            y = downloads_t_14,
            color = "#8B0021"
        ),
        size = 2
    ) +
    geom_point(
        aes(
            x = release_date,
            y = downloads_t_28,
            color = "#007BA0"
        ),
        size = 2
    ) +
    labs(
        title = "*Capitalisn't*: Cumulative episode downloads at *t=14* and *t=28*"
    ) +
    xlab("Release Date") +
    ylab("Downloads") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(
        breaks = c("#8B0021", "#007BA0"),
        values = c("#8B0021", "#007BA0"),
        labels = c("14 days", "28 days"),
    ) +
    guides(
        color = guide_legend(
            title = "Days Since Release:"
        )
    ) +
    theme_minimal() +
    theme(
        plot.title = element_markdown(),
        legend.position = "top"
    )


ggplot(downloads_data %>%
    select(-c(interval)) %>%
    group_by(episode_id) %>%
    filter(days_since_release %in% c(14, 28)) %>%
    pivot_wider(
        id_cols = c(episode_id, title, release_date),
        names_from = days_since_release,
        names_prefix = "downloads_t_",
        values_from = cumulative_downloads
    ) %>%
    select(
        episode_id,
        title,
        release_date,
        downloads_t_14,
        downloads_t_28
    )
    ) +
    geom_segment(
        aes(
            y = release_date,
            yend = release_date,
            x = downloads_t_14,
            xend = downloads_t_28
        )
    ) +
    geom_point(
        aes(
            y = release_date,
            x = downloads_t_14,
            color = "#8B0021"
        ),
        size = 2
    ) +
    geom_point(
        aes(
            y = release_date,
            x = downloads_t_28,
            color = "#007BA0"
        ),
        size = 2
    ) +
    geom_label_repel()
    theme_minimal()
