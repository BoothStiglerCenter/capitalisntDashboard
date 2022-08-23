library(tidyverse)
library(lubridate)
library(scales)
library(ggtext)

downloads_in <- read_csv('episodes_downloads-2022-08-23.csv')


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
