library(tidyverse)
library(lubridate)

total_daily_downloads <- read_csv('episodes_downloads-2023-02-27.csv') %>%
view()

wmata_campaign_period <- interval(ymd('2023-01-16'), ymd('2023-02-15'))

df <- total_daily_downloads %>%
    group_by(episode_id) %>%
    arrange(interval, .by_group = TRUE) %>%
    mutate(
        release_date = min(interval),
        days_since_release = row_number()
    ) %>%
    filter(
        release_date > as.Date("2022-10-01")
    ) %>%
    mutate(
        cumulative_downloads = cumsum(downloads_total),
        pre_during_post_campaign = case_when(
            interval < ymd("2023-01-16") ~ 0,
            interval %within% wmata_campaign_period ~ 1,
            interval > ymd("2023-02-15") ~ 2
        ),
        pre_during_post_campaign = as.factor(pre_during_post_campaign)
    ) %>%
    view()


ggplot(df) +
    geom_line(
        aes(
            x = interval,
            y = cumulative_downloads,
            color = pre_during_post_campaign,
            group = episode_id
        ),
        size = 1.25
    ) +
    scale_color_discrete(
        name = "Time Period",
        breaks = c(0,1,2),
        labels = c("Pre", "During", "Post") 
    ) +
    theme_minimal()

ggplot(df) +
    geom_line(
        aes(
            x = interval,
            y = downloads_total,
            color = pre_during_post_campaign,
            group = episode_id
        ),
        size = 1.25
    ) + 
    scale_color_discrete(
        name = "Time Period",
        breaks = c(0,1,2),
        labels = c("Pre", "During", "Post") 
    ) +
    theme_minimal()
