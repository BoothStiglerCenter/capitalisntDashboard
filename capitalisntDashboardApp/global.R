
library(shiny)
library(shinythemes)
library(shiny)
library(shinythemes)
library(tidyverse)
library(reactable)
library(ggiraph)
library(ggrepel)
library(scales)
library(lubridate)
library(echarts4r)
library(reactable)
library(sparkline)

`%notin%` <- Negate(`%in%`)

local_files <- list.files(path='../', pattern='(.*)-\\d{4}\\-\\d{2}\\-\\d{2}\\.csv')
path_prepend = '../'
# local_files <- list.files(pattern = '(.*)-\\d{4}\\-\\d{2}\\-\\d{2}\\.csv')
# path_prepend = ''

discrete_palette <- c(
    "#7fc97f",
    "#beaed4",
    "#fdc086",
    "#ffff99",
    "#386cb0",
    "#f0027f",
    "#bf5b17",
    "#666666"
)

downloads_select_explainer <- "Click to select additional episodes to display. By default, the five most recent episodse are shown. Epsidoes are listed by release date with the most releases being shown first."

# Define server logic required to draw a histogram
#### DATA PROCESSING
for (file in local_files) {
    if (str_detect(file, "episodes_downloads")){
        downloads_path <- file
    } else if (str_detect(file, "podcast_listening_methods")) {
        podcast_platforms <- file
    } else if (str_detect(file, "episodes_listening_methods")) {
        episode_platforms <- file
    } else if (str_detect(file, "episodes_completion")) {
        completion_rate <- file
    }
}
downloads_data <- read_csv(paste0(path_prepend, downloads_path, sep = "")) %>%
        mutate(interval = as_date(interval, format="%Y-%m-%d"))%>%
        arrange(interval) %>%
        group_by(episode_id) %>%
        mutate(days_since_release=row_number(),
               cumulative_downloads = cumsum(downloads_total),
               release_date = min(interval),
               most_recent_date = max(interval),
               label_text = ifelse(interval == most_recent_date,
                                   paste(release_date, "--", substr(title, 1, 35)),
                                   NA),
               label_text = ifelse(str_length(label_text) > 45,
                                   paste0(label_text, "...", sep = ""),
                                   label_text)
               ) %>%
        ungroup() %>%
        arrange(desc(release_date)) %>%
        mutate(
            title = fct_relevel(title)
        ) %>%
        group_by(title)

episode_title_id <- downloads_data %>%
    select(episode_id, title, release_date) %>%
    distinct(episode_id, .keep_all = TRUE)



episode_titles <- downloads_data %>%
    distinct(release_date, .keep_all = TRUE) %>%
    arrange(desc(release_date)) %>%
    select(title) %>%
    pull()

default_selection <- downloads_data %>%
        ungroup() %>%
        distinct(release_date, .keep_all = TRUE) %>%
        slice_max(release_date, n=5) %>%
        select(title) %>%
        pull()

pod_platforms_data <- read_csv(paste0(path_prepend, podcast_platforms, sep="")) %>%
    mutate(
        categories = ifelse(rank < 8, "unique", "other"),
        name = ifelse(categories == "other", "Other", name),
        historical_pod_downloads_total = sum(downloads_total)
    ) %>%
    group_by(name) %>%
    mutate(
        downloads_total = ifelse(
            categories == "unique",
            downloads_total,
            sum(downloads_total)
        )
    ) %>%
    ungroup() %>%
    distinct(name, .keep_all = TRUE) %>%
    mutate(
        downloads_percent = downloads_total / historical_pod_downloads_total,
        stack_group = 1
    )
pod_platforms_data$color <- discrete_palette


ep_platforms_data <- read_csv(paste0(path_prepend, episode_platforms, sep="")) %>%
    mutate(
        categories = ifelse(
            name %notin% unique(pod_platforms_data$name),
            "other",
            'unique'
        )
    ) %>%
    group_by(episode_id) %>%
    arrange(desc(downloads_total)) %>%
    left_join(episode_title_id, by = "episode_id") %>%
    select(
        rank,
        name,
        title,
        episode_id,
        downloads_total,
        downloads_percent,
        categories,
        release_date
    ) %>%
    ungroup() %>%
    group_by(title) %>%
    mutate(
        episode_downloads_total = sum(downloads_total)
    ) %>%
    ungroup() %>%
    group_by(title, categories) %>%
    mutate(downloads_total = ifelse(
        categories == "unique",
        downloads_total,
        sum(downloads_total)
    )) %>%
    ungroup() %>%
    mutate(
        name = ifelse(categories == "other", "Other", name),
        downloads_percent = downloads_total / episode_downloads_total
    ) %>%
    distinct(episode_id, name, .keep_all = TRUE) %>%
    arrange(desc(downloads_total)) %>%
    group_by(episode_id) %>%
    mutate(rank = row_number())

platforms_caveat_text <- "Only present-moment, cross-sectional listening platform data is available. Time-series is only available with manual interval-timed data-collection."


completion_rate_data <- read_csv(paste0(path_prepend, completion_rate, sep = "")) %>%
    select(id, avg_completion, date_collected) %>%
    left_join(episode_title_id, by = c("id" = "episode_id")) %>%
    select(title, id, avg_completion, date_collected, release_date)