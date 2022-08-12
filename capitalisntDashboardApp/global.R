library(shiny)
library(shinythemes)
library(tidyverse)
library(reactable)
library(scales)
library(lubridate)
library(echarts4r)
library(reactable)
library(countrycode)
library(rdrop2)
library(cyphr)
library(sodium)
library(markdown)
library(sparkline)


`%notin%` <- Negate(`%in%`)

##### For running Shiny app locally (runApp())
# local_files <- list.files(path='../', pattern='(.*)-\\d{4}\\-\\d{2}\\-\\d{2}\\.csv')
# path_prepend = "../"

##### For running code chunks in terminal locally
# local_files <- list.files(pattern = '(.*)-\\d{4}\\-\\d{2}\\-\\d{2}\\.csv')
# path_prepend = ''


##### For running with the dropbox data
drop_token <- readRDS("drop_token_rds_decrypt.rds")
# path_prepend <- "capitalisntDashboardData/"
path_prepend <- ''
dropbox_files <- drop_dir("capitalisntDashboardData", dtoken = drop_token) %>%
    select(path_lower) %>%
    pull()
print(dropbox_files)


for (dfile in dropbox_files) {
    drop_download(
        dfile,
        overwrite = TRUE,
        dtoken = drop_token
    )
}
print(list.files())
local_files <- list.files()




#### File organization for local/hosted development. Never comment out
print(local_files)
for (file in local_files) {
    if (str_detect(file, "episodes_downloads")) {
        downloads_path <- file
        print(paste("downloads_path path is: ", downloads_path))
    } else if (str_detect(file, "podcast_listening_methods")) {
        podcast_platforms <- file
        print(paste("podcast_platforms path is: ", podcast_platforms))
    } else if (str_detect(file, "episodes_listening_methods")) {
        episode_platforms <- file
        print(paste("episode_platforms path is: ", episode_platforms))
    } else if (str_detect(file, "^podcast_locations")) {
        podcast_locations <- file
        print(paste("podcast_locations path is: ", podcast_locations))
    } else if (str_detect(file, "^episodes_locations")) {
        episode_locations <- file
        print(paste("episode_locations path is: ", episode_locations))
    } else if (str_detect(file, "is_isnt_completion_rates")) {
        is_isnt_path <- file
    }
}



################################################
#### DOWNLOADS TAB ##### DATA ####
################################################
#### Also generated Calendar data ####

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

downloads_select_explainer <- "Click to select additional episodes to display. Clicking on white=space allows for search by typing. Highlighted cards/episodes be deleted ot remove them from the plot. By default, the five most recent episodse are shown. Epsidoes are listed by release date with the most releases being shown first."

downloads_data <- read_csv(paste0(path_prepend, downloads_path, sep='')) %>%
        mutate(interval = as_date(interval, format='%Y-%m-%d'))%>%
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
                                   label_text),
                title = case_when(
                    title == "Is Inflation The Fed’s Fault? + Uber Leaks" ~ "Is Inflation The Fed's Fault? + Uber Leaks",
                    TRUE ~ title
                )
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

################################################
#### PLATFORMS TAB ##### DATA ####
################################################

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
            "unique"
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



################################################
#### GEOGRAPHY/MAPS TAB ##### DATA ####
################################################

### echarts4r likes to use 'country.name.en' (see countrycode())

podcast_locations_data <- read_csv(paste0(path_prepend, podcast_locations, sep=""))

country_names <- read_csv(paste0(path_prepend, 'simplecast_countries.csv')) %>%
    rename("name" = "simplecast_countries")

podcast_locations_data <- podcast_locations_data %>%
    left_join(country_names, by = "name")




platforms_caveat_text <- "Only present-moment, cross-sectional listening platform data is available. Time-series is only available with manual interval-timed data-collection."


completion_rate_data <- read_csv(paste0(path_prepend, is_isnt_path, sep = "")) %>%
    select("Episode Title", "Average Consumption", "Total Length", "When Is/Isn't?", "Release Date") %>%
    rename(
        "title" = "Episode Title",
        "avg_completion" = "Average Consumption",
        "is_isnt" = "When Is/Isn't?",
        "srun_time" = "Total Length",
    ) %>%
    mutate(
        `Release Date` = as.Date(`Release Date`, "%m/%d/%Y"),
        is_isnt = ifelse(is_isnt == "N/A", 0, is_isnt),
        is_isnt = as.numeric(is_isnt))

print("REACHED THE BOTTOM OF `global.R`")
