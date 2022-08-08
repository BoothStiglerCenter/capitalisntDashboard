#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

local_files <- list.files(path='../', pattern='(.*)-\\d{4}\\-\\d{2}\\-\\d{2}\\.csv')
# Define server logic required to draw a histogram
#### DATA PROCESSING

shinyServer(function(input, output) {
    # downloads_path <- ifelse(str_match(local_files, 'episodes_downloads'), 1, NA)
   
   
   ################################################
   #### DOWNLOADS TAB #####
   ################################################

    selectEpisodesReactive <- reactive({
        downloads_data %>%
            filter(title %in% input$episodeSelectize)
    })



   ## Cumulative downloads chart
    output$downloadsPlot <- renderEcharts4r({
        selectEpisodesReactive() %>%
            e_charts(x = days_since_release, dispose = TRUE) %>%
            e_line(serie = cumulative_downloads, symbol = "none") %>%
            e_tooltip(trigger = "axis") %>%
            e_legend(show = FALSE) %>%
            e_x_axis(name = "Days since release", nameLocation = "middle") %>%
            e_y_axis(name = "Cumulative downloads") %>%
            e_datazoom(
                end = 50,
                minValueSpan = 14,
            ) %>%
            e_show_loading()
    })

    output$episodeDownloadsTable <- renderReactable({
        downloads_data %>%
            filter(title %in% input$episodeSelectize) %>%
            group_by(title) %>%
            mutate(
                downloads_to_date = ifelse(
                    most_recent_date == interval,
                    cumulative_downloads,
                    NA),
                downloads_t_14 = ifelse(
                    days_since_release == 14,
                    cumulative_downloads,
                    NA
                )
            ) %>%
            left_join(completion_rate_data, by = "title") %>%
            arrange(downloads_t_14) %>%
            fill(downloads_t_14, downloads_to_date, .direction = "downup") %>%
            distinct(title, .keep_all = TRUE) %>%
            select(
                release_date,
                title,
                downloads_to_date,
                downloads_t_14,
                avg_completion,
                is_isnt
            ) %>%
            # This mutate needs to be modified with actual completion-rate data
            mutate(completion_bullet_range = list(c(is_isnt, avg_completion, 1))) %>%
            reactable(
                defaultSorted = "release_date",
                defaultSortOrder = "desc",
                columns = list(
                    release_date = colDef(
                        name = "Release Date"
                    ),
                    title = colDef(
                        name = "Title"
                    ),
                    downloads_to_date = colDef(
                        name =  "Downloads to Date",
                        format = colFormat(separators = TRUE)
                    ),
                    downloads_t_14 = colDef(
                        name = "Downloads (t=14)",
                        format = colFormat(separators = TRUE)
                    ),
                    avg_completion = colDef(show = FALSE),
                    completion_bullet_range = colDef(
                        name = "Completion rate:",
                        ### This function needs to be modified with actual completion-rate data.
                        ### See https://omnipotent.net/jquery.sparkline/#tooltips for documentation
                        cell = function(comp_value) {
                            sparkline(
                                comp_value,
                                type = "bullet",
                                width = "300px",
                                disableTooltips = "true"
                            )
                        }
                    )
                )
            )
    })

    output$selectedEpisodesVerbatim <- renderText({
        input$episodeSelectize
    })

    output$platformShareBar <- renderEcharts4r({
        pod_platforms_data %>%
            e_chart(x = name) %>%
            e_bar(serie = downloads_total, colorBy ='series') %>%
            e_legend(show = FALSE) %>%
            e_show_loading() %>%
            e_tooltip() %>%
            e_add('itemStyle', color)
    })

    output$episodePlatforms <- renderEcharts4r({
        ep_platforms_data %>%
            pivot_wider(
                id_cols = c(episode_id, title, release_date),
                names_from = "name",
                values_from = "downloads_total"
            ) %>% 
            arrange(release_date) %>%
            e_chart(x = title, dispose = FALSE) %>%
            e_bar(serie = `Apple Podcasts`,
                stack = "stack", name = 'Apple Podcasts') %>%
            e_bar(serie = `Spotify`,
                stack = "stack", name = 'Spotify') %>%
            e_bar(serie = `Overcast`,
                stack = "stack", name = 'Overcast') %>%
            e_bar(serie = `Podcast & Radio Addict`,
                stack = "stack", name = 'Podcast & Radio Addict') %>%
            e_bar(serie = `Simplecast`,
                stack = "stack", name = 'Simplecast') %>%
            e_bar(serie = `Pocket Casts`,
                stack = "stack", name = 'Pocket Casts') %>%
            e_bar(serie = `Google Podcasts`,
                stack = "stack", name = 'Google Podcasts') %>%
            e_bar(serie = `Other`,
                stack = "stack", name = 'Other') %>%
            e_legend(show = TRUE) %>%
            e_datazoom() %>%
            e_show_loading() %>%
            e_tooltip() %>%
            e_color(discrete_palette)
    })



    output$calendarPlot <- renderEcharts4r({
        downloads_data %>%
            mutate(year = format(interval, '%Y')) %>%
            filter(year %in% c("2020", "2021", "2022")) %>%
            group_by(interval) %>%
            mutate(total_daily_downloads = sum(downloads_total)) %>%
            arrange(desc(total_daily_downloads)) %>%
            select(interval, total_daily_downloads, year) %>%
            distinct(interval, .keep_all = TRUE) %>%
            ungroup() %>%
            group_by(year) %>%
            e_charts(interval) %>%
            e_calendar(range = "2020", top = "40") %>%
            e_calendar(range = "2021", top = "220") %>%
            e_calendar(range = "2022", top = "400") %>%
            e_heatmap(total_daily_downloads, coord_system = "calendar") %>%
            e_visual_map(total_daily_downloads, calculable = TRUE) %>%
            e_tooltip(trigger = "item")
    })

    calendarDateClicked <- reactive({
        # print('selecting a day')
        if (is.null(input$calendarPlot_clicked_data)){
            # print('today is')
            today <- today()
            most_recent_sunday <- floor_date(today, "week")
            if (wday(today) == 4) {
                # print('its a thursday!')
                today
            } else if (today - most_recent_sunday > 0){
            # We are between Sunday and Thursday
                # print('here1')
                most_recent_thursday <- most_recent_sunday - days(4)
                most_recent_thursday
            } else {
                # We are between Friday and Sunday (inclusive)
                for (i in 1:3){
                    # print(wday(today) - days(1))
                    if (wday(today - days(i)) == 4){
                        today
                    }
                }
            }
        } else {
            str_match(input$calendarPlot_clicked_data[1], '"(\\d{4}-\\d{2}-\\d{2})"')[2]
        }
    })

    output$calendarDateTopEps <- renderReactable({

        # print(class(calendarDateClicked()))
        downloads_data %>%
            filter(interval == calendarDateClicked()) %>%
            select(title, downloads_total) %>%
            arrange(desc(downloads_total)) %>%
            ungroup() %>%
            mutate(rank = row_number()) %>%
            reactable(
                columns = list(
                    title = colDef(name = "Title"),
                    rank = colDef(name = "Rank"),
                    downloads_total = colDef(
                        name = "Daily Downloads",
                        format = colFormat(separators = TRUE)
                    )
                )
            )
    })

    ep_platforms_data_plat_selector <- reactive({
        # Select only the "clicked bar".
        # If thing has been clicked yet, return all the bars
        if (is.null(input$platformShareBar_clicked_data)) {
            unique(ep_platforms_data$name)
        } else {
            str_match(input$platformShareBar_clicked_data[1], '"(.*?)"')[2]
        }
    })

    output$completionRatePlot <- renderPlot({
        ggplot(completion_rate_data %>%
            rename('release_date' = "Release Date")) +
            geom_segment(
                aes(
                    x = 0,
                    xend = avg_completion,
                    y = release_date,
                    yend = release_date
                ),
                size = 4,
                alpha = 1,
                color = "#6e6eaf"
            ) +
            geom_segment(
                aes(
                    x = 0,
                    xend = 1,
                    y = release_date,
                    yend = release_date
                ),
                size = 4,
                alpha = 0.5,
                color = "#4444af"
            ) +
            geom_point(
                aes(x = avg_completion, y = release_date),
                color = "orange",
                size = 4) +
            geom_point(
                aes(x = is_isnt, y = release_date),
                color = "red",
                shape = 15,
                size = 4
            ) +
            geom_label(
                aes(
                    x = -0.01,
                    y = release_date,
                    label = title,
                ),
                hjust = 1
            ) +
            xlim(c(-0.25, 1)) +
            # ggtitle("Share of Episode Completed") +
            theme_minimal()
    })



})
