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

    testReactive <- reactive({
        downloads_data %>%
            filter(title %in% input$episodeSelectize)
        # input$episodeSelectize
        # print(input$episodeSelectize)
    })



   ## Cumulative downloads chart
    output$downloadsPlot <- renderEcharts4r({
        # downloads_data %>%
        #     filter(title %in% input$Selectize) %>%
        testReactive() %>%
            # downloads_data %>%
            e_charts(x = days_since_release, dispose = TRUE) %>%
            e_line(serie = cumulative_downloads, symbol = "none") %>%
            e_tooltip(trigger = "axis") %>%
            e_legend(show = FALSE) %>%
            e_x_axis(name = "Days since release", nameLocation = "middle") %>%
            e_y_axis(name = "Cumulative downloads") %>%
            e_datazoom() %>%
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
            arrange(downloads_t_14) %>%
            fill(downloads_t_14, downloads_to_date, .direction = "downup") %>%
            distinct(title, .keep_all = TRUE) %>%
            select(title, downloads_to_date, downloads_t_14, release_date) %>%
            reactable(
                columns = list(
                    downloads_to_date = colDef(
                        format = colFormat(separators = TRUE)
                        ),
                    downloads_t_14 = colDef(
                        format = colFormat(separators = TRUE)
                        )
                )
            )
    })

    output$selectedEpisodesVerbatim <- renderText({
        input$episodeSelectize
    })

    output$platformShareBar <- renderEcharts4r({
        pod_platforms_data %>%
            arrange(rank) %>%
            e_chart(x = name) %>%
            e_bar(serie = downloads_total) %>%
            e_legend(show = FALSE) %>%
            e_show_loading() %>%
            e_tooltip()
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
                stack = "stack", name = 'Apple Podcasts', color = "#7fc97f",) %>%
            e_bar(serie = `Spotify`,
                stack = "stack", name = 'Spotify', color = "#beaed4") %>%
            e_bar(serie = `Overcast`,
                stack = "stack", name = 'Overcast', color = "#fdc086") %>%
            e_bar(serie = `Podcast & Radio Addict`,
                stack = "stack", name = 'Podcast & Radio Addict', color = "#ffff99") %>%
            e_bar(serie = `Simplecast`,
                stack = "stack", name = 'Simplecast', color = "#386cb0") %>%
            e_bar(serie = `Pocket Casts`,
                stack = "stack", name = 'Pocket Casts', color = "#f0027f") %>%
            e_bar(serie = `Google Podcasts`,
                stack = "stack", name = 'Google Podcasts', color = "#bf5b17") %>%
            e_bar(serie = `Other`,
                stack = "stack", name = 'Other', color = "#666666") %>%
            e_legend(show = TRUE) %>%
            e_datazoom() %>%
            e_show_loading() %>%
            e_tooltip()
    })

    ep_platforms_data_plat_selector <- reactive({
        # print('in episode selector')
        # Select only the "clicked bar".
        # If thing has been clicked yet, return all the bars
        if (is.null(input$platformShareBar_clicked_data)) {
            unique(ep_platforms_data$name)
        } else {
            str_match(input$platformShareBar_clicked_data[1], '"(.*?)"')[2]
        }
    })



})
