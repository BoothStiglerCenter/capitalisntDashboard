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
            view() %>%
            e_chart(x = title) %>%
            e_bar(serie = `Apple Podcasts`, stack = "grp", color = "#7fc97f") %>%
            e_bar(serie = `Spotify`, stack = "grp", color = "#beaed4") %>%
            e_bar(serie = `Overcast`, stack = "grp", color = "#fdc086") %>%
            e_bar(serie = `Podcast & Radio Addict`, stack = "grp", color = "#ffff99") %>%
            e_bar(serie = `Simplecast`, stack = "grp", color = "#386cb0") %>%
            e_bar(serie = `Pocket Casts`, stack = "grp", color = "#f0027f") %>%
            e_bar(serie = `Google Podcasts`, stack = "grp", color = "#bf5b17") %>%
            e_bar(serie = `Other`, stack = "grp", color = "#666666") %>%
            e_legend(show = FALSE) %>%
            e_datazoom() %>%
            e_tooltip()
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

    calendarDayClicked <- reactive({
        if (is.null(input$calendarPlot_clicked_data)){
            print('need to return latest thursday')
        } else {
            print(input$calendarPlot_clicked_data)
        }
        # print(input$calendarPlot_clicked_data)
        # input$calendarPlot_clicked_data
        'title'
    })

    output$calendarDayTopEps <- renderReactable({
        downloads_data %>%
            head(5) %>%
            select(calendarDayClicked()) %>%
            reactable()
    })

})
