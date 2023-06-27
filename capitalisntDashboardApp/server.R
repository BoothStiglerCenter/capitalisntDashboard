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


#### DATA PROCESSING

shinyServer(function(input, output) {
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
            left_join(
                keywords_data %>%
                    group_by(episode_id) %>%
                    summarise(
                        keywords_concat = paste(keyword, collapse = ", ")
                    ),
                by = "episode_id"
            ) %>%
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
                ),
                hidden_keyword_col = ""
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
                is_isnt,
                hidden_keyword_col,
                keywords_concat
            ) %>%
            mutate(
                completion_bullet_range = list(c(is_isnt, avg_completion, 1))
            ) %>%
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
                    hidden_keyword_col = colDef(
                        name = "Keywords/Tags",
                        details = JS("
                            function(rowInfo) {
                                return rowInfo.values['keywords_concat']
                            }
                        ")
                    ),
                    keywords_concat = colDef(
                        show = FALSE,
                    ),
                    avg_completion = colDef(show = FALSE),
                    is_isnt = colDef(show = FALSE),
                    completion_bullet_range = colDef(
                        name = "Completion rate:",
                        # See https://omnipotent.net/jquery.sparkline/#tooltips for docs
                        # For the time being tooltips have been turned off.
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

    ################################################
    #### GEOGRAPHY/MAPS TAB #####
    ################################################
    output$globalListeners <- renderEcharts4r({
        podcast_locations_data %>%
            e_charts(country.name.en) %>%
            e_map(downloads_total) %>%
            e_visual_map(downloads_total) %>%
            e_datazoom()


    })




    ################################################
    #### PLATFORMS TAB #####
    ################################################

    output$platformShareBar <- renderEcharts4r({
        pod_platforms_data %>%
            e_chart(x = name) %>%
            e_bar(serie = downloads_total, colorBy ='series') %>%
            e_legend(show = FALSE) %>%
            e_show_loading() %>%
            e_tooltip() %>%
            e_add("itemStyle", color)
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
                stack = "stack", name = "Apple Podcasts") %>%
            e_bar(serie = `Spotify`,
                stack = "stack", name = "Spotify") %>%
            e_bar(serie = `Overcast`,
                stack = "stack", name = "Overcast") %>%
            e_bar(serie = `Podcast & Radio Addict`,
                stack = "stack", name = "Podcast & Radio Addict") %>%
            e_bar(serie = `Simplecast`,
                stack = "stack", name = "Simplecast") %>%
            e_bar(serie = `Pocket Casts`,
                stack = "stack", name = "Pocket Casts") %>%
            e_bar(serie = `Google Podcasts`,
                stack = "stack", name = "Google Podcasts") %>%
            e_bar(serie = `Other`,
                stack = "stack", name = "Other") %>%
            e_legend(show = TRUE) %>%
            e_datazoom() %>%
            e_show_loading() %>%
            e_tooltip() %>%
            e_color(discrete_palette)
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



    ################################################
    #### ABOUT/HOME TAB #####
    ################################################


    output$calendarPlot <- renderEcharts4r({
        downloads_data %>%
            mutate(year = format(interval, '%Y')) %>%
            filter(year %in% c("2020", "2021", "2022", "2023")) %>%
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
            e_calendar(range = "2023", top = "580") %>%
            e_heatmap(total_daily_downloads, coord_system = "calendar") %>%
            e_visual_map(total_daily_downloads, calculable = TRUE) %>%
            e_tooltip(trigger = "item")
    })

    calendarDateClicked <- reactive({

        # IF: no date has yet been clicked
        if (is.null(input$calendarPlot_clicked_data)){
            today <- today()
            if (wday(today) -1 == 4) {
                # Today is already a thursday (wday() weekday is adjusted by 1)
                today
            } else {
                # Round down to the  most recent Thursday
                floor_date(today, "week", 4)
            }
        # ELSE: Return whatever date was clicked
        } else {
            str_match(
                input$calendarPlot_clicked_data[1],
                '"(\\d{4}-\\d{2}-\\d{2})"'
            )[2]
        }
    })

    output$calendarTableTitle <- renderText({
        date <- calendarDateClicked()
        paste("<h2>Top Daily Episodes: ", date, "</h2>", sep="")
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
                ),
                defaultPageSize = 10
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
            rename("release_date" = "Release Date")) +
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
                aes(
                    x = avg_completion,
                    y = release_date,
                    shape = "circle",
                    color = "orange"
                ),
                size = 4) +
            geom_point(
                aes(
                    x = is_isnt,
                    y = release_date,
                    shape = "square",
                    color = "red"
                ),
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
            scale_color_manual(
                breaks = c("orange", "red"),
                values = c("orange", "red"),
                labels = c("Avg. Completion", "Is/Isn't Begins")
            ) +
            scale_shape_manual(
                breaks = c("circle", "square"),
                values = c(16, 15),
            ) +
            guides(
                color = guide_legend(
                    title = element_blank(),
                    override.aes = list(shape = c(16, 15))),
                shape = "none"
            ) + 
            scale_x_continuous(
                position = "top",
                limits = c(-0.25, 1),
                breaks = seq(0, 1, by = 0.5)
            ) +
            xlab("Completion rate:") +
            theme_minimal() +
            theme(
                axis.title.x.top = element_text(size = 18, face = "bold"),
                axis.text.x.top = element_text(size = 16),
                panel.border = element_blank(),
                legend.position = "top",
                # legend.title = element_text(size = 18, face = "bold"),
                legend.text = element_text(size = 16)
            )

    })




})
