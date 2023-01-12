#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(reactable)
library(ggiraph)
library(scales)
library(reactable)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme('simplex'),
    
    # Application title
    titlePanel("Capitalisn't: Podcast performance"),

    # Primary page/tabs layout structure
    tabsetPanel(
        tabPanel(
            title = "About",
            fluidRow(
                column(6,
                    includeMarkdown("about_panel.md"),
                ),
                column(6,
                ),
            ),
            hr(),
            fluidRow(
                column(8,
                    tags$h2("Podcast Daily Downloads:"),
                    echarts4rOutput(
                        "calendarPlot",
                        width = "100%",
                        height = "800px"
                    ),
                ),
                column(4,
                    htmlOutput(
                        "calendarTableTitle"
                    ),
                    reactableOutput(
                        "calendarDateTopEps"
                    )
                )
            )
        ),
        # Downloads Tab
        tabPanel(
            title = "Downloads",
            tags$h1("Cumulative Daily Downloads"),
            fluidRow(
                column(9,
                    echarts4rOutput(
                        "downloadsPlot",
                        width = "100%",
                        height = "600px"
                    )
                ),
                column(3,
                    tags$h3("Select Episodes:"),
                    tags$p(downloads_select_explainer),
                    selectInput(
                        "episodeSelectize",
                        width = "100%",
                        label = "",
                        choices = episode_titles,
                        selected = default_selection,
                        multiple = TRUE
                    )
                )
            ),
            tags$h2("Episode Details: "),
            tags$p("This table contains details for episodes selected above. In the 'Completion Rate' column, the red marker indicates the begining of the Capitalis/isn't section. The dark blue bar indicates average completion rate."),
            reactableOutput(
                "episodeDownloadsTable"
            )
        ),
        tabPanel(
            title = "Geography",
            echarts4rOutput(
                "globalListeners"
            )
        ),
        tabPanel(
            title = "Platforms",
            tags$p("Please be patient. The plot creation tool handles a large .csv file."),
            tags$h2("Podcast -- Most Popular Listening Platforms"),
            echarts4rOutput(
                "platformShareBar"
            ),
            tags$h2("Episode -- Most Popular Listening Platforms"),
            tags$p("Clicking on a platform in the legend allows for de/selection of specified platform(s)."),
            echarts4rOutput(
                "episodePlatforms"
            ),
            tags$em(platforms_caveat_text)
            ),
        tabPanel(
            title = "Completion",
            plotOutput(
                "completionRatePlot",
                width = "100%",
                height = "3000px"
            )
        )
    )
    
    
))
