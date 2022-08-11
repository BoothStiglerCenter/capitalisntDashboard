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
            title='About',
            includeMarkdown('about_panel.md'),
            tags$h1(getwd()),
            hr(),
            fluidRow(
                column(8,
                    echarts4rOutput(
                        "calendarPlot",
                        width = "100%",
                        height = "600px"
                    ),
                ),
                column(4,
                    reactableOutput(
                        "calendarDateTopEps"
                    )
                )
            )
        ),
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
            title='Platforms',
            tags$h2("Podcast -- Most Popular Listening Platforms"),
            echarts4rOutput(
                "platformShareBar"
            ),
            tags$h2("Episode -- Most Popular Listening Platforms"),
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
        ),
        tabPanel(
            title='Other',
            selectInput(
                "species_select",
                "Select Species: ",
                choices = unique(iris$Species),
                selected = c("setosa", "virginica"),
                multiple = TRUE
            )
        ),
    )
    
    
))
