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
    theme=shinytheme('simplex'),
    
    # Application title
    titlePanel("Capitalisn't: Podcast performance"),
    
    
    # Primary page/tabs layout structure
    tabsetPanel(
        tabPanel(
            title='About',
            includeMarkdown('about_panel.md'),
            tags$h1(getwd())
        ),
        tabPanel(
            title = "Downloads",
            fluidRow(
                column(9,
                    echarts4rOutput(
                        "downloadsPlot",
                        width = "100%", 
                        height = "600px"
                    )
                ),
                column(3,
                    selectInput(
                        "episodeSelectize",
                        "Select Episodes:",
                        choices = episode_titles,
                        selected = default_selection,
                        multiple = TRUE
                    )
                )
            ),
            reactableOutput(
                "episodeDownloadsTable"
            ),
            verbatimTextOutput(
                "selectedEpisodesVerbatim"
            )
        ),
        tabPanel(
            title='Platforms',
            echarts4rOutput(
                "platformShareBar"
            ),
            echarts4rOutput(
                "episodePlatforms"
            )
            ),
        tabPanel(
            title='Other'
        )
    )
    
    
))
