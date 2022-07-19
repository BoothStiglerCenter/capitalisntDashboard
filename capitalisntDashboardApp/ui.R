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
            title='Downloads',
            plotOutput(
                "downloadsPlot"
            )
        ),
        tabPanel(
            title='Platforms',
            ),
        tabPanel(
            title='Other'
        )
    )
    
    
))
