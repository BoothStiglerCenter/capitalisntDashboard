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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    #### DATA PROCESSING
    downloads_data <- read_csv('../episodes_downloads-2022-07-19.csv') %>%
        mutate(interval = as_date(interval, format='%Y-%m-%d'))%>%
        arrange(interval) %>%
        group_by(episode_id) %>%
        mutate(days_since_release=row_number(),
               cumulative_downloads = cumsum(downloads_total),
               release_date = min(interval),
               most_recent_date = max(interval),
               label_text = ifelse(interval == most_recent_date,
                                   paste(release_date, '--', substr(title, 1, 35)),
                                   NA),
               label_text = ifelse(str_length(label_text) > 45,
                                   paste0(label_text, '...', sep=''),
                                   label_text)
               ) 
    
    ten_recent_release_dates <- downloads_data %>%
        ungroup() %>%
        distinct(release_date, .keep_all =TRUE) %>%
        slice_max(release_date, n=10) %>%
        select(release_date) %>%
        pull()
    
    output$downloadsPlot <- renderPlot({
        ggplot(downloads_data %>%
                   ungroup()%>%
                   filter(release_date %in% ten_recent_release_dates) %>%
                   view()
                   )+
            geom_line(aes(x=days_since_release, y=cumulative_downloads, color=title, group=title),
                      size=2)+
            geom_label_repel(aes(x=days_since_release, y=cumulative_downloads, color=title, label=label_text),
                             nudge_x = 4,
                             xlim=c(0, 50),
                             ylim=c(0, 2000000),
                             direction='both')+
            scale_color_viridis_d(breaks = c(as.numeric(min(downloads_data$release_date)),
                                             as.numeric(max(downloads_data$release_date))),
                                  labels = c('Oldest', 'Newest'),
                                  direction=-1,
                                  name='Release date:')+
            theme_minimal()
    })

})
